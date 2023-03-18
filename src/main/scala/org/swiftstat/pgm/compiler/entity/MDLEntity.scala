package org.swiftstat.pgm.compiler.entity

import org.swiftstat.mdl.PGMProto
import org.swiftstat.mdl.PGMType
import org.swiftstat.mdl.CPDProto
import org.swiftstat.pgm.compiler.entity.CPDEntity
import scala.annotation.varargs
import org.swiftstat.mdl.QueryProto
import org.swiftstat.pgm.compiler.entity.ast.QueryExpressionASTNode
import org.swiftstat.util
import org.swiftstat.mdl.UserNode
import com.typesafe.scalalogging.Logger

object NetworkTypes extends Enumeration {
  type NetworkType = Value
  val Bayesian, Markov = Value

  def fromPGMType(networkType: PGMType): NetworkType = {
    networkType match {
      case PGMType.BAYESIAN => Bayesian
      case PGMType.MARKOV => Markov
      case _ => throw new Exception("Unknown network type")
    }
  }
}

/**
  * Our internal representation of an MDL.
  *
  * Serves as what we used to call the 'netlist'. I.e. it fully represents the network
  * and provides an interface to the RTL generation / hardware construction phase of the
  * compiler.
  */
final case class MDLEntity private (
    name: String=  "",
    networkType: NetworkTypes.NetworkType = NetworkTypes.Bayesian,
    userNodes: Set[UserNodeEntity] = Set.empty,
    queries: Set[QueryEntity] = Set.empty) {

    def evidenceNodes: Set[UserNodeEntity] = queries.flatMap(_.evidenceNodes)
    def runTimeEvidenceNodes: Set[UserNodeEntity] = queries.flatMap(_.runTimeEvidenceNodes)
}

object MDLEntity {
    val logger = Logger[MDLEntity]

    /**
      * Constructs a set of user nodes given some info
      *
      *
      * @param parentMap a map from node name to its parents
      * @param valueMap a map from node name to its values
      * @param distributionMap a map from node name to its probability
      * @return a set of user nodes
      */
    def constructUserNodes(
      parentMap: Map[String, Seq[String]],
      valueMap: Map[String, Seq[Double]],
      distributionMap: Map[String, Seq[Double]]): Set[UserNodeEntity] = {

      /* description of the algorithm:

        essentially, we have to construct UserNodes for parents before children, since
        the constructor of CPD relies on parent UserNodes. So we sort the nodes in
        *reverse* topological order (remember, parents point to children) and then
        construct UserNodes by looking up parent UserNodes in a map
      */
      logger.debug(s"Performing topological sort on ${parentMap.size} nodes")
      val sortedNodeNames = util.sortReverseTopological(parentMap, valueMap.keys.toSet)
      logger.debug(s"Performed topological sort on ${parentMap.size} nodes")

      var allUserNodesMap = scala.collection.mutable.Map[String, UserNodeEntity]()

      logger.debug("Recursively constructing nodes")
      val allUserNodes = sortedNodeNames.map(node => {
        logger.debug(s"Constructing node ${node}")
        val parentNodes = parentMap(node).map(allUserNodesMap)
        val values = valueMap(node)
        val distribution = distributionMap(node)
        val usernode = UserNodeEntity(node, parentNodes, values, distribution)
        allUserNodesMap += (node -> usernode)

        logger.debug(s"Constructed node ${node}")
        usernode
      })

      logger.debug(s"Recursively constructed ${allUserNodes.size} nodes")

      allUserNodes.toSet
    }

    def constructQueries(queryProtos: Seq[QueryProto], userNodes: Set[UserNodeEntity]): Set[QueryEntity] = {
      val astBuilder = new QueryExpressionASTBuilder(userNodes)
      val queries = queryProtos.map(queryProto => {
        logger.debug(s"Parsing query ${queryProto.name}")
        val (querytype: QueryTypes.QueryType, tree: QueryExpressionASTNode) = queryProto.innerQuery match {
          case QueryProto.InnerQuery.MarginalQuery(mq) => (QueryTypes.Marginal, astBuilder.parseProto(mq.unobserved))
          case QueryProto.InnerQuery.ExpectationQuery(eq) => (QueryTypes.Expectation, astBuilder.parseProto(eq.nodeFunction.get))
          case QueryProto.InnerQuery.MapQuery(mq) => (QueryTypes.MaximumAPosteriori, astBuilder.parseProto(mq.nodeFunction.get))
          case _ => throw new Exception("Unknown query type")
        }

        val evidence = queryProto.conditions.map(c => {
          val node = userNodes.find(_.name == c.node).get
          c.value match {
            case Some(value) => CompileTimeEvidenceEntity(node, node.values.find(_.value == value).get)
            case None => RunTimeEvidenceEntity(node)
          }
        }).toSet[EvidenceEntity]

        logger.debug(s"Parsed query ${queryProto.name}")
        QueryEntity(queryProto.name, queryProto.epsilon, queryProto.delta, tree, evidence, querytype)
      })

      queries.toSet
    }

    /**
      * Performs validation on a proto
      * Checks:
      *   1. Name uniqueness
      *
      */
    def validate(proto: PGMProto): Unit = {
      // validate name uniqueness
      val names = scala.collection.mutable.Set[String]()
      proto.nodes.foreach(node => {
        assert(!names.contains(node.name), s"Duplicate node name: ${node.name}")
        names.add(node.name)
      })
    }

    /**
      * Construct an MDL from a proto specification
      *
      * @param name the name of the network
      * @param proto the proto specification
      * @return
      */
    def fromProto(proto: PGMProto): MDLEntity = {
      validate(proto)

      logger.debug("Constructing MDLEntity from Protobuf MDL")
      val networkType = NetworkTypes.fromPGMType(proto.`type`)
      val nodeNameToValuesSeq = proto.nodes.map(a => a.name -> a.cpd.get.values).toMap
      val nodeNameToParentOrderSeq = proto.nodes.map(a => a.name -> a.cpd.get.parentOrder).toMap
      val nodeNameToDistributionSeq = proto.nodes.map(a => a.name -> a.cpd.get.distribution).toMap

      logger.debug("Constructing user nodes")
      val userNodes = constructUserNodes(nodeNameToParentOrderSeq, nodeNameToValuesSeq, nodeNameToDistributionSeq)
      logger.debug("Constructed user nodes")

      logger.debug("Constructing queries")
      val queries = constructQueries(proto.queries, userNodes)
      logger.debug("Constructed queries")

      MDLEntity(proto.networkName, networkType, userNodes, queries)
    }

    /**
      * Creates an MDL from a set of user nodes which have already been 'wired up' correctly.
      * normally just used for testing things
      *
      * @param userNodes the set of user nodes
      * @return the MDL
      */
    def fromUserNodes(userNodes: UserNodeEntity*): MDLEntity = {
      MDLEntity("MDL_DEFAULT", NetworkTypes.Bayesian, userNodes.toSet, Set.empty)
    }
}

