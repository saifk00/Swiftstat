package org.swiftstat.pgm.visitors

import org.swiftstat.pgm.antlr.PGMFileBaseVisitor

import org.swiftstat.mdl._
import org.swiftstat.pgm.antlr.PGMFileParser.PgmContext
import org.antlr.v4.runtime.tree.ParseTreeVisitor
import org.swiftstat.pgm.antlr.PGMFileParser.BayesianPGMTypeContext
import org.swiftstat.pgm.antlr.PGMFileParser.MarkovPGMTypeContext
import org.swiftstat.pgm.antlr.PGMFileParser.ConnectionContext
import org.swiftstat.pgm.antlr.PGMFileParser.ParentListContext
import org.swiftstat.pgm.antlr.PGMFileParser.NodeDefinitionContext
import org.swiftstat.pgm.antlr.PGMFileParser.ConnectionToContext
import org.swiftstat.pgm.antlr.PGMFileParser.EvidenceAtRuntimeContext
import org.swiftstat.pgm.antlr.PGMFileParser.EvidenceAtCompileTimeContext
import org.swiftstat.pgm.antlr.PGMFileParser.PgmTypeContext
import org.swiftstat.pgm.antlr.PGMFileParser
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.tree.RuleNode
import org.antlr.v4.runtime.tree.TerminalNode
import org.antlr.v4.runtime.tree.ErrorNode
import scala.collection.JavaConverters
import scala.jdk.javaapi.CollectionConverters
import javax.management.QueryExp

/**
  * Any visitor which can visit the parse tree and return a single value of type T
  *
  * NB: sealed here means that all the subclasses of this trait must be defined in this file.
  *
  */
sealed trait ValueProducingVisitor[T, E] extends PGMFileBaseVisitor[E] {
    def getValue(): T
    def parserRule(): PGMFileParser => ParserRuleContext
}

/**
 * Allows applying a visitor to a parse tree.
 */
object ApplyMDLVisitor {
    def apply[T, E](tree: PgmContext, valueVisitor: ValueProducingVisitor[T, E]) = {
        tree.accept(valueVisitor)
        valueVisitor.getValue()
    }
}

object MDLVisitor {
    /**
     * Returns the MDL of a PGM.
     */
    def apply(tree: PgmContext) = {
        val mdlVisitor = new MDLVisitor()
        tree.accept(mdlVisitor)
        mdlVisitor.getValue()
    }
}

/**
  * Visitor to construct an MDL Protobuf from a PGM parse tree
  */
class MDLVisitor extends ValueProducingVisitor[PGMProto, Unit] {
    override def parserRule() = _.pgm()

    private var pgmProto = PGMProto()
    def getValue() = pgmProto

    private def updateType(pgmType: PGMType) = {
        pgmProto = pgmProto.withType(pgmType)
    }

    override def visitHeader(ctx: PGMFileParser.HeaderContext): Unit = {
        val typeVisitor = new MDLTypeVisitor()
        ctx.pgmType().accept(typeVisitor)

        updateType(typeVisitor.getValue())

        pgmProto = pgmProto.withNetworkName( ctx.pgmName().getText())
    }

    override def visitSecNodeDefinitions(ctx: PGMFileParser.SecNodeDefinitionsContext): Unit = {
        val nodeVisitor = new MDLUserNodesVisitor()
        ctx.nodeDefinitions().accept(nodeVisitor)

        pgmProto = pgmProto.withNodes(nodeVisitor.getValue().toSeq)
    }

    override def visitSecConnections(ctx: PGMFileParser.SecConnectionsContext): Unit = {
        val edgeVisitor = new MDLEdgesVisitor()
        ctx.connections().accept(edgeVisitor)

        pgmProto = pgmProto.withEdges(edgeVisitor.getValue().toSeq)
    }

    override def visitSecQueries(ctx: PGMFileParser.SecQueriesContext): Unit = {
        val queryVisitor = new MDLQueriesVisitor()
        ctx.queries().accept(queryVisitor)

        pgmProto = pgmProto.withQueries(queryVisitor.getValue().toSeq)
    }
}

class MDLTypeVisitor extends ValueProducingVisitor[PGMType, Unit] {
    override def parserRule() = _.pgmType()

    private var pgmType: Option[PGMType] = None

    override def visitBayesianPGMType(ctx: BayesianPGMTypeContext): Unit = {
        pgmType = Some(PGMType.BAYESIAN)
    }

    override def visitMarkovPGMType(ctx: MarkovPGMTypeContext): Unit = {
        pgmType = Some(PGMType.MARKOV)
    }

    def getValue() = pgmType.getOrElse(throw new Exception("No value"))
}

class MDLUserNodesVisitor extends ValueProducingVisitor[Set[UserNode], Unit] {
    override def parserRule() = _.nodeDefinitions()

    private var userNodes: Set[UserNode] = Set()

    override def visitNodeDefinition(ctx: NodeDefinitionContext): Unit = {
        val name = ctx.node().getText()

        /**
          * Note the problem here: we need to convert from ANTLR java to Scala to process stuff,
          * then convert back to java to construct protobuf stuff.
          *
          * This may be a significant problem
          */
        val parentList = if (ctx.parentList() != null) CollectionConverters.asScala(ctx.parentList().node()) else List()
        val valuesList = CollectionConverters.asScala(ctx.valuesList().nodeValue())
        val cpdRowsList = CollectionConverters.asScala(ctx.cpd().cpdRow())

        val parents = parentList.map(_.getText)
        val values = valuesList.map(_.getText.toDouble)
        val distribution = cpdRowsList.flatMap(row => {
            val rowValuesList = CollectionConverters.asScala(row.NUMBER())
            val rowValues = rowValuesList.map(_.getText.toDouble)

            rowValues
        })

        val cpdProto = CPDProto()
            .withParentOrder(parents.toSeq)
            .withValues(values.toSeq)
            .withDistribution(distribution.toSeq)

        val node = UserNode()
            .withName(name)
            .withCpd(cpdProto)

        userNodes += node
    }

    def getValue() = userNodes
}

class MDLEdgesVisitor extends ValueProducingVisitor[Set[EdgeProto], Unit] {
    override def parserRule() = _.connections()

    private var edges: Set[EdgeProto] = Set()
    private var parent: String = ""

    override def visitConnection(ctx: ConnectionContext): Unit = {
        parent = ctx.node().getText()

        ctx.connectionTo().forEach(_.accept(this))
    }

    override def visitConnectionTo(ctx: ConnectionToContext): Unit = {
        val child = ctx.node().getText()

        val edge = EdgeProto()
            .withFromNode(parent)
            .withToNode(child)

        edges += edge
    }

    def getValue() = edges
}

class MDLQueriesVisitor extends ValueProducingVisitor[Set[QueryProto], Unit] {
    override def parserRule() = _.queries()

    private var queries: Set[QueryProto] = Set()

    override def visitMarginalQuery(ctx: PGMFileParser.MarginalQueryContext): Unit = {
        val queryStatementsList = CollectionConverters.asScala(
                ctx.queryStatementList()
                    .queryStatement())

        val unobserved = queryStatementsList.map(_.accept(new MDLRelationQueryVisitor))
        var conditions = ctx.evidenceList().accept(new MDLEvidenceListVisitor)
        val epsilon = ctx.epsilonDeltaSpecification().epsilonSpecifier().NUMBER().getText.toDouble
        val delta = ctx.epsilonDeltaSpecification().deltaSpecifier().NUMBER().getText.toDouble
        val queryName = ctx.queryName().IDENTIFIER().getText

        val query = QueryProto()
            .withMarginalQuery(
                MarginalQuery()
                    .withUnobserved(unobserved.toSeq))
            .withConditions(conditions)
            .withEpsilon(epsilon)
            .withDelta(delta)
            .withName(queryName)

        queries += query
    }

    override def visitExpectationQuery(ctx: PGMFileParser.ExpectationQueryContext): Unit = {
        val nodeFunction = ctx.queryStatement().accept(new MDLQueryExpressionVisitor)
        var conditions =
            if (ctx.evidenceList() != null)
                ctx.evidenceList().accept(new MDLEvidenceListVisitor)
            else
                Seq[Condition]()
        val epsilon = ctx.epsilonDeltaSpecification().epsilonSpecifier().NUMBER().getText.toDouble
        val delta = ctx.epsilonDeltaSpecification().deltaSpecifier().NUMBER().getText.toDouble
        val queryName = ctx.queryName().IDENTIFIER().getText

        val query = QueryProto()
            .withExpectationQuery(
                ExpectationQuery()
                    .withNodeFunction(nodeFunction))
            .withConditions(conditions)
            .withEpsilon(epsilon)
            .withDelta(delta)
            .withName(queryName)

        queries += query
    }

    override def visitMapQuery(ctx: PGMFileParser.MapQueryContext): Unit = {
        val nodeFunction = ctx.queryStatement().accept(new MDLQueryExpressionVisitor)
        val conditions = ctx.evidenceList().accept(new MDLEvidenceListVisitor)
        val epsilon = ctx.epsilonDeltaSpecification().epsilonSpecifier().NUMBER().getText.toDouble
        val delta = ctx.epsilonDeltaSpecification().deltaSpecifier().NUMBER().getText.toDouble
        val queryName = ctx.queryName().IDENTIFIER().getText

        val query = QueryProto()
            .withMapQuery(
                MAPQuery()
                    .withNodeFunction(nodeFunction))
            .withConditions(conditions.toSeq)
            .withEpsilon(epsilon)
            .withDelta(delta)
            .withName(queryName)

        queries += query
    }

    def getValue() = queries
}

class MDLEvidenceListVisitor extends ValueProducingVisitor[Seq[Condition], Seq[Condition]] {
    override def parserRule() = _.evidenceList()
    private var evidence: Seq[Condition] = Seq()
    def getValue() = evidence

    override def visitEvidenceAtRuntime(ctx: EvidenceAtRuntimeContext) = {
        val node = ctx.node().getText()

        val condition = Condition()
            .withNode(node)


        evidence = evidence :+ condition
        evidence
    }

    override def visitEvidenceAtCompileTime(ctx: EvidenceAtCompileTimeContext) = {
        val node = ctx.node().getText()
        val value = ctx.nodeValue().getText().toDouble

        val condition = Condition()
            .withNode(node)
            .withValue(value)


        evidence = evidence :+ condition
        evidence
    }
}

class MDLRelationQueryVisitor extends ValueProducingVisitor[RelationQuery, RelationQuery] {
    override def parserRule() = _.relationQueryExpression()
    private var query: Option[RelationQuery] = None
    def getValue() = query.getOrElse(throw new Exception("No value"))

    def buildQuery(l: QueryExpression, r: QueryExpression, op: RelationQuery.Type) = {
        val query = RelationQuery()
            .withLexpr(l)
            .withRexpr(r)
            .withRelation(op)


        this.query = Some(query)
        query
    }

    override def visitRqeEquals(ctx: PGMFileParser.RqeEqualsContext) = {
        val left = ctx.queryExpression(0).accept(new MDLQueryExpressionVisitor())
        val right = ctx.queryExpression(1).accept(new MDLQueryExpressionVisitor())

        buildQuery(left, right, RelationQuery.Type.EQUAL)
    }

    override def visitRqeLessThanEquals(ctx: PGMFileParser.RqeLessThanEqualsContext) = {
        val left = ctx.queryExpression(0).accept(new MDLQueryExpressionVisitor())
        val right = ctx.queryExpression(1).accept(new MDLQueryExpressionVisitor())

        buildQuery(left, right, RelationQuery.Type.LESS_THAN_OR_EQUAL)
    }

    override def visitRqeGreaterThanEquals(ctx: PGMFileParser.RqeGreaterThanEqualsContext) = {
        val left = ctx.queryExpression(0).accept(new MDLQueryExpressionVisitor())
        val right = ctx.queryExpression(1).accept(new MDLQueryExpressionVisitor())

        buildQuery(left, right, RelationQuery.Type.GREATER_THAN_OR_EQUAL)
    }

    override def visitRqeGreaterThan(ctx: PGMFileParser.RqeGreaterThanContext) = {
        val left = ctx.queryExpression(0).accept(new MDLQueryExpressionVisitor())
        val right = ctx.queryExpression(1).accept(new MDLQueryExpressionVisitor())

        buildQuery(left, right, RelationQuery.Type.GREATER_THAN)
    }

}

class MDLQueryExpressionVisitor extends ValueProducingVisitor[QueryExpression, QueryExpression] {
    override def parserRule() = _.queryExpression();
    private var query: Option[QueryExpression] = None
    def getValue() = query.getOrElse(throw new Exception("No value"))

    override def visitQeNode(ctx: PGMFileParser.QeNodeContext): QueryExpression = {
        val node = ctx.node().getText()

        val query = QueryExpression()
            .withNode(node)
            .withType(QueryExpression.Type.NODE)

        this.query = Some(query)
        query
    }

    override def visitQeValue(ctx: PGMFileParser.QeValueContext): QueryExpression = {
        val value = ctx.NUMBER().getText().toDouble

        val query = QueryExpression()
            .withValue(value)
            .withType(QueryExpression.Type.CONST)

        this.query = Some(query)
        query
    }

    /*
    * TODO(skhattak) surely there is a better way to do this without all the duplication.
    *                Some way to create the method to visit both children and then set the appropriate type
    *                the problem is there is no common base class of the cases that are just QE OP QE
    */

    def buildOp(l: QueryExpression, r: QueryExpression, op: QueryExpression.Type) = {
        val query = QueryExpression()
            .withLexpr(l)
            .withRexpr(r)
            .withType(op)


        this.query = Some(query)
        query
    }

    override def visitQePlus(ctx: PGMFileParser.QePlusContext): QueryExpression = {
        val left = ctx.queryExpression(0).accept(this)
        val right = ctx.queryExpression(1).accept(this)

        buildOp(left, right, QueryExpression.Type.PLUS)
    }

    override def visitQeMinus(ctx: PGMFileParser.QeMinusContext): QueryExpression = {
        val left = ctx.queryExpression(0).accept(this)
        val right = ctx.queryExpression(1).accept(this)

        buildOp(left, right, QueryExpression.Type.MINUS)
    }

    override def visitQeMultiply(ctx: PGMFileParser.QeMultiplyContext): QueryExpression = {
        val left = ctx.queryExpression(0).accept(this)
        val right = ctx.queryExpression(1).accept(this)

        buildOp(left, right, QueryExpression.Type.MULTIPLY)
    }

    override def visitQeDivide(ctx: PGMFileParser.QeDivideContext): QueryExpression = {
        val left = ctx.queryExpression(0).accept(this)
        val right = ctx.queryExpression(1).accept(this)

        buildOp(left, right, QueryExpression.Type.DIVIDE)
    }

    override def visitQePow(ctx: PGMFileParser.QePowContext): QueryExpression = {
        val left = ctx.queryExpression(0).accept(this)
        val right = ctx.queryExpression(1).accept(this)

        buildOp(left, right, QueryExpression.Type.POW)
    }
}
