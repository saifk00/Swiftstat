package org.swiftstat.pgm.compiler.entity

import org.scalatest.flatspec.AnyFlatSpec
import org.swiftstat.mdl._
import org.swiftstat.pgm.compiler.entity.ast._

class ConstructUserNodesSpec extends AnyFlatSpec {
    behavior of "ConstructUserNodes"

    it should "construct a simple set of nodes" in {
        val parentMap = Map[String, Seq[String]](
            "A" -> Seq(),
            "B" -> Seq(),
            "C" -> Seq(),
            "D" -> Seq("A", "B"),
            "E" -> Seq("C", "D"),
        )
        val valueMap = Map[String, Seq[Double]](
            "A" -> Seq(0D, 1),
            "B" -> Seq(2D, 3),
            "C" -> Seq(4D, 5),
            "D" -> Seq(6D, 7),
            "E" -> Seq(8D, 9)
        )
        val distMap = Map[String, Seq[Double]](
            "A" -> Seq(0.5, 0.5),
            "B" -> Seq(0.3, 0.7),
            "C" -> Seq(0.1, 0.9),
            "D" -> Seq(0.2, 0.8,
                       0.1, 0.9,
                       0.4, 0.6,
                       0.3, 0.7),
            "E" -> Seq(0.1, 0.9,
                       0.2, 0.8,
                       0.3, 0.7,
                       0.4, 0.6)
        )

        val a = UserNodeEntity("A", Seq(), Seq(0D, 1), Seq(0.5, 0.5))
        val b = UserNodeEntity("B", Seq(), Seq(2D, 3), Seq(0.3, 0.7))
        val c = UserNodeEntity("C", Seq(), Seq(4D, 5), Seq(0.1, 0.9))
        val d = UserNodeEntity("D", Seq(a, b), Seq(6D, 7), Seq(0.2, 0.8,
                                                        0.1, 0.9,
                                                        0.4, 0.6,
                                                        0.3, 0.7))
        val e = UserNodeEntity("E", Seq(c, d), Seq(8D, 9), Seq(0.1, 0.9,
                                                        0.2, 0.8,
                                                        0.3, 0.7,
                                                        0.4, 0.6))

        val expected = Set(a, b, c, d, e)
        val actual = MDLEntity.constructUserNodes(parentMap, valueMap, distMap)
        assert(actual == expected)
    }
}

class ConstructQueriesSpec extends AnyFlatSpec {
    behavior of "ConstructQueries"

    it should "construct a simple set of queries" in {
        // A > B | C
        val queryProto = QueryProto()
            .withMarginalQuery(
                MarginalQuery()
                .addUnobserved(RelationQuery()
                        .withLexpr(QueryExpression()
                            .withNode("A")
                            .withType(QueryExpression.Type.NODE))
                        .withRexpr(QueryExpression()
                            .withNode("B")
                            .withType(QueryExpression.Type.NODE))
                        .withRelation(RelationQuery.Type.GREATER_THAN)))
            .addConditions(
                Condition()
                .withNode("C"))
            .withEpsilon(0.9)
            .withDelta(0.1)
            .withName("Q0")
        val a = UserNodeEntity("A", Seq(), Seq(0D, 1), Seq(0.5, 0.5))
        val b = UserNodeEntity("B", Seq(), Seq(2D, 3), Seq(0.3, 0.7))
        val c = UserNodeEntity("C", Seq(), Seq(4D, 5), Seq(0.1, 0.9))
        val nodes = Set(a, b, c)

        val expected =
            QueryEntity(
                "Q0",
                0.9,
                0.1,
                QueryExpressionList(Seq(
                    QueryExpressionBinaryOp(
                        QueryExpressionNodeOp(a),
                        QueryExpressionNodeOp(b),
                        QueryBinaryOpTypes.`>`)
                )),
                Set(RunTimeEvidenceEntity(c)),
                QueryTypes.Marginal
            )

        val actual = MDLEntity.constructQueries(Seq(queryProto), nodes)
        assert(actual.size == 1)
        assert(actual.head == expected)
    }
}

class NetworkTypeSpec extends AnyFlatSpec {
    behavior of "NetworkType"

    it should "be bayesian" in {
        val proto = PGMType.BAYESIAN
        val expected = NetworkTypes.Bayesian
        val actual = NetworkTypes.fromPGMType(proto)
        assert(actual == expected)
    }

    it should "be markov" in {
        val proto = PGMType.MARKOV
        val expected = NetworkTypes.Markov
        val actual = NetworkTypes.fromPGMType(proto)
        assert(actual == expected)
    }
}
