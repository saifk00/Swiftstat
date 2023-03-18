package org.swiftstat.pgm.visitors

import org.swiftstat.mdl._
import org.scalatest.flatspec.AnyFlatSpec
import org.antlr.v4.runtime.ANTLRInputStream
import org.swiftstat.pgm.antlr.PGMFileLexer
import org.antlr.v4.runtime.CommonTokenStream
import org.swiftstat.pgm.antlr.PGMFileParser
import org.antlr.v4.runtime.tree.ParseTreeVisitor
import org.antlr.v4.runtime.ParserRuleContext
import scala.jdk.javaapi.CollectionConverters

object TestData {
    object UserNodes {
        val nodeA = UserNode()
                .withName("A")
                .withCpd(CPDProto()
                    .addParentOrder("B")
                    .addValues(0,1)
                    .addDistribution(0.9, 0.1, 0.1, 0.9))
        val nodeB = UserNode()
                .withName("B")
                .withCpd(CPDProto()
                    .addParentOrder("C", "D")
                    .addValues(2, 3)
                    .addDistribution(0.9, 0.1))
        val nodeC = UserNode()
                .withName("C")
                .withCpd(CPDProto()
                    .addValues(1,2,3)
                    .addDistribution(0.1, 0.2, 0.7))
        val nodeD = UserNode()
                .withName("D")
                .withCpd(CPDProto()
                    .addValues(52.3, 23)
                    .addDistribution(0.5, 0.5))
    }

    object Edges {
        val edgeBA = EdgeProto()
                .withFromNode("B")
                .withToNode("A")
        val edgeCB = EdgeProto()
                .withFromNode("C")
                .withToNode("B")
        val edgeDB = EdgeProto()
                .withFromNode("D")
                .withToNode("B")
    }

    object QueryExpressions {
        val constA = QueryExpression()
                    .withNode("A")
                    .withType(QueryExpression.Type.NODE)
        val constB = QueryExpression()
                    .withNode("B")
                    .withType(QueryExpression.Type.NODE)
        val addAB = QueryExpression()
                    .withType(QueryExpression.Type.PLUS)
                    .withLexpr(constA)
                    .withRexpr(constB)
    }

    object RelationQueries {
        val AGreaterB = RelationQuery()
                        .withLexpr(QueryExpressions.constA)
                        .withRexpr(QueryExpressions.constB)
                        .withRelation(RelationQuery.Type.GREATER_THAN)
    }

    object Queries {
        val AGreaterBGivenC = QueryProto()
            .withMarginalQuery(
                MarginalQuery()
                .addUnobserved(RelationQueries.AGreaterB))
            .addConditions(
                Condition()
                .withNode("C"))
            .withEpsilon(0.9)
            .withDelta(0.1)
            .withName("Q0")

        val APlusB = QueryProto()
            .withExpectationQuery(
                ExpectationQuery()
                .withNodeFunction(QueryExpressions.addAB)
            )
            .withEpsilon(0.9)
            .withDelta(0.1)
            .withName("Q1")
    }

    object PGMs {
        val pgm1 = PGMProto()
            .withType(PGMType.BAYESIAN)
            .addNodes(UserNodes.nodeA, UserNodes.nodeB, UserNodes.nodeC, UserNodes.nodeD)
            .addEdges(Edges.edgeBA, Edges.edgeCB, Edges.edgeDB)
            .addQueries(Queries.AGreaterBGivenC)
            .withNetworkName("myPGM")
    }
}

class MDLVisitorTest extends ValueProducingVisitorTest[PGMProto, Unit] {
    override def makeVisitor() = new MDLVisitor()

    behavior of "MDLVisitor"
    it should "be able to compile a simple PGM file" in {
        val query =
        """
        myPGM BAYESIAN

        connections:
        B -> A
        C -> B
        D -> B

        nodes:
        A<B> : [0, 1] {
            0.9, 0.1;
            0.1, 0.9
        }

        B<C, D> : [2, 3] {
            0.9, 0.1
        }

        C : [1, 2, 3] {
            0.1, 0.2, 0.7
        }

        D : [52.3, 23.0] {
            0.5, 0.5;
        }

        queries:

        P[A > B | C] #Q0<0.9, 0.1>
        """

        val result = performVisit(query)

        assert(result === TestData.PGMs.pgm1)
    }
}

class MDLTypeVisitorSpec extends ValueProducingVisitorTest[PGMType, Unit] {
    override def makeVisitor() = new MDLTypeVisitor()

    behavior of "TypeVisitor"
    it should "be able to get BAYESIAN" in {
        assert(performVisit("BAYESIAN") == PGMType.BAYESIAN)
    }
    it should "be able to get MARKOV" in {
        assert(performVisit("MARKOV") == PGMType.MARKOV)
    }
    it should "fail" in {
        assertThrows[Exception] {
            performVisit("FICTITIOUS")
        }
    }
}

class MDLUserNodesVisitorSpec extends ValueProducingVisitorTest[Set[UserNode], Unit] {
    override def makeVisitor() = new MDLUserNodesVisitor()

    behavior of "UserNodeVisitor"

    it should "be able to get a single node" in {
        val query = """A<B> : [0, 1] {
                      |     0.9, 0.1;
                      |     0.1, 0.9
                      |}""".stripMargin

        val expected = Set(TestData.UserNodes.nodeA)

        assert(performVisit(query) == expected)
    }

    it should "be able to get two nodes" in {
        val query = """A<B> : [0, 1] {
                      |     0.9, 0.1;
                      |     0.1, 0.9
                      |}
                      |B<C, D> : [2, 3] {
                      |     0.9, 0.1;
                      |}""".stripMargin

        val expected = Set(TestData.UserNodes.nodeA, TestData.UserNodes.nodeB)

        assert(performVisit(query) == expected)
    }

    it should "handle no parents" in {
        val query = """C : [1, 2, 3] {
            0.1, 0.2, 0.7
        }"""

        val expected = Set(TestData.UserNodes.nodeC)
        assert(performVisit(query) == expected)
    }

    it should "handle double values" in {
        val query = """D : [52.3, 23.0] {
            0.5, 0.5;
        }"""

        val expected = Set(TestData.UserNodes.nodeD)
        assert(performVisit(query) == expected)
    }
}

class MDLEdgesVisitorSpec extends ValueProducingVisitorTest[Set[EdgeProto], Unit] {
    override def makeVisitor() = new MDLEdgesVisitor()

    behavior of "EdgesVisitor"

    it should "pick up a single edge" in {
        val result = performVisit("A -> B")
        assert(result.size == 1)
        assert(result.head == EdgeProto()
            .withFromNode("A")
            .withToNode("B"))
    }
    it should "pick up two separate edges" in {
        val result = performVisit("""A -> B
                                    |B -> C""".stripMargin)
        assert(result.size == 2)
        assert(result.head == EdgeProto()
            .withFromNode("A")
            .withToNode("B"))
        assert(result.tail.head == EdgeProto()
            .withFromNode("B")
            .withToNode("C"))
    }
    it should "pick up a reused parent in two edges" in {
        val result = performVisit("""A -> B
                                    |  -> C""".stripMargin)
        assert(result.size == 2)
        assert(result.head == EdgeProto()
            .withFromNode("A")
            .withToNode("B"))
        assert(result.tail.head == EdgeProto()
            .withFromNode("A")
            .withToNode("C"))
    }
}

class MDLQueriesVisitorSpec extends ValueProducingVisitorTest[Set[QueryProto], Unit] {
    override def makeVisitor() = new MDLQueriesVisitor()

    behavior of "QueriesVisitor"

    it should "pick up a marginal query" in {
        val result = performVisit("P[A > B | C] #Q0<0.9, 0.1>")
        assert(result.size == 1)
        assert(result.head == TestData.Queries.AGreaterBGivenC)
    }

    it should "pick up an expectation query with no evidence" in {
        val result = performVisit("E[A + B] #Q1<0.9, 0.1>")
        assert(result.size == 1)
        assert(result.head == TestData.Queries.APlusB)
    }
}

class MDLEvidenceListVisitorSpec extends ValueProducingVisitorTest[Seq[Condition], Seq[Condition]] {
    override def makeVisitor() = new MDLEvidenceListVisitor()

    it should "pick up an empty list" in {
        val result = performVisit("")
        assert(result.isEmpty)
    }

    it should "pick up a single compile-time evidence" in {
        val result = performVisit("A = 1")
        assert(result.size == 1)
        assert(result.head == Condition()
            .withNode("A")
            .withValue(1))
    }

    it should "pick up a single run-time evidence" in {
        val result = performVisit("A")
        assert(result.size == 1)
        assert(result.head == Condition()
            .withNode("A"))
    }
}

class MDLRelationQueryVisitorSpec extends ValueProducingVisitorTest[RelationQuery, RelationQuery] {
    override def makeVisitor() = new MDLRelationQueryVisitor()

    it should "pick up a simple comparison" in {
        val result = performVisit("A > B")
        assert(result == TestData.RelationQueries.AGreaterB)
    }
}

class MDLQueryExpressionVisitorSpec extends ValueProducingVisitorTest[QueryExpression, QueryExpression] {
    override def makeVisitor() = new MDLQueryExpressionVisitor()

    behavior of "QueryExpressionVisitor"

    it should "pick up a node" in {
        val result = performVisit("A")
        assert(result == TestData.QueryExpressions.constA)
    }
}

object Util {
    def getParser(text: String): PGMFileParser = {
        val stream = new ANTLRInputStream(text)
        val lexer = new PGMFileLexer(stream)
        val tokenStream = new CommonTokenStream(lexer)
        val parser = new PGMFileParser(tokenStream)

        parser
    }

    def makeDistribution(distribution: Double*) = {
        val list = List(distribution: _*)
        CollectionConverters.asJava(list.map(Double.box))
    }

    def makeIter[T](vals: T*) = {
        val list = List(vals: _*)
        CollectionConverters.asJava(list)
    }
}

/**
  * A test for any ValueProducingVisitor. Gives a neat interface to parse
  * 'queries', which are just snippets of PGM that pertain to that visitor alone.
  *
  * Unfortunately, due to the limitations of Scala generics/type inference
  * we need to explicitly provide T,E which are the types that the visitor-under-test
  * uses to inherit {@link ValueProducingVisitor}. If there is a better work around please
  * implement it.
  */
abstract class ValueProducingVisitorTest[T, E] extends AnyFlatSpec {
    def makeVisitor(): ValueProducingVisitor[T, E]

    /**
      * Get the value produced by the visitor after parsing a query snippet
      *
      * @param query the query snippet, which should correspond to the syntax
      *              of the relevant rule for the visitor
      * @return the value produced by the visitor after parsing and visiting
      *         the resulting syntax tree
      */
    def performVisit(query: String): T = {
        val parser = Util.getParser(query)
        val visitor = makeVisitor()
        // syntax is a little bit awkward, let me know if there is a better way
        // basically each visitor gives us a function which produces a rule context,
        // which is usually just a parser rule method. we use this to parse the tree
        val parserRule = visitor.parserRule()
        val tree = parserRule(parser)
        tree.accept(visitor)

        visitor.getValue()
    }
}
