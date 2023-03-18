package org.swiftstat.pgm.compiler.entity

import org.scalatest.flatspec.AnyFlatSpec
import org.antlr.v4.runtime.ANTLRInputStream
import org.swiftstat.pgm.antlr.PGMFileLexer
import org.antlr.v4.runtime.CommonTokenStream
import org.swiftstat.pgm.antlr.PGMFileParser
import org.swiftstat.pgm.visitors.MDLQueriesVisitor
import org.swiftstat.mdl.QueryProto
import org.swiftstat.pgm.compiler.entity.ast.QueryExpressionNodeOp
import org.swiftstat.pgm.compiler.entity.ast.QueryExpressionBinaryOp
import org.swiftstat.pgm.compiler.entity.ast.QueryBinaryOpTypes
import org.swiftstat.pgm.compiler.entity.ast.QueryExpressionList
import org.swiftstat.pgm.compiler.entity.ast.QueryExpressionConstOp

object Util {
    def compileQuery(text: String): Set[QueryProto] = {
        val stream = new ANTLRInputStream(text)
        val lexer = new PGMFileLexer(stream)
        val tokenStream = new CommonTokenStream(lexer)
        val parser = new PGMFileParser(tokenStream)
        val queryVisitor = new MDLQueriesVisitor()
        val tree = queryVisitor.parserRule()(parser)
        tree.accept(queryVisitor)

        queryVisitor.getValue()
    }
}

class QueryExpressionASTBuilderTest extends AnyFlatSpec {
    behavior of "QueryExpressionASTBuilder"

    it should "handle the expectation of a plain node" in {
        val proto = Util.compileQuery("E[A] #Q1<0.1, 0.9>").head
        val a = UserNodeEntity("A", Seq(), Seq[Double](1, 2, 3), Seq(0.1, 0.2, 0.7))
        val nodes = Set(a)
        val builder = new QueryExpressionASTBuilder(nodes)
        val actual = builder.parseProto(proto.getExpectationQuery.nodeFunction.get)
        val expected = QueryExpressionNodeOp(a)

        assert(actual == expected)
    }

    it should "handle one query in a marginal query" in {
        val proto = Util.compileQuery("P[A > B | C] #Q1<0.1, 0.9>")
            .head
            .getMarginalQuery
            .unobserved
            .head
        val a = UserNodeEntity("A", Seq(), Seq[Double](1, 2), Seq(0.1, 0.9))
        val b = UserNodeEntity("B", Seq(a), Seq[Double](1, 2, 3), Seq(0.1, 0.2, 0.7,
                                                                      0.5, 0.4, 0.1))
        val nodes = Set(a, b)
        val builder = new QueryExpressionASTBuilder(nodes)
        val actual = builder.parseProto(proto)
        val expected = QueryExpressionBinaryOp(
            QueryExpressionNodeOp(a),
            QueryExpressionNodeOp(b),
            QueryBinaryOpTypes.`>`)

        assert(actual == expected)
    }

    it should "handle a list of queries in a marginal query" in {
        val proto = Util.compileQuery("P[A > B, A = C | C] #Q1<0.1, 0.9>")
            .head
            .getMarginalQuery
            .unobserved
        val a = UserNodeEntity("A", Seq(), Seq[Double](1, 2), Seq(0.1, 0.9))
        val b = UserNodeEntity("B", Seq(a), Seq[Double](1, 2, 3), Seq(0.1, 0.2, 0.7,
                                                                      0.5, 0.4, 0.1))
        val c = UserNodeEntity("C", Seq(a), Seq[Double](1, 2, 3), Seq(0.1, 0.2, 0.7,
                                                                      0.5, 0.4, 0.1))
        val nodes = Set(a, b, c)
        val builder = new QueryExpressionASTBuilder(nodes)
        val actual = builder.parseProto(proto)
        val expected = QueryExpressionList(Seq(
            QueryExpressionBinaryOp(
                QueryExpressionNodeOp(a),
                QueryExpressionNodeOp(b),
                QueryBinaryOpTypes.`>`),
            QueryExpressionBinaryOp(
                QueryExpressionNodeOp(a),
                QueryExpressionNodeOp(c),
                QueryBinaryOpTypes.`=`)))

        assert(actual == expected)
    }

    it should "handle a const node" in {
        val proto = Util.compileQuery("E[1] #Q1<0.1, 0.9>").head
        val nodes = Set[UserNodeEntity]()
        val builder = new QueryExpressionASTBuilder(nodes)
        val actual = builder.parseProto(proto.getExpectationQuery.nodeFunction.get)
        val expected = QueryExpressionConstOp(1)

        assert(actual == expected)
    }

    it should "handle a compute op in an expectation query" in {
        val proto = Util.compileQuery("E[A + B | C] #Q1<0.1, 0.9>")
            .head
            .getExpectationQuery
            .nodeFunction
            .get
        val a = UserNodeEntity("A", Seq(), Seq[Double](1, 2), Seq(0.1, 0.9))
        val b = UserNodeEntity("B", Seq(a), Seq[Double](1, 2, 3), Seq(0.1, 0.2, 0.7,
                                                                      0.5, 0.4, 0.1))
        val c = UserNodeEntity("C", Seq(a), Seq[Double](1, 2, 3), Seq(0.1, 0.2, 0.7,
                                                                      0.5, 0.4, 0.1))
        val nodes = Set(a, b, c)
        val builder = new QueryExpressionASTBuilder(nodes)
        val actual = builder.parseProto(proto)
        val expected = QueryExpressionBinaryOp(
            QueryExpressionNodeOp(a),
            QueryExpressionNodeOp(b),
            QueryBinaryOpTypes.`+`)

        assert(actual == expected)
    }
}
