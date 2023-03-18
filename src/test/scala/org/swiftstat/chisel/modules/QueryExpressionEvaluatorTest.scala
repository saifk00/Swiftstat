package org.swiftstat.chisel.modules

import chisel3._
import chiseltest._
import chiseltest.ChiselScalatestTester
import org.scalatest.flatspec.AnyFlatSpec
import org.swiftstat.pgm.compiler.entity.UserNodeEntity

import hardfloat._
import org.swiftstat.util.exp
import org.swiftstat.util.sig
import org.swiftstat.pgm.compiler.entity.QueryEntity
import org.swiftstat.pgm.compiler.entity.EpsilonDeltaSpecification
import org.swiftstat.pgm.compiler.entity.ast._
import org.swiftstat.pgm.compiler.entity.QueryTypes
import org.swiftstat.mdl.UserNode
import org.swiftstat.chisel.modules.FloatUtils._
import scala.runtime.Static
import org.swiftstat.pgm.compiler.entity.ast.QueryExpressionConstOp

class StaticValueSet(nodes: Seq[UserNodeEntity], values: Map[String, Double]) extends Module {
    val io = IO(new Bundle {
        val sampledValues = Output(ValueSet(nodes))
    })

    io.sampledValues.valuesByName.foreach {
        case (name, value) =>
            value := values(name).RecFN
    }
}

/**
  * Wires in static sample values to a QueryExpressionEvaluator
  *
  * @param query the query to compute
  * @param nodes the set of nodes involved
  * @param values the values to set for each node involved in the query
  */
class DummyQueryExpressionEvaluator(query: QueryEntity, nodes: Seq[UserNodeEntity], values: Map[String, Double]) extends Module {
    val nodeValues = Module(new StaticValueSet(nodes, values))

    val io = IO(new Bundle {
        val queryValue = Output(FN())
    })

    val queryExpressionEvaluator = QueryExpressionEvaluator(query, nodeValues.io.sampledValues)

    // convert the output to double
    io.queryValue := queryExpressionEvaluator.io.queryValue.FN
}

class QueryExpressionEvaluatorTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "QueryExpressionEvaluator"

    val a = UserNodeEntity("A", Seq(), Seq[Double](1, 2), Seq(0.1, 0.9))
    val b = UserNodeEntity("B", Seq(a), Seq[Double](1, 2, 3), Seq(0.1, 0.2, 0.7,
                                                                  0.5, 0.4, 0.1))
    val c = UserNodeEntity("C", Seq(a, b), Seq[Double](1, 2,50), Seq(0.1, 0.2, 0.7,
                                                                  0.5, 0.4, 0.1,
                                                                  0.2, 0.5, 0.3,
                                                                  0.1, 0.2, 0.7,
                                                                  0.2, 0.4, 0.4,
                                                                  0.4, 0.2, 0.4))
    val nodes = Seq(a, b, c)
    val queryEA_plus_B = QueryEntity(
        "test",
        EpsilonDeltaSpecification(0.5, 0.5),
        QueryExpressionBinaryOp(
            QueryExpressionNodeOp(a),
            QueryExpressionNodeOp(b),
            QueryBinaryOpTypes.`+`
        ),
        Set(),
        QueryTypes.Expectation
    )

    it should "synthesize query logic" in
    test({
        new DummyQueryExpressionEvaluator(queryEA_plus_B, nodes, Map("A" -> 2, "B" -> 2, "C" -> 0))
    }) { c =>

        val measured = c.io.queryValue.peek().toDouble
        assert(measured == 2 + 2)
    }

    val queryEA_minus_BC_plus_3 = QueryEntity(
        "test2",
        EpsilonDeltaSpecification(0.5, 0.5),
        QueryExpressionBinaryOp(
            QueryExpressionBinaryOp(
                QueryExpressionNodeOp(a),
                QueryExpressionBinaryOp(
                    QueryExpressionNodeOp(b),
                    QueryExpressionNodeOp(c),
                    QueryBinaryOpTypes.`*`
                ),
                QueryBinaryOpTypes.`-`
            ),
            QueryExpressionConstOp(3D),
            QueryBinaryOpTypes.`+`
        ),
        Set(),
        QueryTypes.Expectation
    )

    it should "synthesize multi-level query logic" in
    test({
        // 1 - 3*2 + 3 = -2
        new DummyQueryExpressionEvaluator(queryEA_minus_BC_plus_3, nodes, Map("A" -> 1, "B" -> 3, "C" -> 2))
    }) {
        c =>

        val measured = c.io.queryValue.peek().toDouble
        assert(measured == 1 - 3*2 + 3)
    }

    val queryEAgtB = QueryEntity(
        "test2",
        EpsilonDeltaSpecification(0.5, 0.5),
        QueryExpressionList(Seq(QueryExpressionBinaryOp(
            QueryExpressionNodeOp(a),
            QueryExpressionNodeOp(b),
            QueryBinaryOpTypes.`>`
        ))),
        Set(),
        QueryTypes.Expectation
    )

    it should "synthesize comparison query logic (1)" in
    test({
        // 2 > 1
        new DummyQueryExpressionEvaluator(queryEAgtB, nodes, Map("A" -> 2, "B" -> 1, "C" -> 50))
    }) {
        c =>

        val measured = c.io.queryValue.peek().toDouble
        assert(measured == 1)
    }

    it should "synthesize comparison query logic (0)" in
    test({
        // 1 < 2
        new DummyQueryExpressionEvaluator(queryEAgtB, nodes, Map("A" -> 1, "B" -> 2, "C" -> 50))
    }) {
        c =>

        val measured = c.io.queryValue.peek().toDouble
        assert(measured == 0)
    }

    val query300AplusB = QueryEntity(
        "Q0",
        EpsilonDeltaSpecification(0.5, 0.5),
        QueryExpressionBinaryOp(
                    QueryExpressionBinaryOp(
                        QueryExpressionConstOp(300.0),
                        QueryExpressionNodeOp(b),
                        QueryBinaryOpTypes.`*`
                    ),
                    QueryExpressionNodeOp(c),
                    QueryBinaryOpTypes.`+`),
        Set(),
        QueryTypes.Expectation)

    it should "compute 300*2 + 50 instead of 300*(2+50)" in
    test({
        new DummyQueryExpressionEvaluator(query300AplusB, nodes, Map("A" -> 1, "B" -> 2, "C" -> 50))
    }) {
        c =>
            val measured = c.io.queryValue.peek().toDouble
            assert(measured == 300*2 + 50)
    }
}
