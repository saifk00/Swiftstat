package org.swiftstat.chisel.modules

import chisel3._
import chiseltest._
import chiseltest.ChiselScalatestTester

import org.scalatest.flatspec.AnyFlatSpec
import hardfloat._
import org.swiftstat.chisel.modules.FloatUtils._
import org.swiftstat.pgm.compiler.entity.UserNodeValue
import org.swiftstat.pgm.compiler.entity._
import org.swiftstat.pgm.compiler.entity.ast._



class DummyWeightMapper(query: QueryEntity, nodes: Seq[UserNodeEntity], samples: Map[String, Int]) extends Module {
    val nodeSamples = Module(new StaticSampleSet(nodes, samples))

    val io = IO(new Bundle {
        val out = Output(Vec(query.evidenceNodes.size, FN()))
    })

    val weightMapper = WeightMapper(nodes, query, nodeSamples.io.samples)

    // convert the output to double
    io.out := VecInit(weightMapper.rawValues.map(_.FN))
}

class WeightMapperTest extends AnyFlatSpec with ChiselScalatestTester {
    val nA = UserNodeEntity("A", Seq(), Seq(0D, 1), Seq(0.03125, 0.96875))
    val nB = UserNodeEntity("B", Seq(), Seq(0D, 1), Seq(0.5, 0.5))
    val nC = UserNodeEntity("C", Seq(nA, nB), Seq(0D, 1), Seq(0.5, 0.5,
                                                            0.25, 0.75,
                                                            0.125, 0.875,
                                                            0.1875, 0.8125))
    val queryEAplusBGivenC1 = QueryEntity(
        "test",
        EpsilonDeltaSpecification(0.5, 0.5),
        QueryExpressionBinaryOp(
            QueryExpressionNodeOp(nA),
            QueryExpressionNodeOp(nB),
            QueryBinaryOpTypes.`+`
        ),
        Set(CompileTimeEvidenceEntity(nC, nC.values(1))),
        QueryTypes.Expectation
    )

    // TODO(skhattak) better way to do these 4 tests
    behavior of "WeightMapper"

    it should "map the weights of a compile-time evidence node (1/4)" in
    test({
        val nodes = Seq(nA, nB, nC)
        val query = queryEAplusBGivenC1
        val samples = Map("A" -> 0, "B" -> 0, "C" -> 1)
        new DummyWeightMapper(query, nodes, samples)
    }) { c =>
        // only 1 weight
        assert(c.io.out.size == 1)
        assert(c.io.out(0).peek().toDouble == 0.5)
    }

    it should "map the weights of a compile-time evidence node (2/4)" in
    test({
        val nodes = Seq(nA, nB, nC)
        val query = queryEAplusBGivenC1
        val samples = Map("A" -> 0, "B" -> 1, "C" -> 1)
        new DummyWeightMapper(query, nodes, samples)
    }) { c =>
        // only 1 weight
        assert(c.io.out.size == 1)
        assert(c.io.out(0).peek().toDouble == 0.75)
    }

    it should "map the weights of a compile-time evidence node (3/4)" in
    test({
        val nodes = Seq(nA, nB, nC)
        val query = queryEAplusBGivenC1
        val samples = Map("A" -> 1, "B" -> 0, "C" -> 1)
        new DummyWeightMapper(query, nodes, samples)
    }) { c =>
        // only 1 weight
        assert(c.io.out.size == 1)
        assert(c.io.out(0).peek().toDouble == 0.875)
    }

    it should "map the weights of a compile-time evidence node (4/4)" in
    test({
        val nodes = Seq(nA, nB, nC)
        val query = queryEAplusBGivenC1
        val samples = Map("A" -> 1, "B" -> 1, "C" -> 1)
        new DummyWeightMapper(query, nodes, samples)
    }) { c =>
        // only 1 weight
        assert(c.io.out.size == 1)
        assert(c.io.out(0).peek().toDouble == 0.8125)
    }

    val queryEAplusBGivenCRuntime = QueryEntity(
        "test",
        EpsilonDeltaSpecification(0.5, 0.5),
        QueryExpressionBinaryOp(
            QueryExpressionNodeOp(nA),
            QueryExpressionNodeOp(nB),
            QueryBinaryOpTypes.`+`
        ),
        Set(RunTimeEvidenceEntity(nC)),
        QueryTypes.Expectation
    )


    it should "map the weights of a run-time evidence node (1/2)" in
    test({
        val nodes = Seq(nA, nB, nC)
        val query = queryEAplusBGivenCRuntime
        val samples = Map("A" -> 1, "B" -> 0, "C" -> 1)
        new DummyWeightMapper(query, nodes, samples)
    }) { c =>
        // only 1 weight
        assert(c.io.out.size == 1)
        assert(c.io.out(0).peek().toDouble == 0.875)
    }

    it should "map the weights of a run-time evidence node (2/2)" in
    test({
        val nodes = Seq(nA, nB, nC)
        val query = queryEAplusBGivenCRuntime
        val samples = Map("A" -> 1, "B" -> 0, "C" -> 0)
        new DummyWeightMapper(query, nodes, samples)
    }) { c =>
        // only 1 weight
        assert(c.io.out.size == 1)
        assert(c.io.out(0).peek().toDouble == 0.125)
    }
}