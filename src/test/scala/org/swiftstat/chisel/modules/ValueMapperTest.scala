package org.swiftstat.chisel.modules

import chisel3._
import chiseltest._
import chiseltest.ChiselScalatestTester
import org.scalatest.flatspec.AnyFlatSpec
import org.swiftstat.pgm.compiler.entity.UserNodeEntity

import hardfloat._
import org.swiftstat.chisel.modules.FloatUtils._
import org.swiftstat.util.exp
import org.swiftstat.util.sig
import org.swiftstat.pgm.compiler.entity.UserNodeValue

/**
  * Sets a SampleSet to the provided values in a Map[String, Int]
  *
  * @param nodes
  * @param values
  */
class StaticSampleSet(nodes: Seq[UserNodeEntity], values: Map[String, Int]) extends Module {
    val io = IO(new Bundle {
        val samples = Output(SampleSet(nodes))
    })

    io.samples.samplesByName.foreach {
        case (name, sample) =>
            val sampleValue = WireInit(values(name).U(sample.getWidth.W))
            sample := sampleValue
    }
}

class SampleAndMapStatic(nodes: Seq[UserNodeEntity], values: Map[String, Int]) extends Module {
    val samples = Module(new StaticSampleSet(nodes, values))
    val mapper = Module(new ValueMapper(nodes))
    mapper.io.samples := samples.io.samples

    val io = IO(new Bundle {
        val mapped = Output(Vec(nodes.size, FN()))
    })
    io.mapped := VecInit(mapper.io.mapped.rawValues.map(_.FN))
}

class SampleAndMapDynamic(nodes: Seq[UserNodeEntity]) extends Module {
    val io = IO(new Bundle {
        val samples = Input(SampleSet(nodes))
        val mapped = Output(Vec(nodes.size, FN()))
    })
    val mapper = Module(new ValueMapper(nodes))
    mapper.io.samples := io.samples
    io.mapped := VecInit(mapper.io.mapped.rawValues.map(_.FN))
}

class ValueMapperTest extends AnyFlatSpec with ChiselScalatestTester {
    val nA = UserNodeEntity("A", Seq(), Seq(1D, 2), Seq(0.03125, 0.96875))
    val nB = UserNodeEntity("B", Seq(), Seq(1D, 2), Seq(0.5, 0.5))
    val nC = UserNodeEntity("C", Seq(nA, nB), Seq(1D, 2, 50, 23), Seq(0.25, 0.25, 0.25, 0.25,
                                                                    0.25, 0.25, 0.25, 0.25,
                                                                    0.25, 0.25, 0.25, 0.25,
                                                                    0.25, 0.25, 0.25, 0.25))
    behavior of "StaticSampleMap"
    it should "index the second of each pair" in
    test({
        new SampleAndMapStatic(Seq(nA, nB, nC), Map("A" -> 1, "B" -> 1, "C" -> 1))
    }) { c =>
        for (i <- 0 until 100) {
            c.clock.step()
            c.io.mapped.foreach(fn => {
                val double = fn.peek().toDouble

                assert(double == 2.0);
            });
        }
    }

    it should "dynamically map samples" in
    test({
        new SampleAndMapDynamic(Seq(nA, nB, nC))
    }) {
        c =>
        c.io.samples.samplesByName("A").poke(0.U)
        assert(c.io.mapped(0).peek().toDouble == 1.0)
        c.io.samples.samplesByName("A").poke(1.U)
        assert(c.io.mapped(0).peek().toDouble == 2.0)

        c.io.samples.samplesByName("B").poke(0.U)
        assert(c.io.mapped(1).peek().toDouble == 1.0)
        c.io.samples.samplesByName("B").poke(1.U)
        assert(c.io.mapped(1).peek().toDouble == 2.0)

        c.io.samples.samplesByName("C").poke(0.U)
        assert(c.io.mapped(2).peek().toDouble == 1.0)
        c.io.samples.samplesByName("C").poke(1.U)
        assert(c.io.mapped(2).peek().toDouble == 2.0)
        c.io.samples.samplesByName("C").poke(2.U)
        assert(c.io.mapped(2).peek().toDouble == 50.0)
        c.io.samples.samplesByName("C").poke(3.U)
        assert(c.io.mapped(2).peek().toDouble == 23.0)
    }

}
