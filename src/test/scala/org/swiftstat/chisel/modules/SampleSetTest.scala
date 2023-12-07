package org.swiftstat.chisel.modules

import chisel3._
import chiseltest._
import chiseltest.ChiselScalatestTester
import org.scalatest.flatspec.AnyFlatSpec
import org.swiftstat.pgm.compiler.entity.UserNodeEntity

class SampleSetTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "SampleSet"

    it should "construct a single sample bundle" in
    {
        val node = UserNodeEntity("A", Seq(), Seq(0D, 1, 223, 44), Seq(0.25, 0.25, 0.25, 0.25))
        val sampleSet = SampleSet(Seq(node))

        assert(sampleSet.samples.size == 1)
        assert(sampleSet.samples(0).getWidth == 2)
    }

    it should "construct a bundle of 3 nodes" in
    {
        val a = UserNodeEntity("A", Seq(), Seq(0D, 1, 223, 44), Seq.fill(4)(0.25))
        val b = UserNodeEntity("B", Seq(), Seq(0D, 1, 223, 44, 42, 24, 55, 35), Seq.fill(8)(0.125))
        val c = UserNodeEntity("C", Seq(), Seq.range(0, 16).map(_.toDouble), Seq.fill(16)(0.0625))

        val sampleSet = SampleSet(Seq(a, b, c))

        assert(sampleSet.samples.size == 3)
        assert(sampleSet.samples(0).getWidth == 2)
        assert(sampleSet.samples(1).getWidth == 3)
        assert(sampleSet.samples(2).getWidth == 4)
    }

    it should "map by name correctly" in
    {
        val a = UserNodeEntity("A", Seq(), Seq(0D, 1, 223, 44), Seq.fill(4)(0.25))
        val b = UserNodeEntity("B", Seq(), Seq(0D, 1, 223, 44, 42, 24, 55, 35), Seq.fill(8)(0.125))
        val c = UserNodeEntity("C", Seq(), Seq.range(0, 16).map(_.toDouble), Seq.fill(16)(0.0625))

        val sampleSet = SampleSet(Seq(a, b, c))

        assert(sampleSet.samples.size == 3)
        assert(sampleSet.samplesByName("A").getWidth == 2)
        assert(sampleSet.samplesByName("B").getWidth == 3)
        assert(sampleSet.samplesByName("C").getWidth == 4)
    }

}
