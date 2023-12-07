package org.swiftstat.chisel.modules

import chisel3._
import chiseltest._
import chiseltest.ChiselScalatestTester
import org.scalatest.flatspec.AnyFlatSpec
import org.swiftstat.pgm.compiler.entity.UserNodeEntity
import org.swiftstat.mdl.UserNode
import chisel3.stage.ChiselStage
import chisel3.util.MixedVec
import chisel3.util.MixedVecInit


class CPDSamplerTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "CPDSampler"

    def getDist(parentAssignment: Map[String, Int])(implicit c: CPDSampler): Map[Int, Double] = {
        for ((parent, value) <- parentAssignment) {
            c.io.parents.samplesByName(parent).poke(value.U)
        }

        val distribution = scala.collection.mutable.Map[Int, Int](
            (0 to 1).map(i => i -> 0): _*)

        c.clock.setTimeout(0)

        for (i <- 1 to 5000) {
            val sampleValue = c.io.sample.peekInt()
            distribution(sampleValue.toInt) += 1

            c.clock.step()
        }

        val total = distribution.values.sum.toDouble
        val measuredDistribution = distribution.map
            { case (k, v) => k -> v / total }

        measuredDistribution.toMap
    }

    it should "sample from the correct row for two parents" in
        test ({
            val a = UserNodeEntity("A", Seq(), Seq(0D, 1), Seq(0.03125, 0.96875))
            val b = UserNodeEntity("B", Seq(), Seq(0D, 1), Seq(0.5, 0.5))
            val c = UserNodeEntity("C", Seq(a, b), Seq(0D, 1), Seq(0.5, 0.5,
                                                                   0.25, 0.75,
                                                                   0.125, 0.875,
                                                                   0.1875, 0.8125))
            new CPDSampler(c)
        }).withAnnotations(Seq(VerilatorBackendAnnotation)) {
            c =>

            implicit val cpdSampler = c

            val distWhenA0B0 = getDist(Map("A" -> 0, "B" -> 0))
            val distWhenA0B1 = getDist(Map("A" -> 0, "B" -> 1))
            val distWhenA1B0 = getDist(Map("A" -> 1, "B" -> 0))
            val distWhenA1B1 = getDist(Map("A" -> 1, "B" -> 1))

            assert(distWhenA0B0(0) === 0.5 +- 0.02 &&
                   distWhenA0B0(1) === 0.5 +- 0.02)

            assert(distWhenA0B1(0) === 0.25 +- 0.02 &&
                   distWhenA0B1(1) === 0.75 +- 0.02)

            assert(distWhenA1B0(0) === 0.125 +- 0.02 &&
                   distWhenA1B0(1) === 0.875 +- 0.02)

            assert(distWhenA1B1(0) === 0.1875 +- 0.02 &&
                   distWhenA1B1(1) === 0.8125 +- 0.02)
    }

    it should "sample from the correct row" in
        test ({
            val a = UserNodeEntity("A", Seq(), Seq(0D, 1), Seq(0.5, 0.5))
            val b = UserNodeEntity("B", Seq(a), Seq(0D, 1), Seq(0.75, 0.25,
                                                                0.25, 0.75))
            new CPDSampler(b)
        }).withAnnotations(Seq(VerilatorBackendAnnotation)) {
            c =>

            implicit val cpdSampler = c

            val distWhenA0 = getDist(Map("A" -> 0))
            val distWhenA1 = getDist(Map("A" -> 1))

            assert(distWhenA0(0) === 0.75 +- 0.03 &&
                   distWhenA0(1) === 0.25 +- 0.03)

            assert(distWhenA1(0) === 0.25 +- 0.03 &&
                   distWhenA1(1) === 0.75 +- 0.03)
        }

    it should "construct a proper IO bundle for a single-parent node" in
        test ({
            val a = UserNodeEntity("A", Seq(), Seq(0D, 1), Seq(0.5, 0.5))
            val b = UserNodeEntity("B", Seq(a), Seq(2D, 3, 4, 5), Seq(0.125, 0.5, 0.125, 0.25,
                                                                      0.25, 0.125, 0.125, 0.5))
            new CPDSampler(b)
        }).withAnnotations(Seq(VerilatorBackendAnnotation)) {
            c =>

            assert(c.io.parents.samples.size == 1)
            assert(c.io.parents.samples(0).getWidth == 1)
            assert(c.io.sample.getWidth == 2)
        }

    it should "construct a proper IO bundle for an orphan" in
        test({
            val a = UserNodeEntity("A", Seq(), Seq(0D, 1), Seq(0.5, 0.5))
            new CPDSampler(a)
        }).withAnnotations(Seq(VerilatorBackendAnnotation)) {
            c =>

            assert(c.io.parents.samples.size == 0)
            assert(c.io.sample.getWidth == 1)
        }

    it should "construct a proper IO bundle for a 3-parent node" in
        test({
            val a = UserNodeEntity("A", Seq(), Seq(0D, 1), Seq(0.5, 0.5))
            val b = UserNodeEntity("B", Seq(), Seq(2D, 3), Seq(0.5, 0.5))
            val c = UserNodeEntity("C", Seq(), Seq(4D, 5), Seq(0.5, 0.5))
            val d = UserNodeEntity("D", Seq(a, b, c), Seq(6D, 7),
                Seq(0.125,  0.875,
                    0.25,   0.75,
                    0.375,  0.625,
                    0.5,    0.5,
                    0.625,  0.375,
                    0.75,   0.25,
                    0.875,  0.125,
                    0.5,    0.5))

            new CPDSampler(d)
        }).withAnnotations(Seq(VerilatorBackendAnnotation)) {
            c =>

            assert(c.io.parents.samples.size == 3)
            assert(c.io.parents.samples(0).getWidth == 1)
            assert(c.io.parents.samples(1).getWidth == 1)
            assert(c.io.parents.samples(2).getWidth == 1)
            assert(c.io.sample.getWidth == 1)
        }
}
