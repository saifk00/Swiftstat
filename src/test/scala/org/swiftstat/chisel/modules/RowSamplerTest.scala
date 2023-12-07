package org.swiftstat.chisel.modules

import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.ChiselScalatestTester
import chisel3.Bool._
import chisel3._
import chiseltest._
import org.swiftstat.chisel.ddg._
import org.swiftstat.pgm.compiler.entity.CPDRowEntity

case class MeasuredDistribution(measured: Map[Int, Double]) {
    override def toString(): String =  measured
        .map { case (key, value) =>
            s"frequency of ${key}: ${value}" }.reduce(_ + "\n" + _)

    def printForTest(): Unit = println(toString())

    def apply(key: Int): Double = measured(key)
}

object TestUtil {
    def distributionTest(sampler: RowSampler, maxValue: Int): MeasuredDistribution = {
        val distribution = scala.collection.mutable.Map[Int, Int](
            (0 to maxValue).map(i => i -> 0): _*)

        sampler.clock.setTimeout(0)
        for (i <- 1 to 10_000) {
            val sample = sampler.io.peekInt()
            distribution(sample.toInt) += 1

            sampler.clock.step()
        }

        val total = distribution.values.sum.toDouble
        val measuredDistribution = distribution.map { case (value, count) =>
            value -> count / total
        }

        MeasuredDistribution(measuredDistribution.toMap)
    }
}

class RowSamplerTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "RowSampler"

    it should "generate a 50-50 distribution" in
        test(new RowSampler(Map(0 -> Set(Seq(1))))) {
            c =>
            val measured = TestUtil.distributionTest(c, 1)

            assert(measured(0) === 0.5 +- 0.02)
            assert(measured(1) === 0.5 +- 0.02)
        }

    it should "handle the output of the minimalDDG bitPrefixSet factory" in
        test({
            val ddg = MinimalDDG.fromCPDRowEntity(
                CPDRowEntity(Seq(0.25, 0.25, 0.25, 0.25)),
                bitPrecision = 4
            )

            new RowSampler(ddg.getBitPrefixMap())
        }) {
            c =>
            val measured = TestUtil.distributionTest(c, 3)

            assert(measured(0) === 0.25 +- 0.02)
            assert(measured(1) === 0.25 +- 0.02)
            assert(measured(2) === 0.25 +- 0.02)
            assert(measured(3) === 0.25 +- 0.02)
        }

    it should "generate a nontrivial distribution" in
        test(RowSampler.fromCPDRow(CPDRowEntity(Seq(0.125, 0.875)))) {
            c =>
            val measured = TestUtil.distributionTest(c, 1)

            assert(measured(0) === 0.125 +- 0.01)
            assert(measured(1) === 0.875 +- 0.01)
        }
}
