package org.swiftstat.chisel.modules.examples

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.swiftstat.chisel.modules._

class BinaryOrphanCPDSamplerTest extends AnyFlatSpec with ChiselScalatestTester {
    "BinaryOrphanCPDSampler" should "sample" in {
        test(new BinaryOrphanCPDSampler) { c =>
            val distribution = scala.collection.mutable.Map[Int, Int](
                0 -> 0,
                1 -> 0)

            for (i <- 1 to 10_000) {
                val sample = c.io.peekInt()
                distribution(sample.toInt) += 1

                c.clock.step()
                c.clock.setTimeout(0)
            }

            val total = distribution.values.sum.toDouble
            val pct_0 = distribution(0) / total
            val pct_1 = distribution(1) / total
            assert(pct_0 === 0.5 +- 0.05)
            assert(pct_1 === 0.5 +- 0.05)
        }
    }
}