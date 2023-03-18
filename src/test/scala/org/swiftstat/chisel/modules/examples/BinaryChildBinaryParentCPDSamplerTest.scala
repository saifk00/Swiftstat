package org.swiftstat.chisel.modules.examples

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.swiftstat.chisel.modules._

class BinaryChildBinaryParentCPDSamplerTest extends AnyFlatSpec with ChiselScalatestTester {
    "BinaryChildBinaryParentCPDSampler" should "sample" in {
        test(new BinaryChildBinaryParentCPDSampler).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
            val distribution = scala.collection.mutable.Map[Int, Int](
                0 -> 0,
                1 -> 0)

            var parentSample = 0;
            for (i <- 1 to 10_000) {
                // simulate a 50-50 parent sampler
                c.io.parentSample.samples(0).sample.poke(parentSample.U)
                parentSample = 1 - parentSample

                val sample = c.io.output.sample.peekInt()
                distribution(sample.toInt) += 1

                c.clock.step()
                c.clock.setTimeout(0)
            }

            val total = distribution.values.sum.toDouble
            val pct_0 = distribution(0) / total
            val pct_1 = distribution(1) / total

            // see how the resulting distribution is pretty darn close!
            assert(pct_0 === 0.1875 +- 0.01)
            assert(pct_1 === 0.8125 +- 0.01)
        }
    }
}