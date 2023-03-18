package org.swiftstat.chisel.modules

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class BasicTest extends AnyFlatSpec with ChiselScalatestTester {
    "ExampleModule" should "add" in {
        test(new ExampleModule) { c =>
            c.io.a.poke(1.U)
            c.io.b.poke(2.U)
            c.clock.step()
            c.io.out.expect(3.U)
        }
    }

    it should "not add" in {
        test(new ExampleModule) {c =>
            c.io.a.poke(1.U)
            c.io.b.poke(2.U)
            c.io.out.expect(0.U)
        }
    }
}