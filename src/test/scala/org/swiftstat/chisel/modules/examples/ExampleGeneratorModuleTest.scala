package org.swiftstat.chisel.modules.examples

import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.ChiselScalatestTester
import chisel3.Bool._
import chisel3._
import chiseltest._

class ExampleGeneratorModuleTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "BoolVectorModule"
    val boolValues = Seq(true, false, true)
    it should "hold the data constant" in
        test(new BoolVectorModule(boolValues.map(_.B))) {
            c =>
            // this is real ugly
            (0 until boolValues.size).foreach(i => c.io.out(i).expect(boolValues(i)))

            // slightly better
            c.io.out.zip(boolValues).foreach { case (actual, expected) => actual.expect(expected)}

            // TODO(skhattak) could we define a vectored `expect` to do something like
            // c.io.out.expectAll(boolValues)
        }

    behavior of "ExampleGeneratorModule"
    it should "map the data correctly" in
        test(new ExampleGeneratorModule(
            Seq(0, 2),
            Module(new BoolVectorModule(boolValues.map(_.B))))
        ) {
            c =>

            assert(c.io.out.size == 2)
            c.io.out(0).expect(true)
            c.io.out(1).expect(true)
        }

    behavior of "ExampleRandomMapperModule"
    it should "get a random bool" in
        test(new ExampleRandomMapperModule(4)) {
        c =>

        var trueOccurrences = 0
        for (i <- 0 to 10) {
            trueOccurrences = if (c.io.out.peekBoolean()) trueOccurrences + 1 else trueOccurrences
            c.clock.step()
        }
    }
}
