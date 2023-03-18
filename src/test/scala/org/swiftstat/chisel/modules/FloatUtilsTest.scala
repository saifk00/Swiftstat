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

class BackAndForthConverter(value: Double) extends Module {
    val io = IO(new Bundle {
        val out = Output(FN())
    })

    io.out := value.RecFN.FN
}

class TestAdder(a: Double, b: Double) extends Module {
    val sum = AddRecFN(a.RecFN, b.RecFN)

    val io = IO(new Bundle {
        val out = Output(FN())
    })

    val backToRaw = Module(new RecFNToFNConverter)
    backToRaw.io.in := sum

    io.out := backToRaw.io.out
}

class TestLT(a: Double, b: Double) extends Module {
    val io = IO(new Bundle {
        val out = Output(Bool())
    })

    val isLT = RecFNComparer.lessThan(a.RecFN, b.RecFN)
    io.out := isLT
}

class TestGT(a: Double, b: Double) extends Module {
    val io = IO(new Bundle {
        val out = Output(Bool())
    })

    val isGT = RecFNComparer.greaterThan(a.RecFN, b.RecFN)
    io.out := isGT
}

class TestEQ(a: Double, b: Double) extends Module {
    val io = IO(new Bundle {
        val out = Output(Bool())
    })

    val isEQ = RecFNComparer.equal(a.RecFN, b.RecFN)
    io.out := isEQ
}

class TestGTE(a: Double, b: Double) extends Module {
    val io = IO(new Bundle {
        val out = Output(Bool())
    })

    val isGTE = RecFNComparer.greaterThanOrEqual(a.RecFN, b.RecFN)
    io.out := isGTE
}

class TestLTE(a: Double, b: Double) extends Module {
    val io = IO(new Bundle {
        val out = Output(Bool())
    })

    val isLTE = RecFNComparer.lessThanOrEqual(a.RecFN, b.RecFN)
    io.out := isLTE
}


class FloatUtilsTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "recFNFromScalaDouble"

    it should "convert back to the original value" in
    test({
        new BackAndForthConverter(0.5)
    }) { c =>
        val rawFN = c.io.out
        val double = rawFN.peek().toDouble
        assert(double == 0.5)
    }

    behavior of "RecFNAdder"
    it should "add two values" in
    test({
        new TestAdder(0.5, 0.25)
    }) { c =>
        val rawFN = c.io.out
        val double = rawFN.peek().toDouble
        assert(double == 0.75)
    }

    behavior of "RecFNComparer"
    it should "compare a < b when a < b" in
    test({
        new TestLT(0.5, 0.75)
    }) { c =>
        val lt = c.io.out
        assert(lt.peekBoolean() == true)
    }
    it should "fail a < b when a > b" in
    test({
        new TestLT(0.75, 0.5)
    }) { c =>
        val lt = c.io.out
        assert(lt.peekBoolean() == false)
    }
    it should "fail a < b when a = b" in
    test({
        new TestLT(0.5, 0.5)
    }) { c =>
        val lt = c.io.out
        assert(lt.peekBoolean() == false)
    }

    it should "compare a > b when a > b" in
    test({
        new TestGT(0.75, 0.5)
    }) { c =>
        val gt = c.io.out
        assert(gt.peekBoolean() == true)
    }
    it should "fail a > b when a < b" in
    test({
        new TestGT(0.1, 245.3)
    }) { c =>
        val gt = c.io.out
        assert(gt.peekBoolean() == false)
    }
    it should "fail a > b when a = b" in
    test({
        new TestGT(0.25, 0.25)
    }) { c =>
        val gt = c.io.out
        assert(gt.peekBoolean() == false)
    }

    it should "compare a = b when a = b" in
    test({
        new TestEQ(0.25, 0.25)
    }) { c =>
        val eq = c.io.out
        assert(eq.peekBoolean() == true)
    }
    it should "fail a = b when a < b" in
    test({
        new TestEQ(0.1, 0.25)
    }) { c =>
        val eq = c.io.out
        assert(eq.peekBoolean() == false)
    }
    it should "fail a = b when a > b" in
    test({
        new TestEQ(0.25, 0.1)
    }) { c =>
        val eq = c.io.out
        assert(eq.peekBoolean() == false)
    }

    it should "compare a >= b when a > b" in
    test({
        new TestGTE(0.25, 0.1)
    }) { c =>
        val gte = c.io.out
        assert(gte.peekBoolean() == true)
    }
    it should "compare a >= b when a = b" in
    test({
        new TestGTE(0.25, 0.25)
    }) { c =>
        val gte = c.io.out
        assert(gte.peekBoolean() == true)
    }
    it should "fail a >= b when a < b" in
    test({
        new TestGTE(0.1, 0.25)
    }) { c =>
        val gte = c.io.out
        assert(gte.peekBoolean() == false)
    }

    it should "compare a <= b when a < b" in
    test({
        new TestLTE(0.1, 0.25)
    }) { c =>
        val lte = c.io.out
        assert(lte.peekBoolean() == true)
    }
    it should "compare a <= b when a = b" in
    test({
        new TestLTE(0.25, 0.25)
    }) { c =>
        val lte = c.io.out
        assert(lte.peekBoolean() == true)
    }
    it should "fail a <= b when a > b" in
    test({
        new TestLTE(0.25, 0.1)
    }) { c =>
        val lte = c.io.out
        assert(lte.peekBoolean() == false)
    }

    behavior of "BoolToRecFN"
    it should "be 1 and 0 correctly" in
    test({
        new Module {
            val io = IO(new Bundle{
                val true_val = Output(FN())
                val false_val = Output(FN())
            })

            val true_val = BoolToRecFN(true.B)
            val false_val = BoolToRecFN(false.B)

            io.true_val := true_val.FN
            io.false_val := false_val.FN
        }
    }) {
        c =>
        val true_val = c.io.true_val.peek().toDouble
        assert(true_val == 1.0)

        val false_val = c.io.false_val.peek().toDouble
        assert(false_val == 0.0)
    }
}
