package org.swiftstat.chisel.modules

import chisel3._
import hardfloat.recFNFromFN
import org.swiftstat.util.sig
import org.swiftstat.util.exp
import Chisel.INPUT
import hardfloat.RawFloat

object FloatUtils {
    // 65.W
    type RecFN = UInt
    // 64.W
    type FN = Bits

    def FN() = Bits(64.W)
    def RecFN() = UInt(65.W)

    implicit class RecFNDoubleUtils(value: Double) {
        /**
          * Converts a static double to a chisel RecFN to be used with the berkeley hardfloat
          * computation modules
          */
        def RecFN: RecFN = StaticDoubleToRecFNConverter(value)
    }

    implicit class RecFNFNUtils(value: RecFN) {
        /**
          * Converts a recFN to a 64-bit IEEE754 format
          *
          * @return
          */
        def FN: FN = RecFNToFNConverter(value)
    }

    implicit class FNDoubleUtils(value: FN) {
        /**
          * Unfortunately we can't directly convert RecFN to Double since that involves generating hardware,
          * and basic idea is that the 'highest level' floating point representation we have is
          *     in hardware: FN
          *     in software: Double
          *
          * thus tests should create modules that convert everything to FN (IEE754), then use this
          * to compare it to software Doubles
          *
          */
        def toDouble: Double = java.lang.Double.longBitsToDouble(value.litValue.longValue)
    }

    implicit class FNBoolUtils(value: Bool) {
        def RecFN: RecFN = BoolToRecFN(value)
    }


    class StaticDoubleToRecFNConverter(value: Double) extends Module {
        val io = IO(new Bundle {
            val out = Output(RecFN())
        })

        val longBits = java.lang.Double.doubleToLongBits(value)
        val bits = WireInit(longBits.U(64.W))

        io.out := recFNFromFN(exp(64), sig(64), bits)
    }

    object StaticDoubleToRecFNConverter {
        def apply(value: Double): UInt = Module(new StaticDoubleToRecFNConverter(value)).io.out
    }

    class BoolToRecFN extends Module {

        val io = IO(new Bundle {
            val in = Input(Bool())
            val out = Output(RecFN())
        })

        val one = 1.RecFN
        val zero = 0.RecFN

        io.out := Mux(io.in, one, zero)
    }
    object BoolToRecFN {
        def apply(value: Bool): RecFN = {
            val module = Module(new BoolToRecFN)
            module.io.in := value

            module.io.out
        }
    }

    class RecFNToFNConverter extends Module {
        val io = IO(new Bundle {
            val in = Input(RecFN())
            val out = Output(Bits(64.W))
        })

        io.out := hardfloat.fNFromRecFN(exp(64), sig(64), io.in)
    }

    object RecFNToFNConverter {
        def apply(value: UInt): Bits = {
            val module = Module(new RecFNToFNConverter)
            module.io.in := value

            module.io.out
        }
    }

    class RecFNComparer extends Module {
        val io = IO(new Bundle {
            val a = Input(RecFN())
            val b = Input(RecFN())
            val lt = Output(Bool())
            val eq = Output(Bool())
            val gt = Output(Bool())
        })

        val comparer = Module(new hardfloat.CompareRecFN(exp(64), sig(64)))

        comparer.io.a := io.a
        comparer.io.b := io.b
        comparer.io.signaling := false.B

        io.lt := comparer.io.lt
        io.eq := comparer.io.eq
        io.gt := comparer.io.gt
    }

    object RecFNComparer {
        def apply(a: UInt, b: UInt): RecFNComparer = {
            val comparer = Module(new RecFNComparer)

            comparer.io.a := a
            comparer.io.b := b

            comparer
        }

        def lessThan(a: UInt, b: UInt): Bool = RecFNComparer(a, b).io.lt
        def greaterThan(a: UInt, b: UInt): Bool = RecFNComparer(a, b).io.gt
        def equal(a: UInt, b: UInt): Bool = RecFNComparer(a, b).io.eq
        def greaterThanOrEqual(a: UInt, b: UInt): Bool = !RecFNComparer(a, b).io.lt
        def lessThanOrEqual(a: UInt, b: UInt): Bool = !RecFNComparer(a, b).io.gt
    }

    class AddRecFN extends Module {
        val io = IO(new Bundle {
            val a = Input(RecFN())
            val b = Input(RecFN())
            val out = Output(RecFN())
        })

        val adder = Module(new hardfloat.AddRecFN(exp(64), sig(64)))

        adder.io.a := io.a
        adder.io.b := io.b
        adder.io.roundingMode := 0.U
        adder.io.detectTininess := 0.U
        adder.io.subOp := false.B

        io.out := adder.io.out
    }

    object AddRecFN {
        def apply(a: UInt, b: UInt): UInt = {
            val adder = Module(new AddRecFN)

            adder.io.a := a
            adder.io.b := b

            adder.io.out
        }
    }

    class SubRecFN extends Module {
        val io = IO(new Bundle {
            val a = Input(RecFN())
            val b = Input(RecFN())
            val out = Output(RecFN())
        })

        val subber = Module(new hardfloat.AddRecFN(exp(64), sig(64)))

        subber.io.a := io.a
        subber.io.b := io.b
        subber.io.roundingMode := 0.U
        subber.io.detectTininess := 0.U
        subber.io.subOp := true.B

        io.out := subber.io.out
    }

    object SubRecFN {
        def apply(a: UInt, b: UInt): UInt = {
            val subber = Module(new SubRecFN)

            subber.io.a := a
            subber.io.b := b

            subber.io.out
        }
    }


    class MultRecFN extends Module {
        val io = IO(new Bundle {
            val a = Input(RecFN())
            val b = Input(RecFN())
            val out = Output(RecFN())
        })

        val multiplier = Module(new hardfloat.MulRecFN(exp(64), sig(64)))

        multiplier.io.a := io.a
        multiplier.io.b := io.b
        multiplier.io.roundingMode := 0.U
        multiplier.io.detectTininess := 0.U

        io.out := multiplier.io.out
    }

    object MultRecFN {
        def apply(a: UInt, b: UInt): UInt = {
            val mul = Module(new MultRecFN)

            mul.io.a := a
            mul.io.b := b

            mul.io.out
        }
    }

    /**
      * TODO(skhattak) i have no idea how to use this module correctly
      *
      * from the docs, looks like we need a pipeline depth of 6
      * since the next div operation is available at most 6 cycles later
      */
    class DivRecFN extends Module {
        val io = IO(new Bundle {
            val a = Input(RecFN())
            val b = Input(RecFN())
            val out = Output(RecFN())
        })

        val divider = Module(new hardfloat.DivSqrtRecF64)


        divider.io.inValid := !(io.b === 0.RecFN)
        divider.io.sqrtOp := false.B
        divider.io.a := io.a
        divider.io.b := io.b
        divider.io.roundingMode := 0.U
        divider.io.detectTininess := 0.U

        io.out := divider.io.out
    }

    object DivRecFN {
        def apply(a: UInt, b: UInt): UInt = {
            val mul = Module(new DivRecFN)

            mul.io.a := a
            mul.io.b := b

            mul.io.out
        }
    }
}
