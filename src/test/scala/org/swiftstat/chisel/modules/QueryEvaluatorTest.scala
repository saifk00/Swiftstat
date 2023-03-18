package org.swiftstat.chisel.modules

import chisel3._
import chiseltest._
import chiseltest.ChiselScalatestTester
import chisel3.util.Counter
import org.scalatest.flatspec.AnyFlatSpec
import org.swiftstat.pgm.compiler.entity.UserNodeEntity

import hardfloat._
import org.swiftstat.util.exp
import org.swiftstat.util.sig
import org.swiftstat.pgm.compiler.entity.QueryEntity
import org.swiftstat.pgm.compiler.entity.EpsilonDeltaSpecification
import org.swiftstat.pgm.compiler.entity.ast.QueryExpressionNodeOp
import org.swiftstat.pgm.compiler.entity.ast.QueryExpressionBinaryOp
import org.swiftstat.pgm.compiler.entity.ast.QueryBinaryOpTypes
import org.swiftstat.pgm.compiler.entity.QueryTypes
import org.swiftstat.mdl.UserNode
import org.swiftstat.chisel.modules.FloatUtils._
import scala.runtime.Static
import org.swiftstat.pgm.compiler.entity.ast.QueryExpressionConstOp
import org.swiftstat.pgm.compiler.entity._
import chisel3.util.ShiftRegister
import org.scalatest.Ignore

/**
  * Wires in static sample values to a QueryExpressionEvaluator
  *
  * @param query the query to compute
  * @param nodes the set of nodes involved
  * @param values the values to set for each node involved in the query
  */
class DummyQueryEvaluator(query: QueryEntity, nodes: Seq[UserNodeEntity]) extends Module {
    val io = IO(new Bundle {
        val samples = Input(SampleSet(nodes))
        val currentWeightedAverage = Output(FN())
    })

    val nodeValues = ValueMapper(nodes, io.samples)

    // convert the output to double
    io.currentWeightedAverage := QueryEvaluator(nodes, query, io.samples, nodeValues, true).io.currentWeightedAverage.FN
}

class ConstRATest(weight: Seq[Double], value: Seq[Double]) extends Module {
    assert(weight.length == value.length, "weight and value must be the same length")

    val io = IO(new Bundle{
        val average = Output(FN())

        val Wt = Output(FN())
        val Et = Output(FN())
        val St = Output(FN())
        val Sigmat = Output(FN())
        val WtPlusSigmat = Output(FN())
        val WtEt = Output(FN())
        val StSigmat = Output(FN())
        val EtNumerator = Output(FN())
        val EtDenominator = Output(FN())
        val EtNext = Output(FN())
        val WtNext = Output(FN())
        val enableUpdate = Output(Bool())
        val cnt = Output(UInt(5.W))
    })

    // shift each value in by one
    val rW = RegInit(0.RecFN)
    val rV = RegInit(0.RecFN)

    // in each cycle N, rW should be weight[N].RecFN
    // and rV should be value[N].RecFN
    val (ctr, _) = Counter(true.B, weight.length)
    val weights = VecInit(weight.map(_.RecFN))
    val values = VecInit(value.map(_.RecFN))

    rW := weights(ctr)
    rV := values(ctr)

    val rwa = Module(new RunningWeightedAverage(rW, rV))
    rwa.io.curWeight := rW
    rwa.io.curValue := rV
    io.average := rwa.io.currentWeightedAverage.FN

    // Need to convert debug outputs to FN so they can be examined as Double in tests
    io.Wt := rwa.io.inputs.Wt.FN
    io.Et := rwa.io.inputs.Et.FN
    io.St := rwa.io.inputs.St.FN
    io.Sigmat := rwa.io.inputs.Sigmat.FN
    io.WtPlusSigmat := rwa.io.WtPlusSigmat.FN
    io.WtEt := rwa.io.WtEt.FN
    io.StSigmat := rwa.io.StSigmat.FN
    io.EtNumerator := rwa.io.EtNumerator.FN
    io.EtDenominator := rwa.io.EtDenominator.FN
    io.EtNext := rwa.io.EtNext.FN
    io.WtNext := rwa.io.WtNext.FN
    io.enableUpdate := rwa.io.enableUpdate
    io.cnt := rwa.io.cnt
}

class RunningWeightedAverageTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "RunningWeightedAverage"
    it should "compute the unweighted average of 3 values" in
    test({
        val weight = Seq.fill(23 + 23 + 23 + 1)(1.0)
        val value =
            Seq.fill(1)(2.0) ++
            Seq.fill(22)(10000.0) ++
            Seq.fill(1)(10.0)++
            Seq.fill(22)(10000.0) ++
            Seq.fill(1)(3.0) ++
            Seq.fill(22)(10000.0) ++
            Seq.fill(1)(1.0)

        // it should compute the average as (2.0 + 10.0 + 3.0)/3.0 = 5.0 since each of these values appears
        // exactly on the 23rd cycle
        new ConstRATest(weight, value)
    }).withAnnotations(Seq(VerilatorBackendAnnotation)) {
        c =>
        var cycleinfo = new scala.collection.mutable.ListBuffer[String]()
        val printDebug = false
        val steps = 23 + 23 + 23 + 1
        if (printDebug) {
            println("===Debug===")
            for (i <- 0 until steps) {
                c.clock.step()

                cycleinfo.addOne(s"""
                    | ===Cycle ${i}===
                    |Wt:        ${c.io.Wt.peek().toDouble}
                    |Sigmat:    ${c.io.Sigmat.peek().toDouble}
                    |   WtPlusSigmat: ${c.io.WtPlusSigmat.peek().toDouble}
                    |
                    |Wt:        ${c.io.Wt.peek().toDouble}
                    |Et:        ${c.io.Et.peek().toDouble}
                    |   WtEt:      ${c.io.WtEt.peek().toDouble}
                    |
                    |Sigmat:    ${c.io.Sigmat.peek().toDouble}
                    |St:        ${c.io.St.peek().toDouble}
                    |   StSigmat:  ${c.io.StSigmat.peek().toDouble}
                    |
                    |       EtNumerator:    ${c.io.EtNumerator.peek().toDouble}
                    |       EtDenominator:  ${c.io.EtDenominator.peek().toDouble}
                    |
                    |           EtNext:         ${c.io.EtNext.peek().toDouble}
                    |           WtNext:         ${c.io.WtNext.peek().toDouble}
                    |           enableUpdate:   ${c.io.enableUpdate.peek().litToBoolean}
                    |           cnt:            ${c.io.cnt.peek().litValue}
                """.stripMargin)
            }

            // check if output.txt exists and delete it if so
            val file = new java.io.File("output.txt")
            if (file.exists()) {
                file.delete()
            }

            val pw = new java.io.PrintWriter(new java.io.File("output.txt"))
            pw.write(cycleinfo.mkString("\n"))
            pw.close()
        } else {
            c.clock.setTimeout(0)
            c.clock.step(steps)
        }

        assert(c.io.average.peek().toDouble == 5.0)
    }

    it should "compute the weighted average of 3 values" in
    test({
        val weight =
            Seq.fill(1)(0.5) ++
            Seq.fill(22)(0.0) ++
            Seq.fill(1)(0.5)++
            Seq.fill(22)(0.0) ++
            Seq.fill(1)(1.0) ++
            Seq.fill(22)(0.0) ++
            Seq.fill(1)(0.0)
        val value =
            Seq.fill(1)(2.0) ++
            Seq.fill(22)(10000.0) ++
            Seq.fill(1)(10.0)++
            Seq.fill(22)(10000.0) ++
            Seq.fill(1)(3.0) ++
            Seq.fill(22)(10000.0) ++
            Seq.fill(1)(1.0)

        // it should compute the average as (0.5*2.0 + 0.5*10.0 + 1.0*3.0)/(0.5+0.5+1.0) = 4.5 since each of these values appears
        // exactly on the 23rd cycle
        new ConstRATest(weight, value)
    }).withAnnotations(Seq(VerilatorBackendAnnotation)) {
        c =>
        var cycleinfo = new scala.collection.mutable.ListBuffer[String]()
        val printDebug = false
        val steps = 23 + 23 + 23 + 1
        if (printDebug) {
            println("===Debug===")
            for (i <- 0 until steps) {
                c.clock.step()

                cycleinfo.addOne(s"""
                    | ===Cycle ${i}===
                    |Wt:        ${c.io.Wt.peek().toDouble}
                    |Sigmat:    ${c.io.Sigmat.peek().toDouble}
                    |   WtPlusSigmat: ${c.io.WtPlusSigmat.peek().toDouble}
                    |
                    |Wt:        ${c.io.Wt.peek().toDouble}
                    |Et:        ${c.io.Et.peek().toDouble}
                    |   WtEt:      ${c.io.WtEt.peek().toDouble}
                    |
                    |Sigmat:    ${c.io.Sigmat.peek().toDouble}
                    |St:        ${c.io.St.peek().toDouble}
                    |   StSigmat:  ${c.io.StSigmat.peek().toDouble}
                    |
                    |       EtNumerator:    ${c.io.EtNumerator.peek().toDouble}
                    |       EtDenominator:  ${c.io.EtDenominator.peek().toDouble}
                    |
                    |           EtNext:         ${c.io.EtNext.peek().toDouble}
                    |           WtNext:         ${c.io.WtNext.peek().toDouble}
                    |           enableUpdate:   ${c.io.enableUpdate.peek().litToBoolean}
                    |           cnt:            ${c.io.cnt.peek().litValue}
                """.stripMargin)
            }

            // check if output.txt exists and delete it if so
            val file = new java.io.File("output.txt")
            if (file.exists()) {
                file.delete()
            }

            val pw = new java.io.PrintWriter(new java.io.File("output_weighted.txt"))
            pw.write(cycleinfo.mkString("\n"))
            pw.close()
        } else {
            c.clock.setTimeout(0)
            c.clock.step(steps)
        }

        assert(c.io.average.peek().toDouble == 4.5)
    }
}

class QueryEvaluatorTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "QueryEvaluator"

    val a = UserNodeEntity("A", Seq(), Seq[Double](1, 2), Seq(0.1, 0.9))
    val b = UserNodeEntity("B", Seq(a), Seq[Double](1, 2, 3), Seq(0.1, 0.2, 0.7,
                                                                  0.5, 0.4, 0.1))
    val c = UserNodeEntity("C", Seq(a, b), Seq[Double](1, 2,50), Seq(0.1, 0.2, 0.7,
                                                                  0.5, 0.4, 0.1,
                                                                  0.2, 0.5, 0.3,
                                                                  0.1, 0.2, 0.7,
                                                                  0.2, 0.4, 0.4,
                                                                  0.4, 0.2, 0.4))
    val nodes = Seq(a, b, c)
    val queryEA_plus_BGivenC1 = QueryEntity(
        "test",
        EpsilonDeltaSpecification(0.5, 0.5),
        QueryExpressionBinaryOp(
            QueryExpressionNodeOp(a),
            QueryExpressionNodeOp(b),
            QueryBinaryOpTypes.`+`
        ),
        Set(CompileTimeEvidenceEntity(c, c.values(2))),
        QueryTypes.Expectation
    )

    it should "compute the right average when operands are fixed" in
    test({
        new DummyQueryEvaluator(queryEA_plus_BGivenC1, nodes)
    }).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
        c.clock.setTimeout(0)

        // run 23 cycles for each value we want to add to the average
        c.io.samples.samplesByName("A").sample.poke(1.U)
        c.io.samples.samplesByName("B").sample.poke(1.U)

        // A[1] + B[1] = 2.0 + 2.0 = 4.0
        // W = C.cols[2][A1B1] = 0.4
        c.clock.step(23)

        c.io.samples.samplesByName("A").sample.poke(0.U)
        c.io.samples.samplesByName("B").sample.poke(1.U)
        // A[0] + B[1] = 1.0 + 2.0 = 3.0
        // W = C.cols[2][A0B1] = 0.1
        c.clock.step(23)

        // one final step to propagate
        c.clock.step()
        val expected = (0.4 * 4.0 + 0.1 * 3.0) / (0.4 + 0.1)

        assert(c.io.currentWeightedAverage.peek().toDouble == expected)
    }


}
