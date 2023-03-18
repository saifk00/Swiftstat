package org.swiftstat.chisel

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.swiftstat.pgm.compiler.PGMCompilerContext
import org.swiftstat.pgm.compiler.files._
import org.swiftstat.chisel.modules._
import org.swiftstat.chisel.modules.FloatUtils._
import org.scalatest.Ignore
import org.swiftstat.pgm.compiler.entity.MDLEntity

class ModelToSSNTestBase extends AnyFlatSpec with ChiselScalatestTester {
    def compile(filename: String): MDLEntity = {
        val ctx = new PGMCompilerContext()
        val pgm: PGMFile = PGMFile.fromPGMFileName(filename)

        ctx.compile(ctx.compile(pgm))
    }

    def computeQuery(mdl: MDLEntity, queryIndex: Int, evidence: Seq[(String, Int)], shouldPrint: Boolean)(implicit c: SwiftStatNetwork): (Double, Int) = {
        c.clock.setTimeout(0)

        // set the query
        val query = mdl.queries.toSeq(queryIndex)
        c.io.currentQueryID.poke(queryIndex.U)

        // set the evidence
        evidence.foreach { case(name, value) => c.io.elements(name).poke(value.U) }

        val evaluator = new SoftwareRunningWeightedAverage()

        while (!evaluator.done()) {
            c.clock.step()

            val weight = c.io.queryWeight(query).peek().toDouble
            val value = c.io.queryValue(query).peek().toDouble

            evaluator.update(weight, value)

            if (evaluator.count % 10_000 == 0 && shouldPrint) {
                println(s"${evaluator.get()} (${evaluator.count}) delta=${evaluator.delta}")
            }
        }

        (evaluator.get(), evaluator.count)
    }
}

class ModelToSSNTest extends ModelToSSNTestBase {
    val student2 = compile("models/student2.pgm")

    behavior of "SwiftStat Compiler"
    it should "compile student2 and simulate it correctly (Query 1)" in
    test({
        new SwiftStatNetwork(student2, false)
    }).withAnnotations(Seq(VerilatorBackendAnnotation)) {
        c =>
        implicit val network = c

        def evaluate(queryIndex: Int, evidence: Seq[(String, Int)]) = computeQuery(student2, queryIndex, evidence, false)

        // E[G | I0, D1] = 1*0.96875 + 2*0.015625 + 3*0.015625 = 1.046875
        val EGi0d1 = evaluate(1, Seq(("Intelligence", 0)))._1
        println(s"Computed E[G | I0, D1] = ${EGi0d1}")
        assert(EGi0d1 === 1.046875 +- 0.01)

        // now lets switch up the evidence. what grade will they get if I1?
        // E[G | I1, D1] = 1*0.3125 + 2*0.375 + 3*0.3125 = 2.0
        val EGi1d1 = evaluate(1, Seq(("Intelligence", 1)))._1
        println(s"Computed E[G | I1, D1] = ${EGi1d1}")
        assert(EGi1d1 === 2.0 +- 0.01)


        // How about query 0? P[A + L = 2 | Grade]
        // this is asking the probability they are admitted
        // and receive a referral letter, given their grade

        // P[A+L=2 | G0] = P[A1L1|G0] = P[A1|G0]*P[L1|G0] = 0.03125*0.03125 = 0.00097656
        val PAandLGivenG0 = evaluate(0, Seq(("Grade", 0)))._1
        println(s"Computed P[A+L=2| G0] = ${PAandLGivenG0}")
        assert(PAandLGivenG0 === 0.00097656 +- 0.01)

        // now what if they have a really high grade?
        // P[A+L=2 | G2] = P[A1L1|G2] = P[A1|G2]*P[L1|G2] = 0.875 * 0.25 = 0.21875
        val PAandLGivenG2 = evaluate(0, Seq(("Grade", 2)))._1
        println(s"Computed P[A+L=2| G2] = ${PAandLGivenG2}")
        assert(PAandLGivenG2 === 0.21875 +- 0.01)

        // some common sense facts should be true:
        // students with higher grades are more likely to get admitted and receive letters
        assert(PAandLGivenG2 > PAandLGivenG0)
        // students with higher intelligence should have a higher grade given the same difficulty
        assert(EGi1d1 > EGi0d1)
    }
}

class DynamicSSNTest extends ModelToSSNTestBase {
    val dynamic1 = compile("models/dynamic_1.pgm")
    it should "compile dynamic1 and simulate it correctly (Query 1)" in
    test({
        new SwiftStatNetwork(dynamic1, false)
    }).withAnnotations(Seq(VerilatorBackendAnnotation)) {
        c =>
            implicit val network = c

            def evaluate(queryIndex: Int, evidence: Seq[(String, Int)]) = computeQuery(dynamic1, queryIndex, evidence, false)

            val EA0 = evaluate(0, Seq(("A0", 0)))
            val EA1 = evaluate(0, Seq(("A0", 1)))

            println(s"Computed E[300*B1 + C1 | A0=0] = ${EA0._1}")
            println(s"Computed E[300*B1 + C1 | A0=1] = ${EA1._1}")

            assert(EA0._1 === 608 +- 5.0)
            assert(EA1._1 === 575 +- 5.0)

            println(s"dynamic1 a0=${EA0._2}, a1=${EA1._2}")
    }

    val dynamic16 = compile("models/dynamic_16.pgm")
    it should "compile dynamic16 and simulate it correctly (Query 1)" in
    test({
        new SwiftStatNetwork(dynamic16, false)
    }).withAnnotations(Seq(VerilatorBackendAnnotation)) {
        c =>
            implicit val network = c

            def evaluate(queryIndex: Int, evidence: Seq[(String, Int)]) = computeQuery(dynamic16, queryIndex, evidence, false)

            val EA0 = evaluate(0, Seq(("A0", 0)))
            val EA1 = evaluate(0, Seq(("A0", 1)))

            println(s"Computed E[300*B16 + C16 | A0=0] = ${EA0._1}")
            println(s"Computed E[300*B16 + C16 | A0=1] = ${EA1._1}")

            assert(EA0._1 === 580 +- 5.0)
            assert(EA1._1 === 580 +- 5.0)

            println(s"dynamic16 a0=${EA0._2}, a1=${EA1._2}")
    }
}

/**
 * Tests how many cycles it takes to converge for the various dynamic[...].pgm test
 */
@Ignore
class DynamicModelConvergenceTests extends ModelToSSNTestBase {
    def convergenceTest(filename: String): TestResult = {
        val pgm = compile(filename)

        test({
            new SwiftStatNetwork(pgm, false)
        }).withAnnotations(Seq(VerilatorBackendAnnotation)) {
            c =>
                implicit val network = c

                def evaluate(queryIndex: Int, evidence: Seq[(String, Int)]) = computeQuery(pgm, queryIndex, evidence, false)

                val EA0 = evaluate(0, Seq(("A0", 0)))
                val EA1 = evaluate(0, Seq(("A0", 1)))

                println(s"${filename} a0=${EA0._2}, a1=${EA1._2}")
        }
    }

    "dynamic1" should "converge" in
    convergenceTest("models/dynamic_1.pgm")

    "dynamic2" should "converge" in
    convergenceTest("models/dynamic_2.pgm")

    "dynamic4" should "converge" in
    convergenceTest("models/dynamic_4.pgm")

    "dynamic8" should "converge" in
    convergenceTest("models/dynamic_8.pgm")

    "dynamic16" should "converge" in
    convergenceTest("models/dynamic_16.pgm")

    "dynamic32" should "converge" in
    convergenceTest("models/dynamic_32.pgm")

    "dynamic64" should "converge" in
    convergenceTest("models/dynamic_64.pgm")

    for (i <- Seq(2, 4, 8, 16, 32)) {
        s"dynamic32_${i}" should "converge" in
        convergenceTest(s"models/dynamic_32_query_${i}.pgm")
    }
}