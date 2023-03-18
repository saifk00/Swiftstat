package org.swiftstat.chisel.modules

import chisel3._
import chiseltest._
import chiseltest.ChiselScalatestTester
import org.scalatest.flatspec.AnyFlatSpec
import org.swiftstat.pgm.compiler.entity._
import org.swiftstat.pgm.compiler.entity.ast._
import org.swiftstat.chisel.modules.FloatUtils._
import chiseltest.iotesters.PeekPokeTester
import treadle.chronometry.Timer
import org.scalatest.Ignore
import org.swiftstat.mdl.UserNode

class StaticSSN(mdl: MDLEntity, query: Int) extends Module {
    val io = IO(new Bundle {
        val evidence = Input(SampleSet(mdl.userNodes.toSeq))
        val result = Output(FN())
        val currentExpressionValue = Output(FN())
        val curWeight = Output(FN())
    })

    val network = SwiftStatNetwork(mdl, query.U, io.evidence, true)
    io.result := network.io.queryResult

    val queries = mdl.queries.toSeq
    val q = queries(query)
    io.currentExpressionValue := network.io.queryValue(q)
    io.curWeight := network.io.queryWeight(q)
}

/**
  * This test is probably the slowest of all. Feel free to disable (using @Ignore) but ensure it works occasionally
  */
class SwiftStatNetworkTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "SwiftStatNetwork"

    val nA = UserNodeEntity("A", Seq(), Seq(0D, 1), Seq(0.03125, 0.96875))
    val nB = UserNodeEntity("B", Seq(), Seq(0D, 1), Seq(0.5, 0.5))
    val nC = UserNodeEntity("C", Seq(nA, nB), Seq(0D, 1), Seq(0.5, 0.5,         // A0B0
                                                            0.25, 0.75,         // A0B1
                                                            0.125, 0.875,       // A1B0
                                                            0.1875, 0.8125))    // A1B1
    val queryEAPlusBGivenC1 = QueryEntity(
        "test",
        EpsilonDeltaSpecification(0.5, 0.5),
        QueryExpressionBinaryOp(
            QueryExpressionNodeOp(nA),
            QueryExpressionNodeOp(nB),
            QueryBinaryOpTypes.`+`
        ),
        Set(),
        QueryTypes.Expectation
    )

    it should "correctly compute a single query" in
        test({
            val mdl = MDLEntity.fromUserNodes(nA, nB, nC)
            val mdlWithQuery = mdl.copy(queries = Set(queryEAPlusBGivenC1))
            new StaticSSN(mdlWithQuery, 0)
        }).withAnnotations(Seq(VerilatorBackendAnnotation))
        {
            c =>
            // c.io.evidence.samplesByName.foreach{case (name, sample) => sample.sample.poke(0.U)}
            c.clock.setTimeout(0)
            var lastValue = 0.0
            val softwareAverage = new SoftwareRunningWeightedAverage()
            val iters = 250_000
            for (i <- 0 until iters) {
                c.clock.step(23)

                // track the debug weight/values so we can compute the weighted average in software
                // there will be a slight discrepancy because hardware is (1) doing it in RecFN and (2) doing it with either the previous
                // or next sample, not the 23rd one i think
                val qdbWeight = c.io.curWeight.peek().toDouble
                val qdbValue = c.io.currentExpressionValue.peek().toDouble
                softwareAverage.update(qdbWeight, qdbValue)

                if (i % 10_000 == 0) {
                    // val value = c.io.result.peek().toDouble

                    val value = softwareAverage.get()
                    val delta = Math.abs((lastValue - value) / lastValue) * 100.0
                    val deltaPct = "%.5f".format(delta).toDouble

                    println(s"current value: ${lastValue} (Δ=${deltaPct}%)   Completion: ${100.0 * i / iters} %")
                    lastValue = value
                }
            }

            println(s"Software average computed: ${softwareAverage.get()}")
            assert(lastValue === 1.46875 +- 0.01)

            // no evidence:
            // E[A+B] = P[A1B0] + P[A0B1] + 2P[A1B1]
            //        = 0.96875*0.5 + 0.03125*0.5 + 2*0.96875*0.5
            //        = 1.46875

            // If evidence is included:
            // P[C1] = 0.5*( 0.5*0.03125 + 0.75*0.03125 + 0.875*0.96875+0.8125*0.96875 ) = 0.83691406
            // E[A+B | C1] = P[A1B0 | C1] + P[A0B1 | C1] + 2P[A1B1 | C1]
            //             = (P[C1|A1B0]*P[A1B0] + P[C1|A0B1]*P[A0B1] + 2P[C1|A1B1]*P[A1B1]) / P[C1]
            //             = (0.875*0.5*0.96875  + 0.75*0.03125*0.5 + 2*0.8125*0.96875*0.5) / 0.83691406
            //             = 1.22265625 / 0.83691406
            //             = 1.46091016
        }

}