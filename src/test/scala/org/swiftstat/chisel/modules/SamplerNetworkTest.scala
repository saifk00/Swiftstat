package org.swiftstat.chisel.modules

import chisel3._
import chiseltest._
import chiseltest.ChiselScalatestTester
import org.scalatest.flatspec.AnyFlatSpec
import org.swiftstat.pgm.compiler.entity._
import org.swiftstat.mdl.UserNode
import chisel3.stage.ChiselStage
import chisel3.util.MixedVec
import chisel3.util.MixedVecInit
import scala.annotation.tailrec
import org.swiftstat.pgm.compiler.entity.ast._

class StaticQuerySampleNetwork(mdl: MDLEntity, query: UInt) extends Module {
    val io = IO(new Bundle {
        val evidence = Input(SampleSet(mdl.userNodes.toSeq))
        val samples = Output(SampleSet(mdl.userNodes.toSeq))
    })

    val network = SamplerNetwork(mdl, io.evidence, query)
    io.samples := network.io.samples
}

class NoQuerySampleNetwork(mdl: MDLEntity) extends Module {
    val io = IO(new Bundle {
        val samples = Output(SampleSet(mdl.userNodes.toSeq))
    })

    val network = SamplerNetwork(mdl)
    io.samples := network.io.samples
}

class SamplerNetworkTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "SamplerNetwork"

    type FrequencyMap = Map[String, Map[Int, Int]]
    type DistributionMap = Map[String, Map[Int, Double]]

    @tailrec
    final def step(remaining: Int, counts: FrequencyMap)(implicit c: NoQuerySampleNetwork): FrequencyMap =
        remaining match {
            case 0 => counts
            case _ =>
                c.clock.step()
                val newCounts = counts.map {
                    case (name, count) =>
                        val sample = c.io.samples
                            .samplesByName(name)
                            .peekInt()
                            .toInt
                        name -> count.updated(sample, count(sample) + 1)
                }
                step(remaining - 1, newCounts)
        }

    def distribution(counts: FrequencyMap): DistributionMap = {
        val totals = counts.map(_._2.values.sum.toDouble)
        val measuredDistribution = counts.map { dist =>
            val total = dist._2.values.sum.toDouble
            dist._1 -> dist._2.map { case (k, v) => k -> v / total }
        }

        measuredDistribution
    }

    val nA = UserNodeEntity("A", Seq(), Seq(0D, 1), Seq(0.03125, 0.96875))
    val nB = UserNodeEntity("B", Seq(), Seq(0D, 1), Seq(0.5, 0.5))
    val nC = UserNodeEntity("C", Seq(nA, nB), Seq(0D, 1), Seq(0.5, 0.5,
                                                            0.25, 0.75,
                                                            0.125, 0.875,
                                                            0.1875, 0.8125))

    it should "sample a joint distribution" in
        test ({
            val mdl = MDLEntity.fromUserNodes(nA, nB, nC)

            new NoQuerySampleNetwork(mdl)
        }) {
            c =>
            // set c as the implicit network to use for utility functions
            implicit val network = c

            // some sanity checks
            assert(c.io.samples.samplesByName.size == 3)
            assert(c.network.nodes == Seq(nA, nB, nC))
            assert(c.network.parentsBeforeChildren.map(_.name) == Seq("A", "B", "C"))

            c.clock.setTimeout(0)
            val frequencies = step(1000, Map("A" -> Map(0 -> 0, 1 -> 0),
                                                "B" -> Map(0 -> 0, 1 -> 0),
                                                "C" -> Map(0 -> 0, 1 -> 0)))
            val measuredDistribution = distribution(frequencies)

            // CALCULATION:
            // P[C=1] = (0.5*0.03125 + 0.75*0.03125 + 0.875*0.96875+0.8125*0.96875 )* 0.5 = 0.83691406
            // P[C=0] = 1 - P[C=1] = 0.16308594

            assert(measuredDistribution("C")(0) === 0.16308594 +- 0.03)
        }

    it should "correctly sample when compile-time evidence is present in queries" in
        test ({
            val mdl = MDLEntity.fromUserNodes(nA, nB, nC)
            val query = QueryEntity(
                "test",
                EpsilonDeltaSpecification(0.5, 0.5),
                QueryExpressionBinaryOp(
                    QueryExpressionNodeOp(nA),
                    QueryExpressionNodeOp(nB),
                    QueryBinaryOpTypes.`+`
                ),
                Set(CompileTimeEvidenceEntity(nC, nC.values(1))),
                QueryTypes.Expectation
            )

            // copy mdl with the query added
            val mdlWithQuery = mdl.copy(queries = Set(query))
            new StaticQuerySampleNetwork(mdlWithQuery, 0.U)
        }) {
            c =>
            implicit val network = c

            // should fix the C sample to 1
            for (i <- 0 until 100) {
                c.clock.step()
                val sample = c.io.samples.samplesByName("C").peekInt().toInt
                assert(sample == 1)
            }
        }

    it should "correctly sample when run-time evidence is present in queries" in
    test ({
        val mdl = MDLEntity.fromUserNodes(nA, nB, nC)
        val query = QueryEntity(
            "test",
            EpsilonDeltaSpecification(0.5, 0.5),
            QueryExpressionBinaryOp(
                QueryExpressionNodeOp(nA),
                QueryExpressionNodeOp(nB),
                QueryBinaryOpTypes.`+`
            ),
            Set(RunTimeEvidenceEntity(nC)),
            QueryTypes.Expectation
        )

        // copy mdl with the query added
        val mdlWithQuery = mdl.copy(queries = Set(query))
        new StaticQuerySampleNetwork(mdlWithQuery, 0.U)
    }) {
        c =>
        implicit val network = c


        // should fix the C sample to 1
        c.clock.step()
        c.io.evidence.samplesByName("C").poke(1.U)
        var sample = c.io.samples.samplesByName("C").peekInt().toInt
        assert(sample == 1)

        // should fix the C sample to 0
        c.clock.step()
        c.io.evidence.samplesByName("C").poke(0.U)
        sample = c.io.samples.samplesByName("C").peekInt().toInt
        assert(sample == 0)
    }
}