package org.swiftstat.chisel.modules

import chisel3._

import org.swiftstat.pgm.compiler.entity._
import org.swiftstat.util.MapUtils
import chisel3.util.MixedVec
import chisel3.util.MixedVecInit
import scala.annotation.tailrec
import chisel3.util.ShiftRegister
import chisel3.util._

case class ParentCPDSampler(sample: Sample, depth: Int)

/**
  * Constructs a network of CPDSamplers. Provides a single output port containing
  * all the current samples in the network.
  *
  *
  * @param mdl the MDL containing all the user nodes to sample from
  */
class SamplerNetwork(mdl: MDLEntity) extends Module {
  val nodes = mdl.userNodes.toSeq

  // the canonical numbered set of queries
  val orderedQueries: Seq[QueryEntity] = mdl.queries.toSeq
  val nodeToQueriesItIsEvidenceIn: Map[UserNodeEntity, Seq[QueryEntity]] = nodes
    .map(node => node -> orderedQueries.filter(_.evidence.exists(_.node == node)))
    .toMap
  val staticQueryIDs = MixedVecInit(orderedQueries.indices.map(i => WireInit(i.U)))

  val io = IO(new Bundle {
      val currentQueryID = Input(UInt())
      val evidence = Input(SampleSet(nodes))
      val samples = Output(SampleSet(nodes))
  })

  /**
   * Recursively constructs samplers. assumes that the `remaining` list is sorted in reverse topological order.
   * Inserts shift registers to delay signals to accomodate for different parent depths
   */
  @tailrec
  private def constructCPDSamplers(
        remaining: Seq[UserNodeEntity],
        already: Map[UserNodeEntity, ParentCPDSampler]): Map[UserNodeEntity, ParentCPDSampler] = remaining match {
      case Nil => already
      case node :: tail => {
        val sampler = Module(new CPDSampler(node))

        val maxParentDepth = node.parents
          .map(parent =>
            already(parent).depth)
          .maxOption
          .getOrElse(0)

        // connect all the parent samplers' outputs to the current sampler's inputs
        for (parent <- node.parents) {
          val parentCPDSampler = already(parent)

          // may need to delay a certain branch
          val parentSample = (maxParentDepth - parentCPDSampler.depth) match {
            case 0 => parentCPDSampler.sample
            case n => ShiftRegister(parentCPDSampler.sample, n)
          }

          sampler.io.parents.samplesByName(parent.name) := RegNext(parentSample)
        }

        // override samples that are evidence
        val sample = nodeToQueriesItIsEvidenceIn(node) match {
          case Nil => sampler.io.sample
          case queriesItIsEvidenceIn => MuxCase(sampler.io.sample, queriesItIsEvidenceIn.map(query => {
            val queryID = orderedQueries.indexOf(query)
            val queryIDBits = staticQueryIDs(queryID)

            val evidence = query.evidence.find(_.node == node).get
            val sampleIfEvidence = evidence match {
              case RunTimeEvidenceEntity(_) => io.evidence.samplesByName(node.name)
              case CompileTimeEvidenceEntity(_, value) => StaticSample(node, value)
            }

            (io.currentQueryID === queryIDBits) -> sampleIfEvidence
          }))
        }

        constructCPDSamplers(tail, already + (node -> ParentCPDSampler(sample, maxParentDepth + 1)))
      }
    }

  val adjacency = nodes
    .map(node => node -> node.parents)
    .toMap
  val parentsBeforeChildren = adjacency.sortReverseTopological

  val cpdSamplers = constructCPDSamplers(parentsBeforeChildren, Map.empty)

  // connect the outputs of parent samplers to overall network outputs
  for {
      (parent, parentCPDSampler) <- cpdSamplers
      parentSample = parentCPDSampler.sample
    } {
    io.samples.samplesByName(parent.name) := parentSample
  }
}

object SamplerNetwork {
  def apply(mdl: MDLEntity, evidence: SampleSet, currentQueryID: UInt): SamplerNetwork = {
    val samplerNetwork = Module(new SamplerNetwork(mdl))
    samplerNetwork.io.evidence := evidence
    samplerNetwork.io.currentQueryID := currentQueryID
    samplerNetwork
  }

  /**
    * Used for network with no queries (probably just a test)
    *
    * @param mdl
    * @return
    */
  def apply(mdl: MDLEntity): SamplerNetwork = {
    val samplerNetwork = Module(new SamplerNetwork(mdl))

    // hacks to get it to work even though these arent used
    // ideally we would never have a sampler network with no queries but this is useful
    // for testing distributions
    samplerNetwork.io.currentQueryID := 0.U
    samplerNetwork.io.evidence.samplesByName.foreach{ case(name, sample) => {
      val node = mdl.userNodes.find(_.name == name).get
      sample := StaticSample(node, node.values.head)
    }}

    samplerNetwork
  }
}
