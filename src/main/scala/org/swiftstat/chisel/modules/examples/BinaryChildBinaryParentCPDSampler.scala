package org.swiftstat.chisel.modules.examples

import org.swiftstat.pgm.compiler.entity.UserNodeEntity

import chisel3._
import org.swiftstat.chisel.modules.Sample
import org.swiftstat.chisel.modules.RowSampler
import chisel3.util.MuxCase
import org.swiftstat.chisel.modules.SampleSet

/**
  * A mux over a binary child
  */
class BinaryChildBinaryParentCPDSampler extends Module {
    private val parent = UserNodeEntity("A", Seq(), Seq(0D, 1), Seq(0.5, 0.5))
    private val node = UserNodeEntity("B", Seq(parent), Seq(2D, 3), Seq(0.125, 0.875,
                                                                        0.25, 0.75))

    /**
      * The distribution should be: P[B=0] = P[B=0 | A=0]P[A=0] + P[B=0 | A=1]P[A=1]
      *                                    = 0.125*0.5 + 0.25*0.5
      *                                    = 0.1875
      * So we should have
      * P[B=0] = 0.1875
      * P[B=1] = 0.8125
      */

    val io = IO(new Bundle {
      val parentSample = Input(SampleSet(node.parents))
      val output = Output(Sample(node))
    })

    private val rows = node.getRows()

    // for each row, we create the row sampler and keep the association with the parent assignment
    private val rowSamplers = rows.map { case(parentAssignment, cpdRow) => parentAssignment -> Module(RowSampler.fromCPDRow(cpdRow)) }.toMap

    // now, we create a map that tells us the Ordinal for each [[UserNodeValue]] for that parent
    // since there is only one parent, this is a single Map[UserNodeValue, Int] but for many parents it will be
    // Map[UserNodeEntity, Map[UserNodeValue, Int]]
    private val parentOrdinals = parent.getValueOrdinalAssociation().map {case (v, i) => v -> i}.toMap
    val sample = MuxCase(0.U,
      rowSamplers
        .map {
          case (parentAssignment, rowSampler) =>
            // this conditional will have to be changed for multiple parents. That is, the condition is actually
            // for all samples S_i in io.parentSamples, S_i is equal to P_i - the ordinal value (i.e. index)
            // of the value in parentAssignment corresponding to the parent.
            (io.parentSample.samples(0).sample === parentOrdinals(parentAssignment.head).U) -> rowSampler.io.sample }
        .toSeq)

    io.output.sample := sample
}
