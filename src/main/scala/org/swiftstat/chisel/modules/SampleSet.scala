package org.swiftstat.chisel.modules

import chisel3._
import org.swiftstat.pgm.compiler.entity.UserNodeEntity
import chisel3.util.MixedVec


class SampleSet(nodes: Seq[UserNodeEntity]) extends Bundle {
    // unfortunately this needs to be public for Chisel
    // but it allows us to make a neat API - see the other methods of this class
    val rawValues = MixedVec(nodes.map(Sample(_)))

    val samples = nodes
        .zipWithIndex
        .map { case (node, index) =>
            index -> rawValues(index)}
        .toMap

    val samplesByName = nodes
        .zipWithIndex
        .map { case (node, index) =>
            node.name -> samples(index) }
        .toMap

    // val samplesByNode = nodes
    //     .zipWithIndex
    //     .map { case (node, index) =>
    //         node -> samples(index) }
    //     .toMap
}

object SampleSet {
    def apply(nodes: Seq[UserNodeEntity]): SampleSet = new SampleSet(nodes)
}