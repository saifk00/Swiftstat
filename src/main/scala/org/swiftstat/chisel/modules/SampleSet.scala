package org.swiftstat.chisel.modules

import chisel3._
import org.swiftstat.pgm.compiler.entity.UserNodeEntity
import chisel3.util.MixedVec
import org.swiftstat.pgm.compiler.entity.UserNodeValue

/**
  * Represents a set of samples as a bundle in hardware
  * Internally, this is represented as a MixedVec of samples.
  * This class contains convenience functions for accessing the sample
  * by name or index; these should be preferred over using rawValues
  * @param nodes
  */
class SampleSet(nodes: Seq[UserNodeEntity]) extends Bundle {
    // unfortunately this needs to be public for Chisel
    // but it allows us to make a neat API - see the other methods of this class
    // we also cannot prefix this with special symbols because of how chisel
    // generates hardware signal names from the scala variable name
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

    // allows using the syntax sampleset(node) to get the sample
    def apply(n: UserNodeEntity) = samplesByName(n.name)
    def apply(n: UserNodeValue) = samplesByName(n.name)
}

object SampleSet {
    def apply(nodes: Seq[UserNodeEntity]): SampleSet = new SampleSet(nodes)
}