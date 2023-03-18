package org.swiftstat.chisel.modules

import chisel3._
import org.swiftstat.pgm.compiler.entity.UserNodeEntity
import chisel3.util.MixedVec
import org.swiftstat.chisel.modules.FloatUtils._

class ValueSet(nodes: Seq[UserNodeEntity]) extends Bundle {
    // unfortunately this needs to be public for Chisel
    // but it allows us to make a neat API - see the other methods of this class
    // uses the RecFN format, hence the 65.W
    val rawValues = Vec(nodes.size, RecFN())

    val values = nodes
        .zipWithIndex
        .map { case (node, index) =>
            index -> rawValues(index)}
        .toMap

    val valuesByName = nodes
        .zipWithIndex
        .map { case (node, index) =>
            node.name -> values(index) }
        .toMap
}

object ValueSet {
    def apply(nodes: Seq[UserNodeEntity]): ValueSet = {
        new ValueSet(nodes)
    }
}