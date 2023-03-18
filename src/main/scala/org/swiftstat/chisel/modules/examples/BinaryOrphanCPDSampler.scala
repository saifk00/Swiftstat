package org.swiftstat.chisel.modules.examples

import org.swiftstat.pgm.compiler.entity.UserNodeEntity

import chisel3._
import org.swiftstat.chisel.modules.Sample
import org.swiftstat.chisel.modules.RowSampler

/**
  * Basically just a wrapper around a rowSampler
  */
class BinaryOrphanCPDSampler extends Module {
    private val node = UserNodeEntity("A", Seq(), Seq(0D, 1), Seq(0.5, 0.5))
    val io = IO(Sample(node))

    private val row = node.getRows().head._2
    private val rowSampler = Module(RowSampler.fromCPDRow(row))
    io.sample := rowSampler.io.sample
}
