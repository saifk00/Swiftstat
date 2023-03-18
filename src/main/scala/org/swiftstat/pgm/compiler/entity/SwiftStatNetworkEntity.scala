package org.swiftstat.pgm.compiler.entity

import chisel3.stage.ChiselStage
import org.swiftstat.chisel.modules.SwiftStatNetwork
import java.io.File
import java.io.FileOutputStream

class SwiftStatNetworkEntity(mdl: MDLEntity, dir: String) {
    val verilog = (new ChiselStage).emitVerilog(
        new SwiftStatNetwork(mdl, false),
        Array("--target-dir", dir))
}

object SwiftStatNetworkEntity {
    def fromMDLEntity(mdl: MDLEntity, dir: String): SwiftStatNetworkEntity = new SwiftStatNetworkEntity(mdl, dir)
}