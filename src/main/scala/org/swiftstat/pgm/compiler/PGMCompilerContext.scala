package org.swiftstat.pgm.compiler

import org.swiftstat.pgm.visitors.MDLVisitor
import org.swiftstat.pgm.compiler.files._
import org.swiftstat.pgm.compiler.entity._
import org.swiftstat.chisel.modules.SwiftStatNetwork
import chisel3.stage.ChiselStage

class PGMCompilerContext {
    def compile(pgmFile: PGMFile): MDLFile = {
        val tree = pgmFile.compile()
        val mdlProto = MDLVisitor(tree)

        MDLFile.fromProto(mdlProto)
    }

    def compile(mdlFile: MDLFile): MDLEntity = {
        MDLEntity.fromProto(mdlFile.getProto())
    }

    def compile(mdl: MDLEntity, dir: String): SwiftStatNetworkEntity = SwiftStatNetworkEntity.fromMDLEntity(mdl, dir)
}
