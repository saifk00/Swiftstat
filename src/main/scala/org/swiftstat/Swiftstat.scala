package org.swiftstat

import org.swiftstat.pgm.compiler.files.PGMFile
import org.swiftstat.pgm.compiler.PGMCompilerContext
import scopt.OParser
import java.io.File
import com.typesafe.scalalogging.Logger

case class SwiftstatConfig(
    pgmFileIn: File,
    mdlFileProtoOut: File = new File("out.mdl"),
    mdlFileJsonOut: File = new File("mdl.json"),
    hardwareOut: String = "swiftstat_generated",
    debug: Boolean = false,
)

class CompilerTimer {
    private var t0 = 0L
    var total = 0.0
    def start(msg: String): Unit = {
        t0 = System.nanoTime
        println(msg)
    }
    def stop(): Double = {
        val d = (System.nanoTime - t0) / 1_000_000.0
        total += d
        println(f"Done in $d%.0fms")
        d
    }
}

object Swiftstat extends App {
    val logger = Logger(getClass.getName.stripSuffix("$"))
    val builder = OParser.builder[SwiftstatConfig]
    val argParser = {
        import builder._
        OParser.sequence(
            programName("swiftstat"),
            head("swiftstat", "0.1"),
            opt[File]('i', "pgm")
                .required()
                .action((x, c) => c.copy(pgmFileIn = x))
                .text("PGM file to compile"),
            opt[File]('o', "mdl")
                .action((x, c) => c.copy(mdlFileProtoOut = x))
                .text("MDL file to write"),
            opt[File]('j', "json")
                .action((x, c) => c.copy(mdlFileJsonOut = x))
                .text("JSON file to write"),
            opt[Unit]('d', "debug")
                .action((_, c) => c.copy(debug = true))
                .text("Enable debug mode"),
            help('h', "help").text("Prints this usage text"),
        )
    }

    OParser.parse(argParser, args, SwiftstatConfig(new File(""))) match {
        case Some(config) =>
            logger.debug("Parsed arguments")
            val pgmFile = PGMFile.fromPGMFile(config.pgmFileIn)
            val ctx = new PGMCompilerContext()
            val timer = new CompilerTimer()

            // pgm -> mdl
            timer.start("Compiling PGM to MDL")
                val mdl = ctx.compile(pgmFile)
            val mdlTime = timer.stop()

            mdl.writeProtoToFile(config.mdlFileProtoOut)
            logger.debug(s"Wrote protobuf to ${config.mdlFileProtoOut}")

            mdl.writeJsonToFile(config.mdlFileJsonOut)
            logger.debug(s"Wrote json to ${config.mdlFileJsonOut}")
            println(s"\tjson available at ${config.mdlFileJsonOut}\n\tproto available at ${config.mdlFileProtoOut}")

            // mdl -> mdlEntity
            timer.start("Constructing MDLEntity")
                val mdlEntity = ctx.compile(mdl)
            val mdlEntityTime = timer.stop()

            if (config.debug) {
                println(mdlEntity)
            }

            // mdlEntity -> verilog
            timer.start("Constructing Hardware")
                val ssnEntity = ctx.compile(mdlEntity, config.hardwareOut)
            val hardwareTime = timer.stop()

            println(s"\tgenerated output available in ${config.hardwareOut}/")

            println(f"Completed compilation of PGM ${config.pgmFileIn} in ${timer.total}%.0fms.")
            println(f"${mdlTime},${mdlEntityTime},${hardwareTime}")
        case _ =>
            System.exit(1)
    }
}