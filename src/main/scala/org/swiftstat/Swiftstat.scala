package org.swiftstat

import org.swiftstat.pgm.compiler.files.PGMFile
import org.swiftstat.pgm.compiler.PGMCompilerContext
import scopt.OParser
import java.io.File
import com.typesafe.scalalogging.Logger
import java.nio.file.Files
import java.nio.file.Paths

case class SwiftstatConfig(
    pgmFileIn: File,
    mdlProtoName: String = "out.mdl",
    mdlJsonName: String = "out.json",
    hardwareOut: String = "out.swiftstat",
    debug: Boolean = false) {

        def mdlProtoFile: File = new File(s"${hardwareOut}/${mdlProtoName}")
        def mdlJsonFile: File = new File(s"${hardwareOut}/${mdlJsonName}")
}

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
    // TODO: grab the name from the execution environment somehow.
    //       cant use args(0) since args is technically argv[1:]
    val progName = "scc"
    val argParser = {
        import builder._
        OParser.sequence(
            programName(progName),
            head(s"${progName}: The Swiftstat PGM Compiler"),
            opt[File]('i', "pgm")
                .required()
                .action((x, c) => {
                    c.copy(pgmFileIn = x)
                     .copy(hardwareOut = s"${x.getName().stripSuffix(".pgm")}.swiftstat")
                })
                .text("PGM file to compile"),
            opt[String]('o', "mdl")
                .action((x, c) => c.copy(mdlProtoName = x))
                .text("MDL file to write"),
            opt[String]('j', "json")
                .action((x, c) => c.copy(mdlJsonName = x))
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

            // set up the directory for compile output
            val compileDir = Paths.get(config.hardwareOut)
            if (!Files.exists(compileDir)) {
                Files.createDirectory(compileDir)
            } else {
                logger.warn(s"Compile directory ${compileDir} already exists, overwriting some files")
            }


            mdl.writeProtoToFile(config.mdlProtoFile)
            logger.debug(s"Wrote protobuf to ${config.mdlProtoName}")

            mdl.writeJsonToFile(config.mdlJsonFile)
            logger.debug(s"Wrote json to ${config.mdlJsonName}")
            println(s"\tjson available at ${config.mdlJsonName}\n\tproto available at ${config.mdlProtoName}")

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