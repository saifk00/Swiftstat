package org.swiftstat.pgm.compiler.files

import org.antlr.v4.runtime.CharStream
import java.io.File
import java.io.FileInputStream
import org.swiftstat.mdl._
import java.io.FileOutputStream
import scalapb.json4s.Printer
import org.json4s.jackson.Serialization.writePretty
import scalapb.json4s.JsonFormat
import org.json4s.DefaultFormats


class MDLFile private (proto: PGMProto) {
    def getProto() = proto
    def writeProtoToFile(file: File): Unit = {
        val stream = new FileOutputStream(file)
        stream.write(proto.toByteArray)
    }
    def writeProtoToFileName(fileName: String): Unit = {
        val file = new File(fileName)
        writeProtoToFile(file)
    }

    def writeJsonToFile(file: File): Unit = {
        val stream = new FileOutputStream(file)
        var json = (new Printer)
            .includingDefaultValueFields
            .toJson(proto)
        implicit val jsonFormats = DefaultFormats
        val prettyJson = writePretty(json)
        stream.write(prettyJson.getBytes)
    }

    def writeJsonToFileName(fileName: String): Unit = {
        val file = new File(fileName)
        writeJsonToFile(file)
    }
}

object MDLFile {
    def fromProto(proto: PGMProto): MDLFile = {
        new MDLFile(proto)
    }

    def fromProtoFile(file: File): MDLFile = {
        val stream = new FileInputStream(file)
        val data = scala.io.Source.fromInputStream(stream).mkString
        val proto = PGMProto.parseFrom(data.getBytes)
        MDLFile.fromProto(proto)
    }

    def fromProtoFileName(fileName: String): MDLFile = {
        val file = new File(fileName)
        MDLFile.fromProtoFile(file)
    }
}