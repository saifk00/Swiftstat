package org.swiftstat.pgm.compiler.files

import org.antlr.v4.runtime.CharStream
import org.swiftstat.pgm.antlr.PGMFileLexer
import org.antlr.v4.runtime.CommonTokenStream
import org.swiftstat.pgm.antlr.PGMFileParser
import org.antlr.v4.runtime.CharStreams
import scala.io.BufferedSource
import os.FileType
import java.io.InputStream
import java.io.FileInputStream
import java.io.File
import org.antlr.v4.runtime.misc.Interval

/**
  * Contains all the data of a PGM file
  */
class PGMFile private (text: String) {
    /**
      * Get the parse tree for the PGM file.
      */
    def compile(): PGMFileParser.PgmContext = {
        // Internally, we use CharStreams because that's what ANTLR uses.
        // to provide a generic interface to the factory methods, we use InputStream
        val stream = CharStreams.fromString(text)

        val lexer = new PGMFileLexer(stream)
        val tokenStream = new CommonTokenStream(lexer)
        val parser = new PGMFileParser(tokenStream)

        parser.pgm()
    }

    def getText(): String = text
}

/**
  * Factory methods for PGMFile.
  */
object PGMFile {
    def fromText(text: String): PGMFile = {
        new PGMFile(text)
    }

    def fromPGMFile(file: File): PGMFile = {
        val stream = new FileInputStream(file)
        val data = scala.io.Source.fromInputStream(stream).mkString

        PGMFile.fromText(data)
    }

    def fromPGMFileName(fileName: String): PGMFile = {
        val file = new File(fileName)
        PGMFile.fromPGMFile(file)
    }
}