package org.swiftstat.chisel.modules

import chisel3._
import chisel3.util.random.{LFSR, MaxPeriodGaloisLFSR, MaxPeriodFibonacciLFSR}
import org.swiftstat.chisel.ddg._
import org.swiftstat.chisel.ddg.BitSequences._
import org.swiftstat.pgm.compiler.entity.CPDRowEntity
import scala.util.Random
import chisel3.util.Valid
import com.typesafe.scalalogging.Logger


class RowSampler(bitPrefixes: Map[Int, Set[Prefix]]) extends Module {
    private val numBits = bitPrefixes.size
    require(bitPrefixes.keys.toList.sorted == (0 until numBits))
    private val maximumPrefixLength = bitPrefixes.values.flatMap(v => v.maxBy(_.size)).max
    // TODO(skhattak) instead, use the minimum value in LFSR.tapsMaxPeriod that is >= maximumPrefixLength
    private val lfsrWidth = Math.max(maximumPrefixLength, 64)
    private val lfsr = Module(new MaxPeriodGaloisLFSR(lfsrWidth))
    // randomly fill lfsr.io.seed
    private val seed = VecInit(Seq.fill(lfsrWidth)(Random.nextBoolean().B))
    lfsr.io.seed.bits := seed
    lfsr.io.seed.valid := RegNext(RegInit(false.B), true.B)
    lfsr.io.increment := true.B
    val sample = VecInit(
        (0 until numBits)
        .map(bit =>
            bitPrefixes(bit)
                .map(prefix => prefix
                        .zipWithIndex
                        .map { case(value, index) =>
                            value match {
                                case 0 => ~lfsr.io.out(index)
                                case 1 => lfsr.io.out(index)
                                case _ => throw new Exception("Invalid value in prefix")}}
                        // this prefix is active if we get an exact match on the LFSR
                        .reduce(_ & _))
                // This bit is active if any of the prefixes are active
                .reduce(_ | _)
        ))

    val io = IO(Sample.fromNumBits(numBits))

    io := sample.asUInt
}

object RowSampler {
    val defaultMaxPrecision = 64
    val logger = Logger[RowSampler]

    def prefixSetMapperToString(indexToPrefixes: Map[Int, Set[Prefix]]): String = indexToPrefixes.map {
            case (bit, prefixes) => s"b${bit} = " + prefixes.map(_.mkString).mkString(" | ")
        }.mkString("\n")

    def fromCPDRow(row: CPDRowEntity): RowSampler = {
        logger.debug(s"Constructing RowSampler for a row with ${row.distribution.size} values")
        val minimalDDG = MinimalDDG.fromCPDRowEntity(row, defaultMaxPrecision)
        val bitPrefixSets = minimalDDG.bitPrefixSets
        val bitIndexToPrefixSet = bitPrefixSets.prefixes
            .zipWithIndex
            .map { case (prefixSet, index) => index -> prefixSet.toSet }
            .toMap

        logger.debug(s"Constructed RowSampler for a row with ${row.distribution.size} values:\n${prefixSetMapperToString(bitIndexToPrefixSet)}")

        // set the name to something that lets us identify the distribution being sampled
        // this will probably be too much for large distributions but fine for now
        // TODO(skhattak) read the above and shorten this
        new RowSampler(bitIndexToPrefixSet) {
            override def desiredName: String = s"RowSampler_${row.distribution.map(_.toString()).reduce(_ + "_" + _)}"
        }
    }
}
