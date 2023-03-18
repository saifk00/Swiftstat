package org.swiftstat.chisel.modules.examples

import chisel3._
import chisel3.Vec._
import chisel3.util.random.MaxPeriodGaloisLFSR

class BoolVectorModule(values: Seq[Bool]) extends Module {
    val io = IO(new Bundle{
        val out = Output(Vec(values.size, Bool()))
    })

    io.out := DontCare
    io.out.indices.foreach(i => io.out(i) := values(i))
}

/**
  * Example: what if we want to generate a module that wraps another?
  *         someone else has instantiated a vecModule and now wishes to have a module
  *         that maps its output values using a lookup table.
  *
  *         in this method, we pass the entire module to the constructor
  *
  * @param map
  * @param vecModule
  */
class ExampleGeneratorModule(
    map: Seq[Int],
    // somehow this only works as a named parameter.. what? why?
    vecModule: => BoolVectorModule
    ) extends Module {

    val io = IO(new Bundle {
        val out = Output(Vec(map.size, Bool()))
    })

    io.out := DontCare
    io.out.indices.zip(map).foreach{
        case(outIndex, mapIndex) =>
            io.out(outIndex) := vecModule.io.out(mapIndex) }
}

class ExampleRandomMapperModule(num: Int) extends Module {
    val io = IO(new Bundle{
        val out = Output(Bool())
    })

    val lfsr = Module(new MaxPeriodGaloisLFSR(64))
    lfsr.io.seed := DontCare
    lfsr.io.increment := true.B
    val sample = (0 until num)
        .map(bit => lfsr.io.out(bit))
        .reduce(_ | _)

    io.out := sample
}
