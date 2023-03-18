package org.swiftstat.chisel.modules

import chisel3._

/**
 * Example Module
 *
 * this is simply an adder to show how to use chisel
 */

class ExampleModule extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(8.W))
    val b = Input(UInt(8.W))
    val out = Output(UInt(8.W))
  })

  val tmp = RegInit(0.U(8.W))
  tmp := io.a + io.b
  io.out := tmp
}
