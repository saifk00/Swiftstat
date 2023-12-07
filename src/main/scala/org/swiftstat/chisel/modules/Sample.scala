package org.swiftstat.chisel.modules

import chisel3._
import org.swiftstat.pgm.compiler.entity.UserNodeEntity
import org.swiftstat.pgm.compiler.entity.UserNodeValue
import chisel3.util.MixedVec

/**
  * WARNING: DO NOT USE THIS CONSTRUCTOR DIRECTLY
  *
  * you should always use [[Sample.fromMaxValue]] or [[Sample.fromNumBits]] instead
  *
  * @param numBits
  */
class Sample (numBits: Int) extends Bundle {
    val sample = Output(UInt(numBits.W))
}

class StaticSample(numBits: Int, value: UInt) extends Module {
    val io = IO(Output(UInt(numBits.W)))
    io := value
}

object StaticSample {
    def apply(numBits: Int, value: UInt): UInt = Module(new StaticSample(numBits, value)).io
    def apply(node: UserNodeEntity, value: UserNodeValue): UInt = {
        val numBits = (Math.floor(Math.log(node.getMaxOrdinal()) / Math.log(2)) + 1).toInt
        val sampleValue = node.getValueOrdinal(value)

        apply(numBits, sampleValue.U)
    }
}

object Sample {
    def fromNumBits(numBits: Int): UInt = UInt(numBits.W)

    /**
      * Construct a Sample from a user node entity
      * i.e. a bundle which can output a sample corresponding to an ordinal for the node
      *
      * @param node the node to sample
      * @return a [[Sample]] bundle
      */
    def apply(node: UserNodeEntity): UInt = {
      val maxValue = node.getMaxOrdinal()
      val numBits = (Math.floor(Math.log(maxValue) / Math.log(2)) + 1).toInt
      UInt(numBits.W)
    }

}