package org.swiftstat.chisel.modules

import chisel3._
import org.swiftstat.pgm.compiler.entity.UserNodeEntity
import org.swiftstat.pgm.compiler.entity.UserNodeValue

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
    val io = IO(new Sample(numBits))
    io.sample := value
}

object StaticSample {
    def apply(numBits: Int, value: UInt): Sample = Module(new StaticSample(numBits, value)).io
    def apply(node: UserNodeEntity, value: UserNodeValue): Sample = {
        val numBits = (Math.floor(Math.log(node.getMaxOrdinal()) / Math.log(2)) + 1).toInt
        val sampleValue = node.getValueOrdinal(value)

        apply(numBits, sampleValue.U)
    }
}

object Sample {
    def fromNumBits(numBits: Int): Sample = new Sample(numBits)

    /**
      * Construct a Sample from a user node entity
      * i.e. a bundle which can output a sample corresponding to an ordinal for the node
      *
      * @param node the node to sample
      * @return a [[Sample]] bundle
      */
    def apply(node: UserNodeEntity): Sample = {
      val maxValue = node.getMaxOrdinal()
      val numBits = (Math.floor(Math.log(maxValue) / Math.log(2)) + 1).toInt
      new Sample(numBits)
    }
}