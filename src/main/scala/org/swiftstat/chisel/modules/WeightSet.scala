package org.swiftstat.chisel.modules

import chisel3._
import org.swiftstat.pgm.compiler.entity.UserNodeEntity
import chisel3.util.MixedVec

/**
  * Weights are basically the same as values, except instead of
  * representing the value of a node, they represent the weight
  * from observed values to be used in likelihood weighting.
  *
  */
class WeightSet(nodes: Seq[UserNodeEntity]) extends ValueSet(nodes);

object WeightSet {
    def apply(nodes: Seq[UserNodeEntity]): WeightSet = new WeightSet(nodes)
}