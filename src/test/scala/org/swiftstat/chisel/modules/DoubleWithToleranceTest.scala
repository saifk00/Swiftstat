package org.swiftstat.chisel.modules

import org.scalatest.flatspec.AnyFlatSpec

class DoubleWithToleranceTest extends AnyFlatSpec {
  it should "be right" in {
    assert(0.5 +- 0.2 === 0.3 +- 0.5)
    assert(0.5 +- 0.2 === 0.5)
    assert(0.5 === 0.5 +- 0.01)
    assert(0.5 !=== 0.6 +- 0.01)
    assert(0.5 !=== 0.6)
    assert(0.6 +- 0.01 !=== 0.5)
  }
}
