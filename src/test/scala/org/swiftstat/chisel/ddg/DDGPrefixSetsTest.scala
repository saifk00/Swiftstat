package org.swiftstat.chisel.ddg

import org.scalatest.flatspec.AnyFlatSpec
import org.swiftstat.chisel.ddg.BitSequences._
import scala.collection.mutable


class DDGPrefixSetsTest extends AnyFlatSpec {
    behavior of "getValuePrefixes"

    it should "handle a 50-50" in {
        val result = DDGPrefixSets.getValuePrefixes(
            Branch(
                Terminal(0),
                Terminal(1)
            )
        )

        assert(result.size == 2)
        assert(result(0) == Set(
            Seq(0)
        ))
        assert(result(1) == Set(
            Seq(1)
        ))
    }

    it should "handle a 25-75" in {
        val result = DDGPrefixSets.getValuePrefixes(
            Branch(
                Branch(
                    Terminal(0),
                    Terminal(0)
                ),
                Terminal(1)
            )
        )

        assert(result.size == 2)
        assert(result(0) == Set(
            Seq(0, 0),
            Seq(0, 1)
        ))
        assert(result(1) == Set(
            Seq(1)
        ))
    }

    behavior of "makeBitPrefixes"

    it should "handle a 50-50" in {
        val result = DDGPrefixSets.makeBitPrefixes(
            mutable.Map[Int, mutable.Set[Prefix]](
                0 -> mutable.Set(Seq(0)),
                1 -> mutable.Set(Seq(1))
            )
        )

        assert(result.size == 1)
        assert(result(0) == mutable.Set(Seq(1)))
    }

    it should "handle a 25-25-25-25" in {
        val result = DDGPrefixSets.makeBitPrefixes(
            mutable.Map[Int, mutable.Set[Prefix]](
                0 -> mutable.Set(Seq(0, 0)), // 0b00
                1 -> mutable.Set(Seq(0, 1)), // 0b01
                2 -> mutable.Set(Seq(1, 0)), // 0b10
                3 -> mutable.Set(Seq(1, 1))  // 0b11
            )
        )

        assert(result.size == 2)
        assert(result(0) == mutable.Set(Seq(0, 1), Seq(1, 1)))
        assert(result(1) == mutable.Set(Seq(1, 0), Seq(1, 1)))
    }
}
