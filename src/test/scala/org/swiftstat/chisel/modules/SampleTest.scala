package org.swiftstat.chisel.modules

import chisel3._
import chiseltest._
import chiseltest.ChiselScalatestTester
import org.scalatest.flatspec.AnyFlatSpec
import org.swiftstat.pgm.compiler.entity.UserNodeEntity

class SampleTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "SampleTest"

    it should "get the correct width for a 2-value node" in
    {
        val node = UserNodeEntity("A", Seq(), Seq(0D, 1), Seq(0.5, 0.5))
        val sample = Sample(node)

        assert(sample.getWidth == 1)
    }

    it should "get the correct width for a 4-value node" in
    {
        val node = UserNodeEntity("A", Seq(), Seq(0D, 1, 223, 44), Seq(0.25, 0.25, 0.25, 0.25))
        val sample = Sample(node)

        assert(sample.getWidth == 2)
    }

    it should "get the correct width for a 6-value node" in
    {
        val node = UserNodeEntity("A", Seq(), Seq(0D, 1, 2, 3, 4, 5), Seq.fill(6)(1/6D))
        val sample = Sample(node)

        assert(sample.getWidth == 3)
    }



}
