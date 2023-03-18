package org.swiftstat.chisel.ddg

import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.immutable.BitSet
import org.swiftstat.pgm.compiler.entity.CPDRowEntity
import scala.collection.mutable


class MinimalDDGTest extends AnyFlatSpec {
    behavior of "fromCPDRow"
    it should "handle a simple 50-50 CPD row" in {
        val minDDG = MinimalDDG.fromCPDRowEntity(
            CPDRowEntity(Seq(0.5, 0.5)),
            2)

        assert(minDDG.probabilityTable == Seq(
            Seq(0, 1),
            Seq(0, 1)))
        assert(minDDG.treeRootNode == Branch(
            Terminal(0),
            Terminal(1)))
        assert(minDDG.bitPrefixSets == DDGPrefixSets(
            Seq(mutable.Set(Seq(1)))
        ))
    }

    it should "handle a simple 25-25-25-25 CPD row" in {
        val minDDG = MinimalDDG.fromCPDRowEntity(
            CPDRowEntity(Seq(0.25, 0.25, 0.25, 0.25)),
            4)

        assert(minDDG.probabilityTable == Seq(
            Seq(0, 0, 1),
            Seq(0, 0, 1),
            Seq(0, 0, 1),
            Seq(0, 0, 1)))
        assert(minDDG.treeRootNode == Branch(
            Branch(
                Terminal(0),
                Terminal(1)
            ),
            Branch(
                Terminal(2),
                Terminal(3)
            )))
        assert(minDDG.bitPrefixSets == DDGPrefixSets(
            Seq
            (
                mutable.Set(Seq(0, 1), Seq(1, 1)),
                mutable.Set(Seq(1, 0), Seq(1, 1))
            )
        ))
    }

    behavior of "convertToFixedPoint"

    it should "handle a valid fixed point with matched precision" in {
        val fixed = MinimalDDG.convertToFixedPoint(0.5, 2)
        // 0.5 = 0b01
        //          ^- 2^-1

        assert(fixed == Seq(0, 1))
    }

    it should "handle 1" in {
        val fixed = MinimalDDG.convertToFixedPoint(1.0, 1)

        assert(fixed == Seq(1))
    }

    it should "handle 0b011" in {
        val fixed = MinimalDDG.convertToFixedPoint(0.75, 3)

        assert(fixed == Seq(0, 1, 1))
    }

    it should "cut off 0b011 correctly" in {
        val fixed = MinimalDDG.convertToFixedPoint(0.75, 2)

        assert(fixed == Seq(0, 1))
    }

    it should "handle 1.75 correctly" in {
        val fixed = MinimalDDG.convertToFixedPoint(1.75, 3)

        assert(fixed == Seq(1, 1, 1))
    }

    it should "terminate early when overspecified" in {
        val fixed = MinimalDDG.convertToFixedPoint(0.75, 6)

        assert(fixed == Seq(0, 1, 1))
    }

    val `100%`  = Seq(1)
    val `50%`   = Seq(0, 1)
    val `25%`   = Seq(0, 0, 1)
    val `12.5%` = Seq(0, 0, 0, 1)
    val `6.25%` = Seq(0, 0, 0, 0, 1)
    val `3.125%`= Seq(0, 0, 0, 0, 0, 1)

    def makeTree(probs: Seq[Int]*) = MinimalDDG.makeTree(Seq(probs: _*))

    behavior of "makeTree"
    it should "create a 100% tree" in {
        val root = makeTree(`100%`)

        assert(root == Terminal(0))
    }
    it should "create a 50-50 tree" in {
        val root = makeTree(`50%`, `50%`)

        assert(root == Branch(
            Terminal(0),
            Terminal(1)
        ))
    }
    it should "create a 50-25-25 tree" in {
        val root = makeTree(`50%`, `25%`, `25%`)

        assert(root == Branch(
            Terminal(0),
            Branch(
                Terminal(1),
                Terminal(2)
            )
        ))
    }
    it should "create a complex tree" in {
        val root = makeTree(
            `50%`,
            `6.25%`,
            `6.25%`,
            `6.25%`,
            `3.125%`,
            `3.125%`,
            `25%`
        )

        assert(root == Branch(
            Terminal(0),
            Branch(
                Terminal(6),
                Branch(
                    Branch(
                        Terminal(1),
                        Terminal(2)
                    ),
                    Branch(
                        Terminal(3),
                        Branch(
                            Terminal(4),
                            Terminal(5)
                        )
                    )
                )
            )
        ))
    }
    it should "handle values with row entries" in {
        val root = makeTree(
            Seq(0, 0, 1, 1, 0, 0, 1),
            Seq(0, 0, 0, 0, 0, 0, 1),
            Seq(0, 1),
            Seq(0, 0, 0, 0, 1, 1),
        )

        assert(root == Branch(
            Terminal(2),
            Branch(
                Terminal(0),
                Branch(
                    Terminal(0),
                    Branch(
                        Terminal(3),
                        Branch(
                            Terminal(3),
                            Branch(
                                Terminal(0),
                                Terminal(1))))))))
    }

    behavior of "BranchNodeEquality"
    it should "be reflexive" in {
        val a = Branch(Terminal(0), Terminal(1))
        val b = Branch(Terminal(0), Terminal(1))

        assert(a == b)
    }
    it should "allow swapping order" in {
        val a = Branch(Terminal(0), Terminal(1))
        val b = Branch(Terminal(1), Terminal(0))

        assert(a == b)
    }
    it should "recursively check equality" in {
        val a = Branch(
            Terminal(0),
            Branch(
                Terminal(1),
                Terminal(2)
            )
        )
        val b = Branch(
            Branch(
                Terminal(2),
                Terminal(1)
            ),
            Terminal(0)
        )

        assert(a == b)
    }
}
