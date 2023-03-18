package org.swiftstat.pgm.compiler.entity

import org.scalatest.flatspec.AnyFlatSpec

class UserNodeEntityTest extends AnyFlatSpec {
    behavior of "UserNodeEntitySpec"

    it should "construct a node with no parents" in {
        val a = UserNodeEntity("A", Seq(), Seq(0D, 1), Seq(0.5, 0.5))
        assert(a.name == "A")
        assert(a.parents == Seq())
        assert(a.values == Seq(UserNodeValue("A", 0D), UserNodeValue("A", 1D)))
        assert(a.cpd == CPDEntity(Seq(), Seq(UserNodeValue("A", 0D), UserNodeValue("A", 1D)), Seq(0.5, 0.5)))
    }

    it should "construct a node with two parents" in {
        val a = UserNodeEntity("A", Seq(), Seq(0D, 1), Seq(0.5, 0.5))
        val b = UserNodeEntity("B", Seq(), Seq(2D, 3), Seq(0.3, 0.7))
        val d = UserNodeEntity("D", Seq(a, b), Seq(6D, 7), Seq(0.2, 0.8,
                                                0.1, 0.9,
                                                0.4, 0.6,
                                                0.3, 0.7))
        assert(d.name == "D")
        assert(d.parents == Seq(a, b))
        assert(d.values == Seq(UserNodeValue("D", 6D), UserNodeValue("D", 7D)))
        assert(d.cpd == CPDEntity(Seq(a, b), d.values, Seq(0.2, 0.8,
                                                    0.1, 0.9,
                                                    0.4, 0.6,
                                                    0.3, 0.7)))
    }

    it should "fail to construct a node with the wrong number of values" in {
        val a = UserNodeEntity("A", Seq(), Seq(0D, 1), Seq(0.5, 0.5))
        assertThrows[IllegalArgumentException] {
            val b = UserNodeEntity("B", Seq(a), Seq(2D, 3), Seq(0.3, 0.7))
        }
    }
}

class UserNodeEntityHelpersTest extends AnyFlatSpec {
    behavior of "getValueOrdinalAssociation"
    it should "map a node with one value correctly" in {
        val a = UserNodeEntity("A", Seq(), Seq(1D), Seq(1.0))

        val actual = a.getValueOrdinalAssociation()
        assert(actual.size == 1)
        assert(actual.head._1 == UserNodeValue("A", 1D))
        assert(actual.head._2 == 0)
    }
    it should "map a node with two values correctly" in {
        val b = UserNodeEntity("B", Seq(), Seq(2D, 3), Seq(0.3, 0.7))

        val actual = b.getValueOrdinalAssociation()
        assert(actual.size == 2)
        assert(actual.find(assoc => {
            assoc._1 == UserNodeValue("B", 2D) &&
            assoc._2 == 0
        }).isDefined)
        assert(actual.find(assoc => {
            assoc._1 == UserNodeValue("B", 3) &&
            assoc._2 == 1
        }).isDefined)
    }

    behavior of "getRows"
    it should "get one row for a node with no parents" in {
        val a = UserNodeEntity("A", Seq(), Seq(1D, 2D), Seq(0.5, 0.5))

        val actual = a.getRows()

        assert(actual.size == 1)
        assert(actual.head._1 == Set[UserNodeValue]())
        assert(actual.head._2 == CPDRowEntity(Seq(0.5, 0.5)))
    }

    it should "get two rows for a node with a binary parent" in {
        val a = UserNodeEntity("A", Seq(), Seq(1D, 2D), Seq(0.5, 0.5))
        val b = UserNodeEntity("B", Seq(a), Seq(2D, 3), Seq(0.3, 0.7,
                                                            0.1, 0.9))

        val actual = b.getRows()

        assert(actual.size == 2)
        assert(actual.find(row =>
            row._1 == Set(UserNodeValue("A", 1D)) &&
            row._2 == CPDRowEntity(Seq(0.3, 0.7))
        ).isDefined)
        assert(actual.find(row =>
            row._1 == Set(UserNodeValue("A", 2D)) &&
            row._2 == CPDRowEntity(Seq(0.1, 0.9))
        ).isDefined)
    }

    behavior of "getColumns"
    it should "get two columns for binary orphan" in {
        val a = UserNodeEntity("A", Seq(), Seq(1D, 2D), Seq(0.1, 0.9))

        val actual = a.getColumns()

        assert(actual.size == 2)
        assert(actual(a.values(0)) == Seq((Set(), 0.1)))
        assert(actual(a.values(1)) == Seq((Set(), 0.9)))
    }

    it should "get two columns for a node with a binary parent" in {
        val a = UserNodeEntity("A", Seq(), Seq(1D, 2D), Seq(0.5, 0.5))
        val b = UserNodeEntity("B", Seq(a), Seq(2D, 3), Seq(0.3, 0.7,
                                                            0.1, 0.9))

        val actual = b.getColumns()

        assert(actual.size == 2)
        assert(actual(b.values(0)) == Seq(
            (Set(a.values(0)), 0.3),
            (Set(a.values(1)), 0.1)))
        assert(actual(b.values(1)) == Seq(
            (Set(a.values(0)), 0.7),
            (Set(a.values(1)), 0.9)))
    }

    it should "get two columns for a node with two binary parents" in {
        val a = UserNodeEntity("A", Seq(), Seq(1D, 2D), Seq(0.5, 0.5))
        val b = UserNodeEntity("B", Seq(), Seq(1D, 2D), Seq(0.5, 0.5))
        val c = UserNodeEntity("C", Seq(a, b), Seq(2D, 3), Seq(0.3, 0.7,
                                                                0.1, 0.9,
                                                                0.4, 0.6,
                                                                0.2, 0.8))

        val actual = c.getColumns()

        assert(actual.size == 2)
        assert(actual(c.values(0)) == Seq(
            (Set(a.values(0), b.values(0)), 0.3),
            (Set(a.values(0), b.values(1)), 0.1),
            (Set(a.values(1), b.values(0)), 0.4),
            (Set(a.values(1), b.values(1)), 0.2)))
        assert(actual(c.values(1)) == Seq(
            (Set(a.values(0), b.values(0)), 0.7),
            (Set(a.values(0), b.values(1)), 0.9),
            (Set(a.values(1), b.values(0)), 0.6),
            (Set(a.values(1), b.values(1)), 0.8)))
    }

    behavior of "getColumn"
    it should "be consistent with getColumns for a binary node" in {
        val a = UserNodeEntity("A", Seq(), Seq(1D, 2D), Seq(0.1, 0.9))

        assert(a.getColumns()(a.values(0)) == a.getColumn(a.values(0)))
        assert(a.getColumns()(a.values(0)) == a.getColumn(0))
        assert(a.getColumns()(a.values(1)) == a.getColumn(a.values(1)))
        assert(a.getColumns()(a.values(1)) == a.getColumn(1))
    }

    it should "be consistent with getColumns for a node with two binary parents" in {
        val a = UserNodeEntity("A", Seq(), Seq(1D, 2D), Seq(0.5, 0.5))
        val b = UserNodeEntity("B", Seq(), Seq(1D, 2D), Seq(0.5, 0.5))
        val c = UserNodeEntity("C", Seq(a, b), Seq(2D, 3), Seq(0.3, 0.7,
                                                                0.1, 0.9,
                                                                0.4, 0.6,
                                                                0.2, 0.8))

        assert(c.getColumns()(c.values(0)) == c.getColumn(0))
        assert(c.getColumns()(c.values(0)) == c.getColumn(c.values(0)))
        assert(c.getColumns()(c.values(1)) == c.getColumn(1))
        assert(c.getColumns()(c.values(1)) == c.getColumn(c.values(1)))
    }
}