package org.swiftstat.pgm.compiler.entity
import org.scalatest.flatspec.AnyFlatSpec
import org.swiftstat.pgm.compiler.entity.CPDEntity

object TestData {
  val nodeA = UserNodeEntity(
    "A",
    Seq[UserNodeEntity](),
    Seq(1D, 2, 3),
    Seq(0.1, 0.2, 0.7))
}

class CPDEntitySpec extends AnyFlatSpec {
  behavior of "CPD"
  it should "correctly construct a simple CPD" in {
  }
}

class MakeLabelsSpec extends AnyFlatSpec {
  behavior of "MakeLabels"
  it should "correctly construct simple labels" in {

    val parentValues = Seq(Seq(
      UserNodeValue("A", 1),
      UserNodeValue("A", 2),
      UserNodeValue("A", 3)))
    val myValues = Seq(
      UserNodeValue("B", 1),
      UserNodeValue("B", 2))

    val labels = CPDEntity.makeLabels(parentValues, myValues)
    assert(labels.length == 6)
    assert(labels(0) == Set(UserNodeValue("A", 1), UserNodeValue("B", 1)))
    assert(labels(1) == Set(UserNodeValue("A", 1), UserNodeValue("B", 2)))
    assert(labels(2) == Set(UserNodeValue("A", 2), UserNodeValue("B", 1)))
    assert(labels(3) == Set(UserNodeValue("A", 2), UserNodeValue("B", 2)))
    assert(labels(4) == Set(UserNodeValue("A", 3), UserNodeValue("B", 1)))
    assert(labels(5) == Set(UserNodeValue("A", 3), UserNodeValue("B", 2)))
  }
}

class CrossProductSpec extends AnyFlatSpec {
  behavior of "CrossProduct"
  it should "be able to construct a cartesian product of integers" in {

    val sequence = Seq(Seq(0, 1), Seq(2, 3))
    val result = CPDEntity.crossProduct(sequence)

    assert(result == Seq(Seq(0, 2), Seq(0, 3), Seq(1, 2), Seq(1, 3)))
  }
  it should "be able to construct a cartesian product of strings" in {

    val sequence = Seq(Seq("x", "y"), Seq("a", "b"), Seq("c", "d"))
    val result = CPDEntity.crossProduct(sequence)

    assert(result == Seq(
      Seq("x", "a", "c"),
      Seq("x", "a", "d"),
      Seq("x", "b", "c"),
      Seq("x", "b", "d"),
      Seq("y", "a", "c"),
      Seq("y", "a", "d"),
      Seq("y", "b", "c"),
      Seq("y", "b", "d")))
  }

  it should "be useful when flattened" in {

      val sequence = Seq(Seq("x", "y"), Seq("a", "b"), Seq("c", "d"))
      val result = CPDEntity.crossProduct(sequence).map(_.mkString)

      assert(result == Seq(
        "xac",
        "xad",
        "xbc",
        "xbd",
        "yac",
        "yad",
        "ybc",
        "ybd"))
  }
  it should "handle 0 values" in {
    val seq = Seq[Seq[Double]]()
    val result = CPDEntity.crossProduct(seq)
    assert(result.isEmpty)
  }
  it should "handle 1 list of values" in {
    val seq = Seq(Seq(1, 2, 3))
    val result = CPDEntity.crossProduct(seq)
    assert(result == Seq(Seq(1), Seq(2), Seq(3)))
  }
}