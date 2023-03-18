package org.swiftstat

import org.swiftstat.util.MapUtils
import org.scalatest.flatspec.AnyFlatSpec

class ReverseTopologicalSortSpec extends AnyFlatSpec {
    behavior of "ReverseTopologicalSort"

    it should "correctly sort a simple graph" in {
        val graph = Map[String, Seq[String]](
            "A" -> Seq(),
            "B" -> Seq(),
            "C" -> Seq(),
            "D" -> Seq("A", "B"),
            "E" -> Seq("C", "D"),
        )

        val result = graph.sortReverseTopological

        // first 3 must be ABC (in any order), last 2 must be DE in order
        assert(result.take(3).toSet == Set("A", "B", "C"))
        assert(result.takeRight(2) == Seq("D", "E"))
    }

    it should "sort a triangle" in {
        val graph = Map[String, Seq[String]](
            "C" -> Seq("A", "B"),
            "A" -> Seq(),
            "B" -> Seq(),
        )

        val result = graph.sortReverseTopological

        assert(result == Seq("A", "B", "C"))
    }

    it should "sort a triangle without parents" in {
        val graph = Map[String, Seq[String]](
            "C" -> Seq("A", "B")
        )

        val result = util.sortReverseTopological(graph, Set("A", "B", "C"))

        assert(result == Seq("A", "B", "C"))
    }
}
