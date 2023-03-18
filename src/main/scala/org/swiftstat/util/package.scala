package org.swiftstat

package object util {

    /**
      * Floating point parameters (exp and sig widths)
      *
      *
      * TODO(skhattak) find a cooler(/better) way to do this. Im thinking an enum with associated implicit fn that returns an object
      * that has the exp and sig properties
      */

    def exp(f: Int) = f match {
      case 16 => 5
      case 32 => 8
      case 64 => 11
    }

    def sig(f: Int) = f match {
      case 16 => 11
      case 32 => 24
      case 64 => 53
    }

    /**
      * Sort nodes in reverse topological order. Parents appear before children, but the map
      * that you pass in has edges pointing from children to parents. Allows you to pass in
      * nodes that are not present as keys in the adjacency map.
      *
      * @param adjacency a map where each value lists the parents of the key
      * @param allNodes a list of all the nodes that should appear
      * @return a list of nodes in 'reverse' topological order
      */
    def sortReverseTopological[A](adjacency: Map[A, Seq[A]], allNodes: Set[A]): Seq[A] = {
        var to_visit = allNodes.toSet
        var visited = Set[A]()
        var ordered = Seq[A]()
        def visit(node: A): Unit = {
            if (visited.contains(node)) return

            adjacency.getOrElse(node, Seq()).foreach(visit)
            ordered = ordered :+ node
            to_visit -= node
            visited += node
        }

        while (!to_visit.isEmpty) { visit(to_visit.head) }

        ordered
    }


    implicit class MapUtils[A](val adjacency: Map[A, Seq[A]]) {
      /**
      * Sort nodes in reverse topological order. Parents appear before children, but the map
      * that you pass in has edges pointing from children to parents. Assumes that all nodes
      * appear as keys in the map
      *
      * @param adjacency a map where each value lists the parents of the key
      * @return a list of nodes in 'reverse' topological order
      */
      def sortReverseTopological: Seq[A] = {
        val allNodes = adjacency.keys.toSet
        util.sortReverseTopological(adjacency, allNodes)
      }
    }
}
