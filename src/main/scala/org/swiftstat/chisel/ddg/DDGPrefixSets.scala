package org.swiftstat.chisel.ddg

import scala.collection.mutable.Set
import scala.collection.mutable.Map
import BitSequences._

final case class DDGPrefixSets (val prefixes: Seq[Set[Prefix]]) {
    val size = prefixes.size
    def apply(index: Int): Set[Prefix] = prefixes(index)
}

final object DDGPrefixSets {
    def getValuePrefixes(root: DDGNode): Map[Int, Set[Prefix]] = {
        var valuePrefixes = Map[Int, Set[Prefix]]()
        def visit(node: DDGNode, prefix: Prefix): Unit = node match {
            case Branch(left, right) => {
                visit(left, prefix :+ 0)
                visit(right, prefix :+ 1)
            }
            case Terminal(value) => {
                val curPrefixes = valuePrefixes.getOrElse(value, Set[Prefix]())
                valuePrefixes(value) = curPrefixes.union(Set(prefix))
            }
            case _ => throw new Exception("Invalid DDGNode")
        }

        visit(root, Seq())
        valuePrefixes
    }

    def makeBitPrefixes(valuePrefixes: Map[Int, Set[Prefix]]): Seq[Set[Prefix]] = {
        val numBits = (Math.floor(Math.log(valuePrefixes.keys.max) / Math.log(2)) + 1).toInt
        val bitPrefixes = Seq.fill(numBits)(Set[Prefix]())
        valuePrefixes.foreach { case (value, prefixes) => {
            val bitSets = (0 until numBits).filter(bit => (value & (1 << bit)) != 0).toSeq
            bitSets.foreach(bitSet => bitPrefixes(bitSet).addAll(prefixes))
        }}

        bitPrefixes
    }

    /**
      * Create a [[DDGPrefixSets]] containing the bit prefixes for
      * the DDG tree rooted at @root
      *
      * @param root the root note of a DDG tree
      * @return
      */
    def apply(root: DDGNode): DDGPrefixSets = {
        val valuePrefixes = getValuePrefixes(root)
        val bitPrefixes = makeBitPrefixes(valuePrefixes)

        DDGPrefixSets(bitPrefixes)
    }
}
