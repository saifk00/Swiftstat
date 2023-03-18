package org.swiftstat.chisel.ddg

import org.swiftstat.pgm.compiler.entity.CPDRowEntity
import scala.collection.immutable.BitSet
import BitSequences._
import com.typesafe.scalalogging.Logger

final case class MinimalDDG private(
    val probabilityTable: Seq[FixedPoint],
    val treeRootNode: DDGNode,
    val bitPrefixSets: DDGPrefixSets) {

    /**
      * Provides the set of prefixes that should result in bit i
      * being activated (little endian - bit i ~ 2^i)
      *
      * TODO(skhattak) this is currently NOT OPTIMIZED. For example, when i run it with (0.125, 0.875) the logic
      *                expression generated is 111 | 0XX | 10X , when it could easily be ~000. This optimization must be
      *                done BEFORE synthesis - it cannot be done at the FIRRTL level because there is no way to know that
      *                we only care about the frequency of matches and not the actual values of the bits.
      */
    def getBitPrefixMap(): Map[Int, Set[Prefix]] = bitPrefixSets.prefixes
        .zipWithIndex
        .map{ case (prefixes, bit) => bit -> prefixes.toSet }
        .toMap
}

object MinimalDDG {
    val logger = Logger[MinimalDDG]
    case class TableRow(probability: FixedPoint, value: Int) {
        override def toString: String = {
            return s"[${probability.mkString(" ")}]"
        }
    }



    /**
      * Converts a probability to a fixed point format
      *
      * The returned format has its leftmost bit representing 2^0,
      * and then each subsequent element i representing 2^-i. that is,
      * fixed[i] is the 2^-i place of the resulting value.
      *
      * WARNING: if the specified @bitPrecision is higher than what
      *          is required for the given value, the size of the result
      *          will be *less* than @bitPrecision.
      *
      * examples:
      *          convertToFixedPoint(0.75, 4) = Seq(0, 1, 1)
      *                 -> 0.75 is 2^-1 + 2^-2
      *          convertToFixedPoint(1, 1) = Seq(1)
      *                 -> 1.0 is 2^-0
      *          convertToFixedPoint(0.75, 65535) = Seq(0, 1, 1)
      *                 -> overspecified bit precision
      *
      * @param probability a value <= 1 to convert to fixed point format
      * @param bitPrecision the maximum number of bits in the fixed-point representation
      * @return the fixed point value as a bitstring
      */
    def convertToFixedPoint(probability: Double, bitPrecision: Int): FixedPoint = {
        // TODO(skhattak) do this in a better (more Scala-ish) way
        // this is currently just copied from the python version
        var p = probability
        var b = scala.collection.mutable.Seq.fill(bitPrecision)(0)
        for (i <- 0 until bitPrecision) {
            if (p == 0) {
                // early termination - there are no more 1s to be found
                return b.take(i).toList
            } else if (p >= 1.0D / (Math.pow(2, i))) {
                p -= 1.0/Math.pow(2, i)
                b(i) = 1
            } else {
                b(i) = 0
            }
        }

        b.toList
    }

    /**
      * Helper function to construct a set of branches in a layer.
      * You probably dont want this one - use [[makeTree]] instead.
      */
    def makeTreeLayer(values: Seq[TableRow]): Seq[Branch] = {
        if (values.isEmpty)
            return Seq[Branch]()

        // nodes with a 1 in the current column (which corresponds to the current layer)
        // will have a terminal node in the current layer
        val nodesInThisLayer = values.filter(_.probability.head == 1)
        val terminals = nodesInThisLayer.map[DDGNode](r => new Terminal(r.value))

        // All nodes with a 1 in a column other than the current one will be terminal nodes
        // in layers below. Recursively get the branches that layers below this will construct
        val nodesInChildLayers = values.filter(_.probability.size > 1)
        val branches = makeTreeLayer(nodesInChildLayers.map(r => TableRow(r.probability.tail, r.value)))

        // the nodes at this layer consist of the branch nodes constructed in below layers
        // along with the terminal nodes at this layer.
        // there must be an even number of nodes since everything adds up to 1 in the end,
        // so we can pair them up into branches and return them to the layer above this
        val layerNodes = terminals ++ branches
        val result = layerNodes.grouped(2).map(pair => new Branch(pair(0), pair(1))).toSeq

        result
    }

    /**
      * Creates a tree from a set of probabilities where each node is a [[DDGNode]]
      * and returns the root of the constructed tree
      *
      * @param values the rows (in order) of the distribution
      * @return the root of the constructed tree, with terminal nodes corresponding to the
      *         indices of @values
      */
    def makeTree(values: Seq[FixedPoint]): DDGNode = {
        values.zipWithIndex.find(_._1.head == 1) match {
            case Some(row) => return Terminal(row._2)
            case None => null
        }

        val rows = values.zipWithIndex.map(row => TableRow(row._1.tail, row._2))
        logger.debug(s"Constructing DDG tree with ${rows.size} rows:\n${rows.mkString(",\n")}")

        val rootLayer = makeTreeLayer(rows)
        logger.debug(s"Constructed DDG tree with ${rows.size} rows")

        // this should never happen unless the probability table is invalid
        // (i.e. the fixed point representation of the probabilities dont add up to 1)
        assert(rootLayer.size == 1, s"ERROR: invalid DDG tree from ${values}")
        rootLayer.head
    }

    def fromCPDRowEntity(row: CPDRowEntity, bitPrecision: Int): MinimalDDG = {
        // first see if we can reduce the precision based on the probabilities
        val fixedPointRows = row.distribution.map(convertToFixedPoint(_, bitPrecision))
        val root = makeTree(fixedPointRows)
        val bitPrefixes = DDGPrefixSets(root)

        MinimalDDG(fixedPointRows, root, bitPrefixes)
    }
}