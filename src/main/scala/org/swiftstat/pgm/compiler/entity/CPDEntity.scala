package org.swiftstat.pgm.compiler.entity

import com.typesafe.scalalogging.Logger

final case class CPDEntity(distribution: Map[Set[UserNodeValue], Double]);

object CPDEntity {
    val logger = Logger[CPDEntity]

    /**
      * Constructs the 'cross product' of a list of sequences
      * example:
      *         crossProduct(List(List(1,2), List(3,4), List(5,6)))
      * returns:
      *         List(List(1,3,5), List(1,3,6), List(1,4,5), List(1,4,6),
      *              List(2,3,5), List(2,3,6), List(2,4,5), List(2,4,6))
      *
      * @param vals a list of sequences to be 'crossed'
      */
    def crossProduct[T](vals: Seq[Seq[T]]): Seq[Seq[T]] = {
        if (vals.isEmpty) {
          vals
        } else if (vals.length == 1) {
          // List(List(1, 2)) -> List(List(1), List(2))
          vals.head.map(Seq(_))
        }else if (vals.length == 2) {
            // base case, there are only 2 sets to cross
            // so we can simply prepend each value of the first list to the values of
            // the second list
            vals.head.flatMap(x => vals.last.map(y => Seq(x, y)))
        } else {
            vals.head.flatMap(x => crossProduct(vals.tail).map(y => x +: y))
        }
    }

    /**
      * Make value labels for a CPD
      *
      * @param parentValues the parent's value lists, *in order*
      * @param nodeValues the node's value list
      * @return a list of value labels, which can be mapped to flattened probabilities
      *        in the CPD
      */
    def makeLabels(parentValues: Seq[Seq[UserNodeValue]], nodeValues: Seq[UserNodeValue]): Seq[Set[UserNodeValue]] = {
        logger.debug(s"Making labels for ${parentValues.size + nodeValues.size} nodes")
        crossProduct(parentValues :+ nodeValues).map(_.toSet)
    }

    /**
      * Construct the CPD from labels and the probabilities
      *
      * @param labels the flattened list of labels for each probability
      * @param distribution the flattened list of probabilities
      */
    def makeFromLabels(labels: Seq[Set[UserNodeValue]], distribution: Seq[Double]): CPDEntity = {
        val nodeName = labels.last.last.name
        if (distribution.size != labels.size) {
          throw new IllegalArgumentException(
            if (distribution.size < labels.size) {
                s"(${nodeName}) Not enough"
            } else {
                s"(${nodeName}) Too many"
            } + s" values in CPD distribution of node ${nodeName} (expected ${labels.size}, got ${distribution.size})"
          )
        }

        logger.debug(s"Made CPDEntity for ${nodeName} containing ${labels.size} elements")
        val labelDistribution = labels.zip(distribution).toMap
        CPDEntity(labelDistribution)
    }

    /**
      * Construct a CPD with no parents
      *
      * @param values the values of the node, *in order*
      * @param distribution the probabilities of the node, *in order*
      */
    def apply(values: Seq[UserNodeValue], distribution: Seq[Double]): CPDEntity = apply(Seq(), values, distribution)


    /**
      * Construct a CPD based on its parents, values and probabilities
      *
      * @param parents the parents of the node, *in order*
      * @param values the values of the node, *in order*
      * @param distribution the probabilities of the node, *in order*
      */
    def apply(parents: Seq[UserNodeEntity], values: Seq[UserNodeValue], distribution: Seq[Double]): CPDEntity = {
        val labels = if (parents.isEmpty) {
            values.map(Set(_))
        } else {
            makeLabels(parents.map(_.values), values)
        }

        makeFromLabels(labels, distribution)
    }
}
