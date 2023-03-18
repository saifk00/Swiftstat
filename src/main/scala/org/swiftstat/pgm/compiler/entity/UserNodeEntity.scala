package org.swiftstat.pgm.compiler.entity

import com.typesafe.scalalogging.Logger

final class UserNodeEntity private (
    val name: String,
    val parents: Seq[UserNodeEntity],
    val values: Seq[UserNodeValue],
    val cpd: CPDEntity) {

        // NOTE: for some reason, the default case-class implementation
        //       of these is super slow. Instead, we assume that all nodes have
        //       unique names and thus can just use the name as hash/equality
        override def hashCode(): Int = name.hashCode()
        override def equals(obj: Any): Boolean = obj match {
          case une: UserNodeEntity => une.name == this.name
          case _ => false
        }

        // this has no sensible restrictions like CPDRow, so its just an alias
        type CPDColumnEntity = Seq[(Set[UserNodeValue], Double)]

        /**
          * Get the association between actual user node values and their canonical 'index'
          * which can be used e.g. to index a row in the CPD
          *
          * @return a set of tuples indicating the ordinal for each value
          */
        def getValueOrdinalAssociation(): Set[(UserNodeValue, Int)] = this.values.zip(0 until this.values.size).toSet

        def getValueOrdinal(value: UserNodeValue): Int = this. getValueOrdinalAssociation().find(_._1 == value).get._2

        /**
          * Get the maximum ordinal value for this node
          * e.g. if a node takes on values (244.3, 21, 3) the maximum ordinal is 2
          *
          * @return the maximum ordinal
          */
        def getMaxOrdinal(): Int = this.values.size - 1;

        /**
          * Get the set of rows in this node's CPD. Each row represents a discrete probability distribution
          *
          * @return a set which associates a set of parent values to a [[CPDRowEntity]]
          *         specifying the probability distribution of the current node in that context
          */
        def getRows(): Set[(Set[UserNodeValue], CPDRowEntity)] = {
            if (this.parents.isEmpty) {
                val row = this.values.map(v => this.cpd.distribution(Set(v)))

                Set((Set(), CPDRowEntity(row)))
            } else {
                val parentValues = this.parents.map(_.values)
                val parents = CPDEntity.crossProduct(parentValues)
                val rowDistributions = parents.map(rowLabel => {
                    // rowLabel is something like <A0,B0>
                    // so take each of my values in order and append it to this row label
                    // to get the probability labels in order (each of my values designates a column)
                    // then look them up in the cpd distribution
                    val probLabels = this.values.map(value => rowLabel :+ value)
                    val probabilities = probLabels.map(probLabel => this.cpd.distribution(probLabel.toSet))

                    (rowLabel.toSet, CPDRowEntity(probabilities))
                })

                rowDistributions.toSet
            }
        }

        /**
          * Get a map which, for a given value for the current node, provides the column as a sequence of (parentValueSet, double)
          * I.e. each entry of the column tells you what the probability is in that column given the set of values for the parent
          *
          * @return
          */
        def getColumns(): Map[UserNodeValue, CPDColumnEntity] = {
            val rows = getRows()

            this.values.zipWithIndex.map{ case (value, index) => {
                val column = rows.map(row => {
                    (row._1, row._2.distribution(index))
                })

                value -> column.toSeq
            }}.toMap
        }

        /**
          * Gets the column corresponding to the given value
          *
          * @param value the value for which to get the column
          * @return the column
          */
        def getColumn(value: UserNodeValue): CPDColumnEntity = getColumns()(value)

        /**
          * Gets the column corresponding to the i'th value of this node
          *
          * @param i the index of the value to get the column for
          * @return the column
          */
        def getColumn(i: Int): CPDColumnEntity = getColumn(this.values(i))

        def parent(name: String): Option[UserNodeEntity] = this.parents.find(_.name == name)

        /**
          * Get the number of bits required to represent value indices (samples) of this node
          */
        def numBits: Int = (Math.floor(Math.log(this.values.length) / Math.log(2)) + 1).toInt
    }


object UserNodeEntity{
    val logger = Logger[UserNodeEntity]

    def apply(name: String,
            parents: Seq[UserNodeEntity],
            values: Seq[Double],
            distribution: Seq[Double]): UserNodeEntity = {
        val userNodeValues = values.map(UserNodeValue(name, _))

        logger.debug(s"Constructing CPD for node ${name} with ${parents.length} parents")
        val cpd = CPDEntity(parents, userNodeValues, distribution)
        new UserNodeEntity(name, parents, userNodeValues, cpd)
    }
}
