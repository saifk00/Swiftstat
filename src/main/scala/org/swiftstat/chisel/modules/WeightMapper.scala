package org.swiftstat.chisel.modules

import chisel3._
import org.swiftstat.pgm.compiler.entity._
import org.apache.commons.lang3.NotImplementedException
import chisel3.util.MuxCase
import org.swiftstat.chisel.modules.FloatUtils.RecFNDoubleUtils
import FloatUtils._

class WeightMapper(nodes: Seq[UserNodeEntity], query: QueryEntity) extends Module {
    val io = IO(new Bundle {
        val samples = Input(SampleSet(nodes))
        // weights must be mapped for _all_ evidence nodes - not just runtime ones
        val mapped = Output(WeightSet(query.evidenceNodes.toSeq))
    })

    // synthesizes the correct RecFN weight for this node given a statically determined value for it, but based on runtime parent samples
    def synthForValue(node: UserNodeEntity, value: UserNodeValue): RecFN = {
        assert(node.name == value.name, s"got compile time evidence mismatch (${node.name} != ${value.name})")

        val column = node.getColumn(value)

        val parentSamplesToWeight = column.map{ case (parentValueSet, weight) => {
            // here we need to figure out if this weight should be output. The condition is that,
            // for each parent UNV (userNodeValue) for this column entry, the sample for that UNV
            // given by io.samples is equal to the ordinal of the UNV

            // parentValueSet is the set of values of parent nodes that must be matched for this column entry
            // to be active
            val parentSamplesMatched = if (parentValueSet.nonEmpty) {
                parentValueSet.map(parentUNV => {
                    // the current Sample in hardware
                    val currentHardwareSample = io.samples.samplesByName(parentUNV.name).sample

                    // the ordinal that the sample should equal for this column to be active
                    val ordinalParentTarget = node.parent(parentUNV.name).get.getValueOrdinal(parentUNV)

                    currentHardwareSample === ordinalParentTarget.U
                })
                .reduce(_ & _) // all parent samples must match
            } else {
                true.B
            }

            // if the parent samples match, then the weight is active
            parentSamplesMatched -> weight.RecFN
        }}

        MuxCase(0.RecFN, parentSamplesToWeight)
    }

    // synthesizes the correct RecFN weight based on all runtime values
    def synthForAll(node: UserNodeEntity): RecFN = {
        val thisNodeSample = io.samples.samplesByName(node.name).sample

        val matchThisNodeToWeight = node.values.map(value => {
            val valueBasedOnParent = synthForValue(node, value)
            val thisNodeForThisValueTarget = node.getValueOrdinal(value).U
            val thisNodeEqualsThisValue = thisNodeSample === thisNodeForThisValueTarget

            thisNodeEqualsThisValue -> valueBasedOnParent
        })

        MuxCase(0.RecFN, matchThisNodeToWeight)
    }

    query.evidence.map(evidence => evidence match {
        case RunTimeEvidenceEntity(node) => io.mapped.valuesByName(node.name) := synthForAll(node)
        case CompileTimeEvidenceEntity(node, value) => io.mapped.valuesByName(node.name) := synthForValue(node, value)
    })
}

object WeightMapper {
    def apply(nodes: Seq[UserNodeEntity], query: QueryEntity, samples: SampleSet): WeightSet = {
        val mapper = Module(new WeightMapper(nodes, query))
        mapper.io.samples := samples
        mapper.io.mapped
    }
}