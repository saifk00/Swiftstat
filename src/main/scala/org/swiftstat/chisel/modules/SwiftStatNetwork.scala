package org.swiftstat.chisel.modules

import chisel3._
import org.swiftstat.pgm.compiler.entity._
import org.swiftstat.chisel.modules.FloatUtils._
import chisel3.util.MuxCase
import chisel3.util.MixedVec
import scala.collection.immutable.SeqMap
import com.typesafe.scalalogging.Logger

/**
  * This is one of the nastiest hacks I have ever seen.
  *
  * Basically, Record is supposed to be an internal chisel class which Bundle uses to map reflected
  * property names to actual signal names. Normally, to create a dynamic set of signals as IO for a module,
  * you use Vec/MixedVec (depending on whether all elts have the same size). However, you cannot use suggestName
  * on the elements of a Vec/MixedVec in any way I could figure out. Instead, we need to override the reflection
  * mechanism and manually create a name->signal mapping within this new bundle.
  *
  * Because this is so nasty, I've decided to only use it where I think it is absolutely necessary: for the top-level
  * module that users will connect in their design.
  *
  * Credit to this SO post https://stackoverflow.com/questions/60086124/how-to-dynamically-add-io-ports-to-a-chisel-bundle
  *
  */
class SwiftStatNetworkBundle(mdl: MDLEntity) extends Record {
    def numBits(value: Int): Int = (Math.floor(Math.log(value) / Math.log(2)) + 1).toInt

    // only need inputs for _runtime_ evidence
    val nodes = mdl.runTimeEvidenceNodes.toSeq
    val queryBits = numBits(mdl.queries.size)
    val evidence = nodes.map(node => node.name -> Input(UInt(node.numBits.W)))
    val queryDebug = mdl.queries.flatMap(query => Seq(
        s"${query.name}Value" -> Output(FN()),
        s"${query.name}Weight" -> Output(FN())
    ))
    val elts = Seq("currentQueryID" -> Input(UInt(queryBits.W))) ++
        evidence ++
        queryDebug ++
        Seq("queryResult" -> Output(FN()))
    val elements = SeqMap(elts:_*)

    def queryResult: FN = elements("queryResult")
    def currentQueryID: UInt = elements("currentQueryID")
    def queryValue(query: QueryEntity): FN = elements(s"${query.name}Value")
    def queryWeight(query: QueryEntity): FN = elements(s"${query.name}Weight")

    override def cloneType: this.type = (new SwiftStatNetworkBundle(mdl)).asInstanceOf[this.type]
}

/**
  * Entry point for compiling an MDL to a SwiftStatNetwork (the hardware that answers queries)
  *
  * @param mdl the MDLEntity to synthesize
  * @param hardwareWeightedAverage whether to perform the weighted average in hardware
  *                                if set to `true`, the io.queryResult port will contain the computed average
  *                                otherwise a separate module will be required to compute
  *                                queries from the io.queryValue map
  */
class SwiftStatNetwork(mdl: MDLEntity, hardwareWeightedAverage: Boolean) extends Module {
    val logger = Logger[SwiftStatNetwork]

    logger.debug(s"Constructing SwiftStatNetwork for network ${mdl.name}")

    val nodes = mdl.userNodes.toSeq
    val queries = mdl.queries.toSeq
    val io = IO(new SwiftStatNetworkBundle(mdl))

    // we remap the user-facing mixedvec to an internal sampleset to allow nicer naming for the user
    val evidenceInternal = Wire(SampleSet(nodes))
    nodes.zipWithIndex.foreach{ case(node, index) => {
        if (mdl.runTimeEvidenceNodes.contains(node)) {
            evidenceInternal(node).sample := io.elements(node.name)
        } else {
            evidenceInternal(node).sample := 0.U
        }
    }}

    val samplerNetwork = SamplerNetwork(mdl, evidenceInternal, io.currentQueryID)
    val samples = samplerNetwork.io.samples
    val values = ValueMapper(nodes, samples)

    val queryEvaluators = mdl.queries.map(q => {
        val queryEvaluator = QueryEvaluator(nodes, q, samples, values, hardwareWeightedAverage)
        q -> queryEvaluator
    }).toMap

    queryEvaluators.foreach { case(qe, evaluator) => {
        // attach debug for this query
        io.queryValue(qe) := evaluator.io.curExpressionValue.FN
        io.queryWeight(qe) := evaluator.io.curWeight.FN
    }}

    if (hardwareWeightedAverage) {
        io.queryResult := MuxCase(0.RecFN.FN,
            mdl.queries.map(q => {
                val queryMatched = io.currentQueryID === queries.indexOf(q).U
                queryMatched -> queryEvaluators(q).io.currentWeightedAverage.FN
            }).toSeq)
    } else {
        io.queryResult := 0.RecFN.FN
    }

    logger.debug(s"Constructed SwiftStatNetwork for network ${mdl.name}")
}

object SwiftStatNetwork {
    def apply(mdl: MDLEntity, currentQuery: UInt, evidence: SampleSet, hardwareWeightedAverage: Boolean): SwiftStatNetwork = {
        val module = Module(new SwiftStatNetwork(mdl, hardwareWeightedAverage))
        module.io.elements("currentQueryID") := currentQuery
        module.io.evidence.foreach { case (name, evidenceIn) => evidenceIn := evidence.samplesByName(name).sample; }

        module
    }
}