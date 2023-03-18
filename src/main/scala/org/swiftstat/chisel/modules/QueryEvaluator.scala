package org.swiftstat.chisel.modules


import chisel3._
import org.swiftstat.pgm.compiler.entity.QueryEntity
import org.swiftstat.pgm.compiler.entity.visitor.QueryExpressionASTNodeVisitor
import org.swiftstat.pgm.compiler.entity.ast.QueryExpressionBinaryOp
import org.swiftstat.pgm.compiler.entity.ast.QueryBinaryOpTypes
import org.swiftstat.pgm.compiler.entity.ast.QueryExpressionList
import org.swiftstat.pgm.compiler.entity.ast.QueryExpressionNodeOp
import org.swiftstat.pgm.compiler.entity.ast.QueryExpressionConstOp
import org.swiftstat.chisel.modules.FloatUtils._
import hardfloat.CompareRecFN
import org.swiftstat.chisel.modules
import org.json4s.prefs.EmptyValueStrategy
import hardfloat.MulRecFN
import chisel3.util.ShiftRegister
import org.swiftstat.pgm.compiler.entity._
import chisel3.util.Counter

/**
  * This computes the weighted average of a sequence of values.
  * Since this is only used for queries, there are some pecularities
  *
  * Due to the high latency of division, this actually only considers every 24th value for the average.
  * This can change in the future, but its the simplest way to do it for now, and it aids in quality
  * since ignoring every N samples is often used in conjunction with Burn-in periods to reduce correlation between
  * samples
  *
  * for a value provided at cycle n, the average will be updated in cycle n+23 (total = (22+n) - n + 1 = 24 cycles)
  * at which point a new computation will begin based on the inputs provided in cycle n+23
  *
  *
  * Q: If we stall for 24 cycles why are some ops pipelined?
  * A: Its so that someone smarter than me can figure out how to pipeline it fully.
  * Since we know it takes 19 cycles to divide, we can do 19 ops in parallel and
  * its just a matter of figuring out the right bypass (the 2nd unit's WtEt should be ready when it starts dividing)
  *
  * @param weight
  * @param value
  */
class RunningWeightedAverage(weight: RecFN, value: RecFN) extends Module {
    val io = IO(new Bundle {
        val curWeight = Input(RecFN())
        val curValue = Input(RecFN())
        val currentWeightedAverage = Output(RecFN())

        // Debug outputs
        val inputs = new Bundle {
            val Wt = Output(RecFN())
            val Et = Output(RecFN())
            val St = Output(RecFN())
            val Sigmat = Output(RecFN())
        }
        val WtPlusSigmat = Output(RecFN())
        val WtEt = Output(RecFN())
        val StSigmat = Output(RecFN())
        val EtNumerator = Output(RecFN())
        val EtDenominator = Output(RecFN())
        val EtNext = Output(RecFN())
        val WtNext = Output(RecFN())
        val Et = Output(RecFN())
        val Wt = Output(RecFN())
        val enableUpdate = Output(Bool())
        val cnt = Output(UInt(5.W))
    })

    val shouldCount = ShiftRegister(true.B, 1, false.B, true.B)
    val (cnt, enableUpdate) = Counter(shouldCount, 23)

    val rSt = RegInit(0.RecFN)
    val rSigmat = RegInit(0.RecFN)
    val rWt = RegInit(0.RecFN)
    val rEt = RegInit(0.RecFN)

    // E_t+1 = (W_t * E_t + curWeight*curValue) / (W_t + curWeight)
    // W_t+1 = W_t + curWeight

    val rWtEt = RegNext(MultRecFN(rWt, rEt), 0.RecFN)
    val rStSigmat = RegNext(MultRecFN(rSigmat, rSt), 0.RecFN)
    val rEtNumerator = RegNext(AddRecFN(rWtEt, rStSigmat), 0.RecFN)
    val rWtPlusSigmat = RegNext(AddRecFN(rWt, rSigmat), 0.RecFN)
    val rEtDenominator = RegNext(rWtPlusSigmat, 0.RecFN)
    val rEtNext = RegNext(DivRecFN(rEtNumerator, rEtDenominator), 0.RecFN)
    val rWtNext = RegNext(rEtDenominator)

    // at the beginning of the computation, load the new data
    when (cnt === 0.U) {
        rSt := io.curValue
        rSigmat := io.curWeight
    }
    // at the end of the computation, update the values
    when (enableUpdate) {
        rWt := rWtNext
        rEt := rEtNext
    }

    io.currentWeightedAverage := rEt

    // set some debug outputs
    io.inputs.Wt := rWt
    io.inputs.Et := rEt
    io.inputs.St := rSt
    io.inputs.Sigmat := rSigmat
    io.WtPlusSigmat := rWtPlusSigmat
    io.WtEt := rWtEt
    io.StSigmat := rStSigmat
    io.EtNumerator := rEtNumerator
    io.EtDenominator := rEtDenominator
    io.EtNext := rEtNext
    io.WtNext := rWtNext
    io.Et := rEt
    io.Wt := rWt
    io.cnt := cnt
    io.enableUpdate := enableUpdate
}

object RunningWeightedAverage {
    def apply(weight: RecFN, value: RecFN): RecFN = {
        val module = Module(new RunningWeightedAverage(weight, value))
        module.io.curWeight := weight
        module.io.curValue := value

        module.io.currentWeightedAverage
    }
}

/**
  * A software implementation of a running weighted average
  *
  */
class SoftwareRunningWeightedAverage {
    private var accumulatedWeight = 0.0
    private var currentAverage = 0.0
    var delta = 1.0
    private var deltaScore = 0
    private var isDone = false
    var count = 0

    def get(): Double = currentAverage
    def done(): Boolean = isDone
    def update(weight: Double, value: Double): Double = {
        val newAverage = (accumulatedWeight * currentAverage + weight*value) / (accumulatedWeight + weight)
        val newWeight = accumulatedWeight + weight

        delta = Math.abs((newAverage - currentAverage) / newAverage)
        if (delta < 0.00001) {
            deltaScore += 1
        } else {
            deltaScore = 0
        }

        if (deltaScore >= 100) {
            isDone = true
        }

        currentAverage = newAverage
        accumulatedWeight = newWeight
        count += 1
        currentAverage
    }
}


class QueryEvaluator(nodes: Seq[UserNodeEntity], query: QueryEntity, samples: SampleSet, values: ValueSet, hardwareWeightedAverage: Boolean) extends Module {
    override def desiredName: String = s"QueryEvaluator_${query.name}"

    val io = IO(new Bundle {
        val samplesIn = Input(samples.cloneType)
        val valuesIn = Input(values.cloneType)
        val currentWeightedAverage = Output(RecFN())
        val curExpressionValue = Output(RecFN())
        val curWeight = Output(RecFN())
    })

    val curQueryExpressionValue : RecFN = QueryExpressionEvaluator(query, io.valuesIn).io.queryValue
    val curWeight : RecFN = if (query.evidence.nonEmpty) {
        WeightMapper(nodes, query, io.samplesIn).rawValues.reduce(MultRecFN(_, _))
    } else {
        1.RecFN
    }

    if (hardwareWeightedAverage) {
        io.currentWeightedAverage := RunningWeightedAverage(curWeight, curQueryExpressionValue)
    } else {
        io.currentWeightedAverage := 0.RecFN
    }

    io.curExpressionValue := curQueryExpressionValue
    io.curWeight := curWeight
}

object QueryEvaluator {
    /**
      * Construct a queryevaluator given some info about the query and current samples/values
      *
      * @param query    the query to synthesize
      * @param samples the current samples from samplenetwork
      * @param values the current values mapped from samples (the QueryCoordinator should do this mapping _ONCE_ to be shared by all queries)
      * @return the current value of the query
      */
    def apply(nodes: Seq[UserNodeEntity], query: QueryEntity, samples: SampleSet, values: ValueSet, hardwareWeightedAverage: Boolean): QueryEvaluator = {
        val module = Module(new QueryEvaluator(nodes, query, samples, values, hardwareWeightedAverage))

        module.io.samplesIn := samples
        module.io.valuesIn := values

        module
    }
}