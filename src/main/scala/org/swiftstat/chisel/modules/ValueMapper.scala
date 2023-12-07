package org.swiftstat.chisel.modules
import chisel3._
import org.swiftstat.pgm.compiler.entity.UserNodeEntity
import hardfloat.recFNFromFN
import org.swiftstat.util.sig
import org.swiftstat.util.exp
import org.swiftstat.chisel.modules.FloatUtils._
import chisel3.util.MuxLookup

/*
    Maps the 'samples' used by samplernetwork into the actual values
    e.g. for a pgm node Grade : [50, 80, 90, 100]
    the 'samples' generated in the network are actually elements of {0, 1, 2, 3}

    Some notes on Berkeley Hardfloat (may be wrong, this is just my understanding):
    - All the FPU operations use 'RecFN' which is their "Recoded Floating Number"
    - There are also 'rawFloat' which is the Chisel data type that wraps around Bits (a native Chisel type) in IEEE754 format
    - IN is an "Integer Number", i.e. with a sign and Bits of raw data
    - FN is a "Floating Number", i.e. Bits with a sign, exponent, and mantissa

    - So the flow is:
        - Encode a scala `double` as a `rawFloat` using the IEEE754 format (java.lang.Double.doubleToLongBits)
        - Convert the `rawFloat` to a `RecFN` using the `recFNFromFN` method
        - Do the operation on the `RecFN`
        - Convert the RecFN to rawFloat using the `recFNToFN` method

*/
class ValueMapper(nodes: Seq[UserNodeEntity]) extends Module {
    val io = IO(new Bundle {
        val samples = Input(SampleSet(nodes))
        val mapped = Output(ValueSet(nodes))
    })

    // for each node in [nodes], we can generate a Vec of RecFNs from
    // the values in UserNodeEntity::values. Then, using the sample
    // from io.samples, we can index this Vec to get the actual value
    // for that sample
    val valuesByName = nodes.map { node =>
        val values = node.values.map(nv => nv.value.RecFN)
        val valueVec = VecInit(values)

        node.name -> MuxLookup(io.samples.samplesByName(node.name), valueVec(0), valueVec.zipWithIndex.map { case (v, i) => i.U -> v })
    }.toMap

    io.mapped.valuesByName.foreach({ case (name, value) => value := valuesByName(name) })
}

object ValueMapper {
    def apply(nodes: Seq[UserNodeEntity], samples: SampleSet): ValueSet = {
        val module = Module(new ValueMapper(nodes))
        module.io.samples := samples

        module.io.mapped
    }
}
