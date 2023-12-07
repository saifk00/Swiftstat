package org.swiftstat.chisel.modules
import chisel3._
import org.swiftstat.pgm.compiler.entity.CPDEntity
import org.swiftstat.pgm.compiler.entity.UserNodeEntity
import chisel3.util.MuxCase
import com.typesafe.scalalogging.Logger

class CPDSampler(node: UserNodeEntity) extends Module {
  val logger = Logger[CPDSampler]

  logger.debug(s"Constructing CPDSampler for node ${node.name}")

  // names are unique so we can simply append them
  override def desiredName: String = s"CPDSampler_${node.name}"

  val io = IO(new Bundle {
    val parents = Input(SampleSet(node.parents))
    val sample = Output(Sample(node))
  })

  io.sample := 0.U

  // for each row, we create the row sampler and keep the association with the parent assignment
  // 'assignment' meaning the value of the parent node for that row
  private val rowSamplers = node.getRows()
    .map { case (parentAssignment, cpdRow) => parentAssignment -> Module(RowSampler.fromCPDRow(cpdRow)) }
    .toMap

  // get a big map of the ordinal for each parent assignment. This is what we should match
  // the measured sample against to determine the row for sampling
  private val parentOrdinals = node.parents.flatMap(parent =>
    parent.getValueOrdinalAssociation().map { case (v, i) => v -> i }.toMap).toMap

  // if there are no parents we need not synthesize a mux, just directly assign samples
  // TODO(skhattak) probably should refactor the muxer into a function or something nice
  val sample = if (node.parents.nonEmpty) {
    // finally, mux over all these conditions (synthesis will take care of bit mappings)
    // Side note - see how we dont have to worry about the actual mux wiring and can instead
    //             focus on the domain-specific logic (i.e. the concept of parents matching
    //             assignments)? neat!
    MuxCase(0.U, rowSamplers
      .map {
        case (parentAssignments, rowSampler) =>
          // if all the actually measured parent samples are equal to the
          // assignments in this row, the final sample should be the one drawn by
          // this rowSampler
          val parentAssignmentsMatched = parentAssignments
            .map(parentAssignment => {
              val measuredSample = io.parents.samplesByName(parentAssignment.name)
              val valueForThisRow = parentOrdinals(parentAssignment).U

              measuredSample === valueForThisRow
            })
            .reduceLeft(_ & _)

            parentAssignmentsMatched -> rowSampler.io
      }
    .toSeq)
  } else {
    require(rowSamplers.size == 1, "orphan had multiple row samplers")

    rowSamplers.head._2.io
  }

  io.sample := sample

  logger.debug(s"Constructed CPDSampler for node ${node.name}")
}

object CPDSampler {
    def fromUserNode(userNode: UserNodeEntity): Unit = {
        throw new NotImplementedError("Not implemented yet")
    }
}