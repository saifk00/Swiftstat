package org.swiftstat.pgm.compiler.entity

import org.swiftstat.mdl.UserNode

/**
  * Common usage:
  * evidence match {
  *     case RunTimeEvidenceEntity(node) => [do something with node]
  *     case CompileTimeEvidenceEntity(node, value) => [do something with node and its value)
  *     ...
  * }
  *
  * @param node
  */
sealed abstract class EvidenceEntity(val node: UserNodeEntity)
case class RunTimeEvidenceEntity(override val node: UserNodeEntity) extends EvidenceEntity(node);
case class CompileTimeEvidenceEntity(override val node: UserNodeEntity, val value: UserNodeValue) extends EvidenceEntity(node);
