package org.swiftstat.pgm.compiler.entity

final case class CPDRowEntity(distribution: Seq[Double]) {
    require(distribution.sum == 1.0,
        s"Invalid probability distribution: ${distribution}")
    final val values = 0 until distribution.size
}
