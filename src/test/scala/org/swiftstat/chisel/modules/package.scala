package org.swiftstat.chisel

package object modules {
    // lets us do stuff like 0.5 === 0.2 +- 0.4
    case class DoubleWithTolerance(value: Double, tolerance: Double) {
        def ===(other: DoubleWithTolerance): Boolean = {
            val diff = Math.abs(value - other.value)
            diff <= tolerance
        }

        def ===(other: Double): Boolean = {
            val diff = Math.abs(value - other)
            diff <= tolerance
        }

        def !===(other: DoubleWithTolerance) = !(this === other)
        def !===(other: Double) = !(this === other)

        def +- (value: Double, tolerance: Double): DoubleWithTolerance = DoubleWithTolerance(value, tolerance)

        override def toString() = s"${value} +- ${tolerance}"
    }

    implicit class DoubleToleranceUtil(value: Double) {
        def +- (tolerance: Double): DoubleWithTolerance = DoubleWithTolerance(value, tolerance)
        def ===(other: DoubleWithTolerance): Boolean = other === value
        def !===(other: DoubleWithTolerance): Boolean = !(other === value)
        def ===(other: Double) = value == other
        def !===(other: Double) = value != other
        override def toString(): String = value.toString
    }

}
