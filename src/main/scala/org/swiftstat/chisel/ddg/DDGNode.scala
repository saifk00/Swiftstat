package org.swiftstat.chisel.ddg

sealed trait DDGNode
class Branch(val left: DDGNode, val right: DDGNode) extends DDGNode {
    override def toString: String = s"($left,$right)"
    override def equals(obj: Any): Boolean = obj match {
        case b: Branch => (b.left == left && b.right == right) || (b.left == right && b.right == left)
        case _ => false
    }
}
object Branch {
    def apply(left: DDGNode, right: DDGNode): DDGNode = new Branch(left, right)
    def unapply(node: Branch): Option[(DDGNode, DDGNode)] = Some((node.left, node.right))
}

case class Terminal(value: Int) extends DDGNode {
    override def toString: String = s"$value"
}

