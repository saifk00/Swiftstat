package org.swiftstat.pgm.compiler.entity.ast

import org.swiftstat.pgm.compiler.entity.visitor.QueryExpressionASTNodeVisitor
import org.swiftstat.pgm.compiler.entity.UserNodeEntity
import org.swiftstat.mdl.QueryProto
import org.swiftstat.mdl.RelationQuery
import org.swiftstat.mdl.QueryExpression

/**
  * Represents an QueryEntity's query expression
  */
sealed trait QueryExpressionASTNode {
    def accept[T](visitor: QueryExpressionASTNodeVisitor[T]): T
    override def toString: String = {
        val visitor = new QueryExpressionASTNodeVisitor[String] {
            override def visit(node: QueryExpressionList): String = node.children.map(_.accept(this)).mkString("(", " ", ")")
            override def visit(node: QueryExpressionBinaryOp): String = s"(${node.left.accept(this)} ${node.op} ${node.right.accept(this)})"
            override def visit(node: QueryExpressionNodeOp): String = node.node.name
            override def visit(node: QueryExpressionConstOp): String = node.value.toString
        }
        accept(visitor)
    }
}

object QueryBinaryOpTypes extends Enumeration {
    type QueryBinaryOpType = Value
    val `>`, `<`, `>=`, `<=`, `=`, `+`, `-`, `*`, `/`, `^` = Value
}

case class QueryExpressionBinaryOp(
    val left: QueryExpressionASTNode,
    val right: QueryExpressionASTNode,
    val op: QueryBinaryOpTypes.QueryBinaryOpType
) extends QueryExpressionASTNode {
    def accept[T](visitor: QueryExpressionASTNodeVisitor[T]) = visitor.visit(this)
}

case class QueryExpressionNodeOp(node: UserNodeEntity) extends QueryExpressionASTNode {
    def accept[T](visitor: QueryExpressionASTNodeVisitor[T]) = visitor.visit(this)
}

case class QueryExpressionConstOp(value: Double) extends QueryExpressionASTNode {
    def accept[T](visitor: QueryExpressionASTNodeVisitor[T]) = visitor.visit(this)
}

case class QueryExpressionList(children: Seq[QueryExpressionASTNode]) extends QueryExpressionASTNode {
    def accept[T](visitor: QueryExpressionASTNodeVisitor[T]) = visitor.visit(this)
}
