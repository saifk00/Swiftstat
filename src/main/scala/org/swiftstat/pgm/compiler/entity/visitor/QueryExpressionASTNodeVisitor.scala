package org.swiftstat.pgm.compiler.entity.visitor

import org.swiftstat.pgm.compiler.entity.ast._

/**
  * A visitor that can visit any [[QueryExpressionASTNode]], where
  * each visit method returns a value of type T
  */
trait QueryExpressionASTNodeVisitor[T] {
    def visit(node: QueryExpressionBinaryOp): T
    def visit(node: QueryExpressionList): T
    def visit(node: QueryExpressionNodeOp): T
    def visit(node: QueryExpressionConstOp): T
}

class DefaultQueryExpressionASTNodeVisitor extends QueryExpressionASTNodeVisitor[QueryExpressionASTNode] {
    def visit(node: QueryExpressionBinaryOp): QueryExpressionASTNode = {
        val left = node.left.accept(this)
        val right = node.right.accept(this)
        node
    }

    def visit(node: QueryExpressionList): QueryExpressionASTNode = {
        val children = node.children.map(_.accept(this))
        node
    }

    def visit(node: QueryExpressionNodeOp): QueryExpressionASTNode = node

    def visit(node: QueryExpressionConstOp): QueryExpressionASTNode = node
}
