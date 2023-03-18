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

trait QueryComputeNode extends Module {
    val io = IO(new Bundle {
        val in = Input(RecFN())
        val out = Output(RecFN())
    })
}

class Assigner extends QueryComputeNode {
    io.out := io.in
}

/**
  * This is kind of funky, but basically it is a module that wraps a network of QueryComputeNodes
  * by visiting the given AST, making sure to connect sample values via the ValueSet interface
  * and outputting the overall expression via io.output
  *
  * @param query the query to synthesize
  * @param valueSetType the TYPE (NOT HARDWARE) of valueSet to be plugged in
  */
class QueryComputationVisitor(query: QueryEntity, valueSetType: ValueSet) extends Module with QueryExpressionASTNodeVisitor[Data] {
    val io = IO(new Bundle {
        val values = Input(valueSetType.cloneType)
        val output = Output(RecFN())
    })

    io.output := query.rootQueryExpression.accept(this)

    def assign(desired: UInt): QueryComputeNode = {
        val module = Module(new Assigner)
        module.io.in := desired

        module
    }

    def assign(desired: Bool): QueryComputeNode = {
        val module = Module(new Assigner)
        // convert the bool to a uint to a recFN that can be averaged
        module.io.in := BoolToRecFN(desired)

        module
    }

    def visit(node: QueryExpressionBinaryOp): Data = {
        val left = node.left.accept(this)
        val right = node.right.accept(this)

        node.op match {
            case QueryBinaryOpTypes.`<` => RecFNComparer.lessThan(left.asUInt, right.asUInt)
            case QueryBinaryOpTypes.`>` => RecFNComparer.greaterThan(left.asUInt, right.asUInt)
            case QueryBinaryOpTypes.`<=` => RecFNComparer.lessThanOrEqual(left.asUInt, right.asUInt)
            case QueryBinaryOpTypes.`>=` => RecFNComparer.greaterThanOrEqual(left.asUInt, right.asUInt)
            case QueryBinaryOpTypes.`=` => left.asUInt === right.asUInt
            case QueryBinaryOpTypes.`+` => AddRecFN(left.asUInt, right.asUInt)
            case QueryBinaryOpTypes.`-` => SubRecFN(left.asUInt, right.asUInt)
            case QueryBinaryOpTypes.`*` => MultRecFN(left.asUInt, right.asUInt)
            // div is a little complicated; need to use the divSqrt module and connect it to a Ready interface
            case QueryBinaryOpTypes.`/` => throw new Exception("Unsupported binary operator")
            case QueryBinaryOpTypes.`^` => throw new Exception("Unsupported binary operator")
            case _ => throw new Exception("Unsupported binary operator")
        }
    }

    def visit(node: QueryExpressionList): Data = node.children.map(_.accept(this)).foldLeft(true.B)(_ & _.asUInt.asBool).RecFN
    def visit(node: QueryExpressionNodeOp): Data = io.values.valuesByName(node.node.name)
    def visit(node: QueryExpressionConstOp): Data = node.value.RecFN
}

/**
 * Module that computes the query _expression_'s value (NOT the query's value, which is a likelihood-weighted average)
 */
class QueryExpressionEvaluator(query: QueryEntity, valueSet: ValueSet) extends Module {
    val io = IO(new Bundle {
        val values = Input(valueSet.cloneType)
        val queryValue = Output(RecFN())
    })

    // query -> Marginal (P[E_1, E_2, ... | Evidence])
    //       -> Expectation (E[E_1 + E_2 ... | Evidence])
    val visitorModule = Module(new QueryComputationVisitor(query, io.values))

    visitorModule.io.values := io.values
    io.queryValue := visitorModule.io.output
}

object QueryExpressionEvaluator {
    // valueSet: the TYPE of ValueSet (NOT HARDWARE!)
    def apply(query: QueryEntity, valueSet: ValueSet): QueryExpressionEvaluator = {
        val module = Module(new QueryExpressionEvaluator(query, valueSet))

        module.io.values := valueSet
        module
    }
}