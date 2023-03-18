package org.swiftstat.pgm.compiler.entity

import org.swiftstat.mdl.QueryProto
import org.swiftstat.pgm.compiler.entity.ast._
import org.swiftstat.mdl.RelationQuery
import org.swiftstat.mdl.QueryExpression


/**
  * Builds a [[QueryExpressionASTNode]] tree from various
  * MDL protos (e.g. [[QueryProto]])
  */
class QueryExpressionASTBuilder(userNodes: Set[UserNodeEntity]) {
    val userNodeMap = userNodes.map(node => node.name -> node).toMap

    def parseProto(proto: Seq[RelationQuery]): QueryExpressionASTNode = QueryExpressionList(proto.map(parseProto(_)))
    def parseProto(proto: RelationQuery): QueryExpressionASTNode = {
        val relation = proto.relation match {
            case RelationQuery.Type.EQUAL => QueryBinaryOpTypes.`=`
            case RelationQuery.Type.LESS_THAN => QueryBinaryOpTypes.`<`
            case RelationQuery.Type.LESS_THAN_OR_EQUAL => QueryBinaryOpTypes.`<=`
            case RelationQuery.Type.GREATER_THAN => QueryBinaryOpTypes.`>`
            case RelationQuery.Type.GREATER_THAN_OR_EQUAL => QueryBinaryOpTypes.`>=`
            case _ => throw new Exception("Unhandled op type")
        }
        QueryExpressionBinaryOp(parseProto(proto.lexpr.get), parseProto(proto.rexpr.get), relation)
    }

    def parseProto(proto: QueryExpression): QueryExpressionASTNode = {
        if (proto.`type` == QueryExpression.Type.NODE) {
            QueryExpressionNodeOp(userNodeMap(proto.node))
        } else if (proto.`type` == QueryExpression.Type.CONST) {
            QueryExpressionConstOp(proto.value)
        } else {
            val op = proto.`type` match {
                case QueryExpression.Type.PLUS => QueryBinaryOpTypes.`+`
                case QueryExpression.Type.MINUS => QueryBinaryOpTypes.`-`
                case QueryExpression.Type.MULTIPLY => QueryBinaryOpTypes.`*`
                case QueryExpression.Type.DIVIDE => QueryBinaryOpTypes.`/`
                case QueryExpression.Type.POW => QueryBinaryOpTypes.`^`
                case _ => throw new Exception("Unhandled queryexpression op type")
            }

            QueryExpressionBinaryOp(parseProto(proto.lexpr.get), parseProto(proto.rexpr.get), op)
        }
    }
}
