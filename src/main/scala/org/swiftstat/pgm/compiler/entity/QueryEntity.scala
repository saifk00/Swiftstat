package org.swiftstat.pgm.compiler.entity

import org.swiftstat.pgm.compiler.entity.ast.QueryExpressionASTNode

final case class QueryEntity private (
    name: String,
    epsilonDeltaSpecification: EpsilonDeltaSpecification,
    rootQueryExpression: QueryExpressionASTNode,
    evidence: Set[EvidenceEntity],
    queryType: QueryTypes.QueryType
) {
    def evidenceNodes: Set[UserNodeEntity] = evidence.map(ee => ee match {
        case RunTimeEvidenceEntity(node) => node
        case CompileTimeEvidenceEntity(node, _) => node
    })

    def runTimeEvidenceNodes: Set[UserNodeEntity] = evidence.filter(ee => ee match {
        case RunTimeEvidenceEntity(node) => true
        case CompileTimeEvidenceEntity(node, value) => false
    })
    .map(_.node)
}

final case class EpsilonDeltaSpecification(
    epsilon: Double,
    delta: Double,
)

object QueryTypes extends Enumeration {
    type QueryType = Value
    val Marginal, Expectation, MaximumAPosteriori = Value
}

object QueryEntity {
    def apply(
        name: String,
        epsilon: Double,
        delta: Double,
        rootQueryExpression: QueryExpressionASTNode,
        evidence: Set[EvidenceEntity],
        querytype: QueryTypes.QueryType
    ): QueryEntity = {
        val epsilonDeltaSpecification = EpsilonDeltaSpecification(epsilon, delta)
        QueryEntity(name, epsilonDeltaSpecification, rootQueryExpression, evidence, querytype)
    }
}