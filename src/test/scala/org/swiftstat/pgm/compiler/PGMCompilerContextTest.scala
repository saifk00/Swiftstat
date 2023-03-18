package org.swiftstat.pgm.compiler
import org.scalatest.flatspec.AnyFlatSpec
import org.swiftstat.pgm.compiler.files._
import org.swiftstat.pgm.compiler.entity._
import org.swiftstat.pgm.compiler.entity.ast._


class PGMCompilerContextTest extends AnyFlatSpec {

    def textToMDLEntity(pgm: String): MDLEntity = {
        val ctx = new PGMCompilerContext()

        val pgmFile = PGMFile.fromText(pgm)
        val mdlFile = ctx.compile(pgmFile)
        val mdlEntity = ctx.compile(mdlFile)

        mdlEntity
    }

    behavior of "PGM to MDL"
    it should "parse a simple .pgm file" in {
        val pgmIn = """
            mypgm BAYESIAN

            connections:
            A -> C
            B -> C

            nodes:
            A : [0, 1] {
                0.5, 0.5;
            }

            B : [0, 1] {
                0.5, 0.5;
            }

            C<A,B> : [0, 1] {
                0.5, 0.5;
                0.25, 0.75;
                0.75, 0.25;
                0.825, 0.175;
            }

            queries:
            P[A+B>0 | C] #Q0<0.05, 0.95>
        """

        val mdlEntity = textToMDLEntity(pgmIn)

        assert(mdlEntity.name == "mypgm")

        assert(mdlEntity.networkType == NetworkTypes.Bayesian)

        assert(mdlEntity.userNodes.size == 3)
        val expectedA = UserNodeEntity("A", Seq(), Seq(0D, 1), Seq(0.5, 0.5))
        val expectedB = UserNodeEntity("B", Seq(), Seq(0D, 1), Seq(0.5, 0.5))
        val expectedC = UserNodeEntity("C", Seq(expectedA, expectedB), Seq(0D, 1), Seq(0.5, 0.5,
                                                                   0.25, 0.75,
                                                                   0.75, 0.25,
                                                                   0.825, 0.175))
        assert(mdlEntity.userNodes.contains(expectedA))
        assert(mdlEntity.userNodes.contains(expectedB))
        assert(mdlEntity.userNodes.contains(expectedC))

        assert(mdlEntity.queries.size == 1)
        assert(mdlEntity.queries.head.name == "Q0")
        assert(mdlEntity.queries.head.evidence == Set(RunTimeEvidenceEntity(expectedC)))
        assert(mdlEntity.queries.head.queryType == QueryTypes.Marginal)
        assert(mdlEntity.queries.head.epsilonDeltaSpecification == EpsilonDeltaSpecification(0.05, 0.95))
        assert(mdlEntity.queries.head.rootQueryExpression ==
            QueryExpressionList(Seq(
                QueryExpressionBinaryOp(
                QueryExpressionBinaryOp(
                    QueryExpressionNodeOp(expectedA),
                    QueryExpressionNodeOp(expectedB),
                    QueryBinaryOpTypes.`+`
                ),
                QueryExpressionConstOp(0.0),
                QueryBinaryOpTypes.`>`))))
    }

    it should "parse a .pgm with multi-connection syntax and compile-time evidence" in {
        val pgmIn = """
            mypgm BAYESIAN

            connections:
            A -> B
              -> C
            B -> C

            nodes:
            A : [0, 1] {
                0.5, 0.5;
            }

            B<A> : [0, 1] {
                0.5, 0.5;
                0.25, 0.75;
            }

            C<A,B> : [0, 1] {
                0.5, 0.5;
                0.25, 0.75;
                0.75, 0.25;
                0.825, 0.175;
            }

            queries:
            E[300*A + B | C=1] #Q0<0.05, 0.95>
        """

        val mdlEntity = textToMDLEntity(pgmIn)

        assert(mdlEntity.name == "mypgm")

        assert(mdlEntity.networkType == NetworkTypes.Bayesian)

        assert(mdlEntity.userNodes.size == 3)
        val expectedA = UserNodeEntity("A", Seq(), Seq(0D, 1), Seq(0.5, 0.5))
        val expectedB = UserNodeEntity("B", Seq(expectedA), Seq(0D, 1), Seq(0.5, 0.5,
                                                                    0.25, 0.75))
        val expectedC = UserNodeEntity("C", Seq(expectedA, expectedB), Seq(0D, 1), Seq(0.5, 0.5,
                                                                   0.25, 0.75,
                                                                   0.75, 0.25,
                                                                   0.825, 0.175))
        assert(mdlEntity.userNodes.contains(expectedA))
        assert(mdlEntity.userNodes.contains(expectedB))
        assert(mdlEntity.userNodes.contains(expectedC))

        assert(mdlEntity.queries.size == 1)
        assert(mdlEntity.queries.head.name == "Q0")
        assert(mdlEntity.queries.head.evidence == Set(CompileTimeEvidenceEntity(expectedC, expectedC.values(1))))
        assert(mdlEntity.queries.head.queryType == QueryTypes.Expectation)
        assert(mdlEntity.queries.head.epsilonDeltaSpecification == EpsilonDeltaSpecification(0.05, 0.95))
        assert(mdlEntity.queries.head.rootQueryExpression ==
                QueryExpressionBinaryOp(
                    QueryExpressionBinaryOp(
                        QueryExpressionConstOp(300.0),
                        QueryExpressionNodeOp(expectedA),
                        QueryBinaryOpTypes.`*`
                    ),
                    QueryExpressionNodeOp(expectedB),
                    QueryBinaryOpTypes.`+`
                ))
    }
}