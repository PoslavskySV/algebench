package cc.redberry.algebench

import java.io.{BufferedReader, FileReader}

import cc.redberry.rings.scaladsl.IntZ

/**
  *
  */
object Problems {

  import cc.redberry.algebench.Generators._
  import io.circe.generic.auto._

  sealed trait ProblemType
  case object PolynomialGCD extends ProblemType
  case object PolynomialFactorization extends ProblemType

  /** parameters of the problem */
  sealed trait ProblemConfiguration {
    /** enum value of problem type */
    val problemType: ProblemType
    /** number of problems to solve */
    val nProblems: Int
  }

  /** Polynomial GCD parameters */
  final case class PolynomialGCDConfiguration(characteristic: IntZ,
                                              variables: Array[String],
                                              nProblems: Int,
                                              gcd: PolynomialsDistribution,
                                              factor_1: PolynomialsDistribution,
                                              factor_2: PolynomialsDistribution)
    extends ProblemConfiguration {
    override val problemType: ProblemType = PolynomialGCD
  }

  /** Polynomial factorization parameters */
  final case class PolynomialFactorizationConfiguration(characteristic: IntZ,
                                                        variables: Array[String],
                                                        nProblems: Int,
                                                        nFactors: IntDistribution,
                                                        factors: PolynomialsDistribution)
    extends ProblemConfiguration {
    override val problemType: ProblemType = PolynomialFactorization
  }

  final case class ProblemData(configuration: ProblemConfiguration, file: String) {
    val problemType: ProblemType = configuration.problemType
  }

  object ProblemData {
    def fromFile(filePath: String): ProblemData = {
      val reader = new BufferedReader(new FileReader(filePath))
      val header =
        try {
          reader.readLine()
        } catch {
          case e: Exception => throw new RuntimeException(e)
        } finally {
          reader.close()
        }
      val config = io.circe.parser.decode[ProblemConfiguration](header) match {
        case Left(error) => throw new RuntimeException("broken header line: " + error)
        case Right(c) => c
      }
      ProblemData(config, filePath)
    }
  }


  //  case class StoredProblemData[+Type <: ProblemType](problemType: Type,
  //                                                     problemConfiguration: ProblemConfiguration[Type],
  //                                                     filePath: String)
  //
  //
  //  def importProblem(filePath: String): StoredProblemData = {
  //    val reader = new BufferedReader(new FileReader(filePath))
  //    val firstLine =
  //      try {
  //        reader.readLine()
  //      } catch {
  //        case e: Exception => throw new RuntimeException(e)
  //      } finally {
  //        reader.close()
  //      }
  //
  //
  //  }

  //
  //
  //  object ProblemType {
  //    def deserialize(string: String): ProblemType =
  //      string.toLowerCase match {
  //        case "polynomialgcd" => PolynomialGCD
  //      }
  //  }
  //
  //  case object PolynomialGCD extends ProblemType
  //
  //  case object PolynomialFactorization extends ProblemType
  //
  //
  //  type Poly = MultivariatePolynomial[IntZ]
  //
  //  sealed trait ProblemSet {
  //    def name: String
  //
  //    def dumpToFile(): Unit
  //
  //  }

  //  class PolynomialGCD(val nWarmUps: Int, val problems: Seq[(Poly, Poly, Poly)])


  //  class PolynomialGCD(nProblems: Int, nWarmUps: Int,
  //                      gcds: PolynomialsDistribution,
  //                      cofactors: PolynomialsDistribution) {
  //    val problems: Seq[(Poly, Poly, Poly)] = {
  //      (0 until nProblems).map(_ => {
  //        val gcd = gcds.sample()
  //        (gcd * cofactors.sample(), gcd * cofactors.sample(), gcd)
  //      })
  //    }
  //  }
}
