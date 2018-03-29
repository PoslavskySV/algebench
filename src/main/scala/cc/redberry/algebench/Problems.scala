package cc.redberry.algebench

import java.io.{BufferedReader, FileReader}

import cc.redberry.rings.scaladsl.IntZ

/**
  *
  */
object Problems {

  import cc.redberry.algebench.Generators._
  import io.circe.generic.auto._

  /** Enum problem type */
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

  /**
    * @param configuration configuration
    * @param file          file with encoded problems
    */
  final case class ProblemData(configuration: ProblemConfiguration, file: String) {
    val problemType: ProblemType = configuration.problemType
  }

  object ProblemData {
    /**
      * Decode problem data from file
      */
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

  import io.circe._
  import io.circe.generic.semiauto._

  implicit val pbDecoder: Decoder[ProblemConfiguration] = deriveDecoder[ProblemConfiguration]
  implicit val pbEncoder: Encoder[ProblemConfiguration] = deriveEncoder[ProblemConfiguration]
}
