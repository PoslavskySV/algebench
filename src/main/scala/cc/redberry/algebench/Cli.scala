package cc.redberry.algebench

import java.io.PrintWriter
import java.nio.file.{Files, Paths, StandardOpenOption}

import cc.redberry.algebench.Problems.PolynomialGCDConfiguration
import cc.redberry.rings.WithVariables
import cc.redberry.rings.scaladsl._
import cc.redberry.rings.scaladsl.syntax._
import org.rogach.scallop._

import scala.language.postfixOps
import scala.util.Random

/**
  *
  */
object Cli {


  /** default rnd seed */
  val DEFAULT_RND_SEED: Long = 314158926535L
  /** default field characteristic */
  val DEFAULT_CHARACTERISTIC: IntZ = Z(0)

  private implicit val intZConverter: ValueConverter[IntZ] = singleArgConverter[IntZ](new IntZ(_))


  /** options for random */
  trait RandomOpt {
    this: ScallopConfBase =>
    val rndSeed = opt[Long](
      descr = "Random seed",
      default = Some(DEFAULT_RND_SEED),
      required = false,
      noshort = true)
  }

  trait SingleOutputOpt {
    this: ScallopConfBase =>

    val output = trailArg[String](
      required = true
    )
  }

  trait BasePolyGenOpts extends SingleOutputOpt {
    this: ScallopConfBase =>

    val nProblems = opt[Int](
      descr = "Number of problems to generate",
      required = true,
      validate = 0 <)

    val nVariables = opt[Int](
      descr = "Number of variables",
      required = true,
      noshort = true,
      validate = 0 <)

    val characteristic = opt[IntZ](
      descr = "Field characteristic",
      default = Some(DEFAULT_CHARACTERISTIC),
      validate = p => p.signum() >= 0
    )

    val bitLength = opt[Int](
      descr = "Bit length of coefficients (only for characteristic 0)",
      noshort = true,
      default = Some(128),
      validate = 0 <
    )
  }

  class GlobalConf(arguments: Seq[String]) extends ScallopConf(arguments) {
    val generateProblems = new Subcommand("generate") with RandomOpt {

      val gcdProblem = new Subcommand("gcd") {
        val uniform = new Subcommand("uniform") with BasePolyGenOpts {
          banner("Generates polynomials with uniformly distributed exponents")

          val minSize = opt[Int](
            descr = "Minimal number of terms in factors",
            noshort = true,
            default = Some(100),
            validate = 0 <)

          val maxSize = opt[Int](
            descr = "Maximal number of terms in factors",
            noshort = true,
            default = Some(100),
            validate = 0 <)

          val size = opt[Int](
            descr = "Size of factors and gcd",
            default = Some(100),
            validate = 0 <)

          val minDegree = opt[Int](
            descr = "Minimal exponent of each variable in monomials",
            default = Some(10),
            validate = 0 <)

          val maxDegree = opt[Int](
            descr = "Maximal exponent of each variable in monomials",
            default = Some(20),
            validate = 0 <)

        }
        addSubcommand(uniform)

        val sharp = new Subcommand("sharp") with BasePolyGenOpts {
          banner("Generates polynomials with sharp exponents")

          val minSize = opt[Int](
            descr = "Minimal number of terms in factors",
            noshort = true,
            default = None,
            validate = 0 <)

          val maxSize = opt[Int](
            descr = "Maximal number of terms in factors",
            noshort = true,
            default = None,
            validate = 0 <)

          val size = opt[Int](
            descr = "Size of factors and gcd",
            default = Some(100),
            validate = 0 <)

          val totalDegree = opt[Int](
            descr = "Total degree of polynomials",
            default = Some(10),
            validate = 0 <)

        }

        val custom = new Subcommand("custom") with BasePolyGenOpts {
          banner("Generates polynomials with sharp exponents")

          val factor1 = opt[String](
            descr = "JSON string for first factor distribution",
            required = true)

          val factor2 = opt[String](
            descr = "JSON string for second factor distribution",
            required = true)

          val gcd = opt[String](
            descr = "JSON string for gcd distribution",
            required = true)

        }
        addSubcommand(custom)
      }
      addSubcommand(gcdProblem)
    }
    addSubcommand(generateProblems)


    verify()
  }

  def main(args: Array[String]): Unit = {
    println(intZConverter)
    import Generators._
    import io.circe.generic.auto._
    import io.circe.parser.decode
    import io.circe.syntax._

    val runConfiguration = new GlobalConf(Array("generate", "gcd", "uniform", "-n", "10", "--n-variables", "3", "gcd.txt"))

    runConfiguration.subcommands match {

      case List() =>
        runConfiguration.printHelp()

      case runConfiguration.generateProblems :: subcommands => {
        implicit val random: Random = new Random(runConfiguration.generateProblems.rndSeed())

        subcommands match {
          case runConfiguration.generateProblems.gcdProblem :: method :: Nil =>
            // generate GCD problems
            val gcdProblem = runConfiguration.generateProblems.gcdProblem

            val (f1Dist: PolynomialsDistribution, f2Dist: PolynomialsDistribution, gcdDist: PolynomialsDistribution) = method match {
              case p@gcdProblem.uniform =>
                implicit val ring: Ring[IntZ] = Zp(p.characteristic())
                // coefficients distribution
                implicit val coefficients: CoefficientsDistribution = CoefficientsDistribution.uniform(p.bitLength())
                // exponents distribution
                implicit val exponents: ExponentsDistribution = ExponentsDistribution.uniform(p.minDegree(), p.maxDegree())
                // polynomials distribution
                val polys = PolynomialsDistribution.uniform(p.nVariables(), p.minSize(), p.maxSize())

                (polys, polys, polys)

              case p@gcdProblem.sharp =>
                implicit val ring: Ring[IntZ] = Zp(p.characteristic())
                // coefficients distribution
                implicit val coefficients: CoefficientsDistribution = CoefficientsDistribution.uniform(p.bitLength())
                // exponents distribution
                implicit val exponents: ExponentsDistribution = ExponentsDistribution.sharp(p.totalDegree())
                // polynomials distribution
                val polys = PolynomialsDistribution.uniform(p.nVariables(), p.minSize(), p.maxSize())

                (polys, polys, polys)

              case p@gcdProblem.custom =>
                def decodeJSON(json: String) = {
                  decode[PolynomialsDistribution](json) match {
                    case Left(err) => throw new RuntimeException("broken json: " + err)
                    case Right(dist) => dist
                  }
                }

                (decodeJSON(p.factor1()), decodeJSON(p.factor2()), decodeJSON(p.gcd()), p.output())
            }

            // common opts
            val commonOpts = method.asInstanceOf[BasePolyGenOpts]

            val gcdConfig = PolynomialGCDConfiguration(
              commonOpts.characteristic(),
              WithVariables.defaultVars(commonOpts.nVariables()),
              commonOpts.nProblems(), gcdDist, f1Dist, f2Dist)


            val writer = new PrintWriter(Files.newBufferedWriter(Paths.get(commonOpts.output()), StandardOpenOption.CREATE))
            try {
              writer.println(gcdConfig.asJson.noSpaces)
              writer.println(Seq("#problemID", "poly1", "poly2", "expected gcd").mkString("\t"))
              for (iProblem <- 0 until gcdConfig.nProblems) {
                val gcd = gcdConfig.gcd.sample
                val f1 = gcd * gcdConfig.factor_1.sample
                val f2 = gcd * gcdConfig.factor_2.sample
                writer.println(Seq(iProblem, f1, f2, gcd).mkString("\t"))
              }
            } finally {
              writer.close()
            }


          case Nil =>
        }
      }
      case _ =>
    }
  }
}
