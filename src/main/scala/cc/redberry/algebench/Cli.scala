package cc.redberry.algebench

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths, StandardOpenOption}

import cc.redberry.algebench.Util.TempFileManager
import cc.redberry.rings.WithVariables
import cc.redberry.rings.scaladsl._
import cc.redberry.rings.scaladsl.syntax._
import org.rogach.scallop._

import scala.io.Source
import scala.language.postfixOps
import scala.util.Random

/**
  *
  */
object Cli {

  import Generators._
  import Problems._
  import Solvers._

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

  /** with single output file */
  trait SingleOutputOpt {
    this: ScallopConfBase =>

    val output = trailArg[String](
      descr = "Output file",
      required = true
    )
  }

  /** with single output file */
  trait SingleInputOpt {
    this: ScallopConfBase =>

    val input = trailArg[String](
      descr = "Input file",
      required = true
    )
  }


  /** basic options for generating problems */
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

  trait RingsSolverOpts {
    this: ScallopConfBase =>

    val rings = toggle(
      name = "rings",
      default = Some(false),
      descrYes = "Rings (http://ringsalgebra.io)"
    )

    val ringsExecutable = opt[String](
      descr = "Path to Rings executable",
      default = Some("rings.repl"),
      noshort = true
    )

    def mkRingsSolver()(implicit tempFileManager: TempFileManager): Option[RingsSolver] =
      if (rings())
        Some(RingsSolver(ringsExecutable()))
      else
        None
  }

  trait MathematicaSolverOpts {
    this: ScallopConfBase =>

    val mma = toggle(
      name = "mathematica",
      default = Some(false),
      descrYes = "Wolfram Mathematica"
    )

    val mmaExecutable = opt[String](
      descr = "Path to Mathematica executable",
      default = Some("mathematica"),
      noshort = true
    )

    def mkMathematicaSolver()(implicit tempFileManager: TempFileManager): Option[MathematicaSolver] =
      if (mma())
        Some(MathematicaSolver())
      else
        None
  }


  class GlobalConf(arguments: Seq[String]) extends ScallopConf(arguments) {
    val keepTempFiles = toggle(
      name = "keep-temp-files",
      default = Some(false)
    )

    // generating problems
    val generateProblems = new Subcommand("generate") with RandomOpt {

      // generating GCD problems
      val gcdProblem = new Subcommand("gcd") {

        // use simple uniform distribution
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
            default = None,
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

        // use simple sharp distribution
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
            default = None,
            validate = 0 <)

          val totalDegree = opt[Int](
            descr = "Total degree of polynomials",
            default = Some(10),
            validate = 0 <)
        }
        addSubcommand(sharp)

        // use custom distributions
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


    // solve generated problems
    val solveProblems = new Subcommand("solve")
      with RingsSolverOpts
      with MathematicaSolverOpts
      with SingleInputOpt
      with SingleOutputOpt {
      banner("Run tools on specified benchmarks")

      def mkSolvers()(implicit tempFileManager: TempFileManager): Seq[Option[Solver]] =
        Seq(mkRingsSolver(), mkMathematicaSolver())
    }
    addSubcommand(solveProblems)

    verify()
  }

  def main(args: Array[String]): Unit = {
    import io.circe.generic.auto._
    import io.circe.parser.decode
    import io.circe.syntax._

//        val runConfiguration = new GlobalConf(Array("generate", "gcd", "uniform", "-n", "10", "--n-variables", "3", "--min-size", "2", "--max-size", "2", "gcd.problems"))
    val runConfiguration = new GlobalConf(Array("solve", "--rings", "gcd.problems", "out"))

    // temp file manager
    implicit val fileManaged: TempFileManager = TempFileManager(runConfiguration.keepTempFiles())

    runConfiguration.subcommands match {

      case List() =>
        runConfiguration.printHelp()

      case runConfiguration.generateProblems :: generateSpec => {
        implicit val random: Random = new Random(runConfiguration.generateProblems.rndSeed())

        generateSpec match {
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
              commonOpts.nProblems(),
              gcdDist,
              f1Dist,
              f2Dist)

            val outFile = new File(commonOpts.output())
            if (outFile.exists())
              outFile.delete()

            val writer = new PrintWriter(Files.newBufferedWriter(outFile.toPath, StandardOpenOption.CREATE))
            try {
              writer.println("#" + gcdConfig.asInstanceOf[ProblemConfiguration].asJson.noSpaces)
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
        }
      }


      case (solveCmd@runConfiguration.solveProblems) :: Nil =>
        val solvers = solveCmd.mkSolvers().filter(_.isDefined).map(_.get)
        if (solvers.isEmpty)
          throw new RuntimeException("no any solvers specified")

        val header = Source.fromFile(solveCmd.input()).getLines().toIterable.headOption match {
          case Some(h) => h.replaceFirst("^#", "")
          case None => throw new RuntimeException("Can't read problem configuration from " + solveCmd.input())
        }

        val problemConfiguration = decode[ProblemConfiguration](header) match {
          case Left(err) => throw new RuntimeException("Can't read problem configuration from " + solveCmd.input(), err)
          case Right(conf) => conf
        }

        solvers.find(s => !s.isApplicable(problemConfiguration)) match {
          case Some(solver) => throw new RuntimeException(s"Solver ${solver.name} is not applicable for problem:\n " + problemConfiguration.asJson.spaces2)
          case _ =>
        }

        val problemData = ProblemData(problemConfiguration, solveCmd.input())

        val results = solvers.map(s => (s.name, s.solve(problemData)))
        println(results)
        Files.write(Paths.get(solveCmd.output()), java.util.Collections.singleton(results.toString()))

      case Nil =>

      case _ =>
    }
  }
}
