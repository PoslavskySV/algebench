package cc.redberry.algebench

import java.io._
import java.nio.file.{Files, Paths, StandardOpenOption}

import cc.redberry.algebench.Util.TempFileManager
import cc.redberry.rings.scaladsl._
import cc.redberry.rings.scaladsl.syntax._
import org.rogach.scallop._
import org.rogach.scallop.exceptions.Help

import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.duration._
import scala.io.Source
import scala.language.postfixOps
import scala.util.Random

/**
  *
  */
object Cli {

  import Distributions._
  import Problems._
  import Solvers._

  /** default rnd seed */
  val DEFAULT_RND_SEED: Long = 314158926535L
  /** default field characteristic */
  val DEFAULT_CHARACTERISTIC: IntZ = Z(0)

  def defaultVars(nVars: Int): Array[String] = {
    val vars = new Array[String](nVars)
    var i = 1
    while (i <= nVars) {
      vars(i - 1) = "x" + i
      i += 1
    }
    vars
  }

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

  trait CharacteristicOpt extends SingleOutputOpt {
    this: ScallopConfBase =>
    val characteristic = opt[IntZ](
      descr = "Field characteristic",
      default = Some(DEFAULT_CHARACTERISTIC),
      validate = p => p.signum() >= 0
    )

  }

  /** basic options for generating problems */
  trait BasePolyGenOpts
    extends SingleOutputOpt
      with CharacteristicOpt {
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


    val bitLength = opt[Int](
      descr = "Bit length of coefficients (only for characteristic 0)",
      noshort = true,
      default = Some(128),
      validate = 0 <
    )
  }

  trait PolySizeOpts {
    this: ScallopConfBase =>
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

    def getMinSize(): Int = {
      if (size.isDefined)
        size()
      else
        minSize()
    }

    def getMaxSize(): Int = {
      if (size.isDefined)
        size()
      else
        maxSize()
    }
  }

  trait UniformDistributions extends PolySizeOpts {
    this: ScallopConfBase =>
    banner("\nGenerates polynomials with uniformly distributed exponents")

    val minDegree = opt[Int](
      descr = "Minimal exponent of each variable in monomials",
      default = Some(10),
      noshort = true,
      validate = 0 <=)

    val maxDegree = opt[Int](
      descr = "Maximal exponent of each variable in monomials",
      default = Some(20),
      noshort = true,
      validate = 0 <)

    footer("\n")
  }

  trait SharpDistributions extends PolySizeOpts {
    this: ScallopConfBase =>
    banner("\nGenerates polynomials with sharp exponents")

    val totalDegree = opt[Int](
      descr = "Total degree of polynomials",
      default = Some(10),
      noshort = true,
      validate = 0 <)

    footer("\n")
  }

  trait BaseFactorizationOpts
    extends BasePolyGenOpts {
    this: ScallopConfBase =>

    val nFactors = opt[Int](
      name = "n-factors",
      required = true,
      noshort = true,
      descr = "Number of factors"
    )

    val trivialFactorization = toggle(
      name = "no-factors",
      default = Some(false),
      descrYes = "All input polynomials are irreducible"
    )

    val trivial = toggle(
      "trivial",
      noshort = true,
      default = Some(false),
      descrYes = "whether to generate trivial problems (multiply all factors and increment)"
    )
  }

  /**
    * Global comand line configuration
    *
    * @param arguments
    */
  class GlobalConf(arguments: Seq[String]) extends ScallopConf(arguments) {
    version("algebench v1.0: a tool for benchmarking polynomial software")
    banner(
      """
        |Usage: algebench action [OPTION]...
        |Options:""".stripMargin)
    footer("\nFor all other tricks, consult the documentation!")


    val keepTempFiles = toggle(
      name = "keep-temp-files",
      default = Some(false)
    )

    // generating problems
    val generateProblems = new Subcommand("generate")
      with RandomOpt {
      banner(
        """
          |Usage: algebench generate [gcd|factor] [OPTIONS] output_file
          |Generates problem data and writes it to output_file
          |""".stripMargin)

      // generating GCD problems
      val gcdProblem = new Subcommand("gcd") {
        banner(
          """
            |Usage: algebench generate gcd [uniform|sharp|custom] [OPTIONS] output_file
            |Generates gcd problems data using specified method (uniform/sharp/custom) and writes to output_file
            |""".stripMargin)

        // use simple uniform distribution
        val uniform = new Subcommand("uniform")
          with BasePolyGenOpts
          with UniformDistributions
        addSubcommand(uniform)

        // use simple sharp distribution
        val sharp = new Subcommand("sharp")
          with BasePolyGenOpts
          with SharpDistributions
        addSubcommand(sharp)

        // use custom distributions
        val custom = new Subcommand("custom") with BasePolyGenOpts {
          banner("\nGenerates polynomials with custom distributions")

          val factor1 = opt[String](
            descr = "JSON string for first factor distribution",
            required = true)

          val factor2 = opt[String](
            descr = "JSON string for second factor distribution",
            required = true)

          val gcd = opt[String](
            descr = "JSON string for gcd distribution",
            required = true)

          footer("\n")
        }
        addSubcommand(custom)
      }
      addSubcommand(gcdProblem)

      val factorProblems = new Subcommand("factor") {
        banner(
          """
            |Usage: algebench generate factor [uniform|sharp|custom] [OPTIONS] output_file
            |Generates factorization problems data using specified method (uniform/sharp/custom) and writes to output_file
            |""".stripMargin)

        // use simple uniform distribution
        val uniform = new Subcommand("uniform")
          with BaseFactorizationOpts
          with UniformDistributions
        addSubcommand(uniform)

        // use simple sharp distribution
        val sharp = new Subcommand("sharp")
          with BaseFactorizationOpts
          with SharpDistributions
        addSubcommand(sharp)

        // use custom distributions
        val custom = new Subcommand("custom")
          with BaseFactorizationOpts {
          banner("Generates polynomials with custom distributions")
          val dist = opt[String](
            descr = "JSON string for factor distribution",
            required = true)
        }
        addSubcommand(custom)
      }
      addSubcommand(factorProblems)

      val groebnerProblems = new Subcommand("groebner")
        with CharacteristicOpt
        with SingleOutputOpt {
        banner(
          """
            |
            |Usage: algebench generate groebner --problems problem1 [problem2...] [OPTIONS] output_file
            |Generates a set of Groebner basis problems. To list all available problems run algebench generate groebner --help
            |""".stripMargin)

        val monomialOrder = opt[String](
          name = "order",
          noshort = true,
          default = Some("GREVLEX"),
          descr = "Monomial order to use")

        val problems = opt[List[String]](
          name = "problems",
          descr = "List of problems (separated with spaces)",
          required = true
        )
      }
      addSubcommand(groebnerProblems)
    }
    addSubcommand(generateProblems)

    override protected def onError(e: Throwable): Unit = e match {
      case _: Help if args.contains("groebner") =>
        generateProblems.groebnerProblems.printHelp()
        println("\nAvailable polynomial systems: " + GroebnerBasisData.allSystems.keys.mkString(", "))
        System.exit(0)
      case _ => super.onError(e)
    }

    // solve generated problems
    val solveProblems = new Subcommand("solve")
      with RingsSolver.Cli
      with MathematicaSolver.Cli
      with FormSolver.Cli
      with FermatSolver.Cli
      with SingularSolver.Cli
      with SingleInputOpt
      with SingleOutputOpt {
      banner(
        """
          |Usage: algebench solve [OPTIONS] input_file output_file
          |Solves generated probles with provided solvers and writes timing statistics to output_file
          |""".stripMargin)


      val nThreads = opt[Int](
        descr = "Number of threads for running solvers, if euqual to 1 (default) all solvers will be run sequentially",
        name = "threads",
        noshort = true,
        default = Option(1),
        required = false
      )

      val limit = opt[Int](
        name = "limit",
        noshort = true,
        descr = "Limit number of considered problems",
        default = None
      )

      val timeout = opt[Long](
        name = "timeout",
        noshort = true,
        descr = "Timeout seconds for each tool",
        default = None
      )

      def mkSolvers()(implicit tempFileManager: TempFileManager): Seq[Option[Solver]] =
        Seq(
          mkRingsSolver(),
          mkMathematicaSolver(),
          mkFORMSolver(),
          mkFermatSolver(),
          mkSingularSolver())
          .map(_.map(_.setTimeout(timeout.toOption.map(_.seconds))))
          .map(_.map(_.withLimit(limit.toOption)))
    }
    addSubcommand(solveProblems)

    verify()
  }


  /**
    * The main entrypoint
    */
  def main(args: Array[String]): Unit = {
    import io.circe.parser.decode
    import io.circe.syntax._

    def decodeDistribution(json: String) = {
      decode[PolynomialsDistribution](json) match {
        case Left(err) => throw new RuntimeException("broken json: " + err)
        case Right(dist) => dist
      }
    }

    val runConfiguration = new GlobalConf(args)

    def helpAndReturn(header: String = "", exitCode: Int = 0): Unit = {
      println(
        s"""
           | $header
           | ${runConfiguration.printHelp()}
      """.stripMargin)
      System.exit(exitCode)
    }

    implicit val fileManaged: TempFileManager = TempFileManager(!runConfiguration.keepTempFiles())

    runConfiguration.subcommands match {
      case List() =>
        helpAndReturn()

      case runConfiguration.generateProblems :: generateSpec => {
        implicit val random: Random = new Random(runConfiguration.generateProblems.rndSeed())

        generateSpec match {
          // generate GCD problems
          case runConfiguration.generateProblems.gcdProblem :: method :: Nil =>
            val gcdProblem = runConfiguration.generateProblems.gcdProblem

            val (f1Dist: PolynomialsDistribution, f2Dist: PolynomialsDistribution, gcdDist: PolynomialsDistribution) = method match {
              case p@gcdProblem.uniform =>
                implicit val ring: Ring[IntZ] = Zp(p.characteristic())
                // coefficients distribution
                implicit val coefficients: CoefficientsDistribution = CoefficientsDistribution.uniform(p.bitLength())
                // exponents distribution
                implicit val exponents: ExponentsDistribution = ExponentsDistribution.uniform(p.minDegree(), p.maxDegree())
                // polynomials distribution
                val polys = PolynomialsDistribution.uniform(p.nVariables(), p.getMinSize(), p.getMaxSize())

                (polys, polys, polys)

              case p@gcdProblem.sharp =>
                implicit val ring: Ring[IntZ] = Zp(p.characteristic())
                // coefficients distribution
                implicit val coefficients: CoefficientsDistribution = CoefficientsDistribution.uniform(p.bitLength())
                // exponents distribution
                implicit val exponents: ExponentsDistribution = ExponentsDistribution.sharp(p.totalDegree())
                // polynomials distribution
                val polys = PolynomialsDistribution.uniform(p.nVariables(), p.getMinSize(), p.getMaxSize())

                (polys, polys, polys)

              case p@gcdProblem.custom =>
                (decodeDistribution(p.factor1()), decodeDistribution(p.factor2()), decodeDistribution(p.gcd()), p.output())
            }

            // common opts
            val commonOpts = method.asInstanceOf[BasePolyGenOpts]

            val gcdConfig = PolynomialGCDConfiguration(
              commonOpts.characteristic(),
              defaultVars(commonOpts.nVariables()),
              commonOpts.nProblems(),
              gcdDist, f1Dist, f2Dist)

            val outFile = new File(commonOpts.output())
            if (outFile.exists())
              outFile.delete()

            val writer = new PrintWriter(Files.newBufferedWriter(outFile.toPath, StandardOpenOption.CREATE, StandardOpenOption.WRITE))
            try {
              writer.println("#" + gcdConfig.asInstanceOf[ProblemConfiguration].asJson.noSpaces)
              writer.println(Seq("#problemID", "poly1", "poly2", "expected gcd").mkString("\t"))
              for (iProblem <- 0 until gcdConfig.nProblems) {
                val gcd = gcdConfig.gcd.sample
                val f1 = gcd * gcdConfig.factor_1.sample
                val f2 = gcd * gcdConfig.factor_2.sample
                writer.write(iProblem.toString)
                writer.write("\t")
                writer.write(f1.toString(gcdConfig.variables))
                writer.write("\t")
                writer.write(f2.toString(gcdConfig.variables))
                writer.write("\t")
                writer.write(gcd.toString(gcdConfig.variables))
                writer.write("\n")
                //writer.println(Seq(iProblem, f1.toString(gcdConfig.variables), f2.toString(gcdConfig.variables), gcd.toString(gcdConfig.variables)).mkString("\t"))
              }
            } finally {
              writer.close()
            }

          // generate Factorization problems
          case runConfiguration.generateProblems.factorProblems :: method :: Nil =>
            val factorProblem = runConfiguration.generateProblems.factorProblems

            val dist = (method match {
              case p@factorProblem.uniform =>
                implicit val ring: Ring[IntZ] = Zp(p.characteristic())
                // coefficients distribution
                implicit val coefficients: CoefficientsDistribution = CoefficientsDistribution.uniform(p.bitLength())
                // exponents distribution
                implicit val exponents: ExponentsDistribution = ExponentsDistribution.uniform(p.minDegree(), p.maxDegree())
                // polynomials distribution
                PolynomialsDistribution.uniform(p.nVariables(), p.getMinSize(), p.getMaxSize())

              case p@factorProblem.sharp =>
                implicit val ring: Ring[IntZ] = Zp(p.characteristic())
                // coefficients distribution
                implicit val coefficients: CoefficientsDistribution = CoefficientsDistribution.uniform(p.bitLength())
                // exponents distribution
                implicit val exponents: ExponentsDistribution = ExponentsDistribution.sharp(p.totalDegree())
                // polynomials distribution
                PolynomialsDistribution.uniform(p.nVariables(), p.getMinSize(), p.getMaxSize())

              case p@factorProblem.custom =>
                decodeDistribution(p.dist())
            }).noMonomialContent().squareFree()

            // common opts
            val commonOpts = method.asInstanceOf[BaseFactorizationOpts]

            val factorConfig = PolynomialFactorizationConfiguration(
              commonOpts.characteristic(),
              defaultVars(commonOpts.nVariables()),
              commonOpts.nProblems(),
              IntDistribution.fixed(commonOpts.nFactors()),
              dist, commonOpts.trivial())

            val outFile = new File(commonOpts.output())
            if (outFile.exists())
              outFile.delete()

            val writer = new PrintWriter(Files.newBufferedWriter(outFile.toPath, StandardOpenOption.CREATE, StandardOpenOption.WRITE))
            try {
              writer.println("#" + factorConfig.asInstanceOf[ProblemConfiguration].asJson.noSpaces)
              writer.println(("#problemID" :: "trivial" :: "polynomial" :: (1 to commonOpts.nFactors()).map(i => s"factor_$i").toList).mkString("\t"))
              for (iProblem <- 0 until factorConfig.nProblems) {
                var factors = (1 to commonOpts.nFactors()).map(_ => dist.sample).toList
                val poly = factors.foldLeft(factors.head.createOne)(_ * _)
                if (commonOpts.trivial()) {
                  poly.increment()
                  factors = List(poly)
                }

                writer.write(iProblem.toString)
                writer.write("\t")
                writer.write(commonOpts.trivialFactorization().toString)
                writer.write("\t")
                writer.write(poly.toString(factorConfig.variables))
                factors.foreach { f => writer.write("\t"); writer.write(f.toString(factorConfig.variables)) }
                writer.write("\n")
              }
            } finally {
              writer.close()
            }

          // generate GB problems
          case runConfiguration.generateProblems.groebnerProblems :: Nil =>
            val gbProblems = runConfiguration.generateProblems.groebnerProblems
            val systems = GroebnerBasisData.allSystems

            val problems = gbProblems.problems().map(_.toLowerCase)
            val err = problems.filterNot(systems.contains)
            if (err.nonEmpty) {
              println("Can't recognize the following systems:" + err)
              println("Use --help to view possible GB problems")
              helpAndReturn()
            }

            val outFile = new File(gbProblems.output())
            if (outFile.exists())
              outFile.delete()

            val order = Util.orderDecode(gbProblems.monomialOrder())
            val writer = new PrintWriter(Files.newBufferedWriter(outFile.toPath, StandardOpenOption.CREATE, StandardOpenOption.WRITE))
            try {
              writer.println("#" + GroebnerBasisConfiguration(problems.size).asInstanceOf[ProblemConfiguration].asJson.noSpaces)
              writer.println(s"#problemID\tname\tcharacteristic\torder\tpolynomials")
              problems.zipWithIndex.foreach { case (s, counter) =>
                val gbData = systems(s)
                writer.println(
                  Seq(
                    counter.toString,
                    gbData.name,
                    gbProblems.characteristic().toString(),
                    Util.orderEncode(order),
                    gbData.basis.map(s => gbData.ring.show(s)).mkString(",")
                  ).mkString("\t"))
              }
            } finally {
              writer.close()
            }

          case _ => helpAndReturn(header = s"Unknown method: $generateSpec")
        }
      }

      // Solve the problems
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

        // solve with single system and save intermediate result
        def solve(solver: Solver) = {
          val result = solver.solve(problemData)
          val writer = new PrintWriter(Files.newBufferedWriter(Paths.get(solveCmd.output() + "." + solver.name)))
          try {
            writer.println(s"problemId\tsuccess\ttiming\n")
            result.individualResults
              .foreach { case (i, (s, t)) => writer.println(Seq(i, s, t.toNanos).mkString("\t")) }
          } finally {writer.close() }
          result
        }

        // solve all problems
        val results = (
          if (solveCmd.nThreads() > 1) {
            // solve in parallel
            val parSolvers = solvers.par
            parSolvers.tasksupport = new ForkJoinTaskSupport(
              new scala.concurrent.forkjoin.ForkJoinPool(solveCmd.nThreads()))
            parSolvers
          }
          else
            solvers // solve sequentially
          ).map(s => (s.name, solve(s))).seq

        println("\nShort statistics: ")
        results.foreach { case (soft, stat) =>
          println(s"  $soft: ${Util.prettyDuration(stat.individualResults.map(_._2._2.toNanos).sum.nanoseconds)} ")
        }

        // results as dataset (to write in the file)
        val dataset: Map[Int, Map[String, (Long, Boolean)]] =
          results
            .flatMap { case (name, r) => r.individualResults.toSeq.map(t => ((t._1, name), (t._2._2, t._2._1))) }
            .toMap
            .groupBy(_._1._1)
            .mapValues(_.toSeq.map(t => (t._1._2, (t._2._1.toNanos, t._2._2))).toMap)

        // names of the software that succeed with solution
        val softwareNames = results.map(_._1)

        // writing result to file
        val writer = new PrintWriter(Files.newBufferedWriter(Paths.get(solveCmd.output())))
        try {
          val headerRow = (Seq("problemId") ++ softwareNames.flatMap(s => Seq(s, s + "_success"))).mkString("\t")
          writer.println(headerRow)
          for ((problemId, pResults) <- dataset.toSeq.sortBy(_._1)) {
            val row = (Seq(problemId.toString) ++ softwareNames.flatMap(s => {
              val t = pResults.getOrElse(s, (-1, false))
              Seq(if (t._1 == -1) "NaN" else t._1.toString, t._2)
            })).mkString("\t")
            writer.println(row)
          }
        } finally {
          writer.close()
        }

        // remove tmp individual results
        softwareNames.foreach(s => Files.deleteIfExists(Paths.get(solveCmd.output() + "." + s)))
      case _ => helpAndReturn(header = s"Unknown command: ${runConfiguration.subcommand}")
    }
  }
}
