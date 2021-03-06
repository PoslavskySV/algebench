package cc.redberry.algebench

import java.io.{OutputStream, PrintWriter}
import java.nio.file.{Files, Paths}

import cc.redberry.algebench.Problems.{GroebnerBasis, GroebnerBasisConfiguration, PolynomialFactorization, PolynomialFactorizationConfiguration, PolynomialGCD, PolynomialGCDConfiguration, ProblemConfiguration, ProblemData}
import cc.redberry.algebench.Solvers._
import cc.redberry.algebench.Util.{TempFileManager, createTempFile}
import org.rogach.scallop.ScallopConfBase

import scala.concurrent.duration._
import scala.io.Source

/**
  * Singular solver
  */
case class SingularSolver(executable: String = "Singular")
                         (implicit tmpFileManager: TempFileManager)
  extends Solver
    with StandardGcdSolver
    with StandardFactorizationSolver
    with StandardGBSolver {
  override val name: String = "Singular"

  override def isApplicable(problem: ProblemConfiguration): Boolean = problem.problemType match {
    case PolynomialGCD | PolynomialFactorization | GroebnerBasis => true
  }

  override def innerSolve(problem: ProblemData, limit: Int): SolveResult = {
    problem match {
      case ProblemData(conf: PolynomialGCDConfiguration, file) => solveGCD(conf, file, limit)
      case ProblemData(conf: PolynomialFactorizationConfiguration, file) => solveFactorization(conf, file, limit)
      case ProblemData(conf: GroebnerBasisConfiguration, file) => solveGB(file, limit)
    }
  }

  private def solveGCD(conf: PolynomialGCDConfiguration, inFile: String, limit: Int): SolveResult = {
    val singularIn = createTempFile("singularGCD.in").getAbsolutePath
    val singularOut = createTempFile("singularGCD.out").getAbsolutePath

    val writer = new PrintWriter(Files.newBufferedWriter(Paths.get(singularIn)))
    try {
      writer.println(
        s"""
           | system("--ticks-per-sec",1000);
           | ring r = ${conf.characteristic},(${conf.variables.mkString(",")}),dp;
           | link output = "$singularOut";
      """.stripMargin)

      for (line <- Source.fromFile(inFile).getLines.filter(!_.startsWith("#")).filter(!_.isEmpty).take(limit)) {
        val tabDelim = line.split("\t")
        val problemId = tabDelim(0)
        val poly1 = tabDelim(1)
        val poly2 = tabDelim(2)

        writer.write("poly a = ")
        writer.write(poly1)
        writer.write(";\n")

        writer.write("poly b = ")
        writer.write(poly2)
        writer.write(";\n")


        writer.println(
          s"""
             | int  t = timer;
             | poly g = gcd(a, b);
             | int elapsed = timer - t;
             | fprintf(output, "%s %s %s", "$problemId", elapsed, g);
        """.stripMargin)
      }

      writer.write("quit;")
    } finally {
      writer.close()
    }

    import scala.sys.process._
    val start = System.nanoTime()
    // overcome huge output of Singular which can't be switched off
    val devNull = new OutputStream {override def write(b: Int): Unit = {}}
    runProcess(Seq(s"$executable", "-q", "-c", s"""< "$singularIn";""") #> devNull)
    val totalTime = System.nanoTime() - start

    // read results
    println(s"Reading $name results...")
    val result = SolveResult(importGcdResults(conf, inFile, singularOut, splitHelper = str => str.split(" "), timeUnit = MILLISECONDS), totalTime.nanoseconds)

    // remove tmp files
    if (tmpFileManager.deleteOnExit) {
      Files.delete(Paths.get(singularIn))
      Files.delete(Paths.get(singularOut))
    }

    result
  }

  private def solveFactorization(conf: PolynomialFactorizationConfiguration, inFile: String, limit: Int): SolveResult = {
    val singularIn = createTempFile("singularFactor.in").getAbsolutePath
    val singularOut = createTempFile("singularFactor.out").getAbsolutePath

    val writer = new PrintWriter(Files.newBufferedWriter(Paths.get(singularIn)))
    try {
      writer.println(
        s"""
           | system("--ticks-per-sec",1000);
           | ring r = ${conf.characteristic},(${conf.variables.mkString(",")}),dp;
           | link output = "$singularOut";
      """.stripMargin)

      for (line <- Source.fromFile(inFile).getLines.filter(!_.startsWith("#")).filter(!_.isEmpty).take(limit)) {
        val tabDelim = line.split("\t")
        val problemId = tabDelim(0)
        val poly = tabDelim(2)

        writer.println(
          s"""
             | poly p = $poly;
             | int  t = timer;
             | list factors = factorize(p);
             | int elapsed = timer - t;
             | factors = factors[1];
             | fprintf(output, "%s %s %s", "$problemId", elapsed, factors);
        """.stripMargin)
      }

      writer.write("quit;")
    } finally {
      writer.close()
    }

    import scala.sys.process._
    val start = System.nanoTime()
    runProcess(Seq(s"$executable", "-q", "--no-warn", "-c", s"""< "$singularIn";"""))
    val totalTime = System.nanoTime() - start

    // read results
    println(s"Reading $name results...")
    val result = SolveResult(importFactorizationResults(conf, inFile, singularOut,
      splitHelper = str => str.replace(",", " ").split(" "),
      timeUnit = MILLISECONDS), totalTime.nanoseconds)

    // remove tmp files
    if (tmpFileManager.deleteOnExit) {
      Files.delete(Paths.get(singularIn))
      Files.delete(Paths.get(singularOut))
    }

    result
  }

  private def solveGB(inFile: String, limit: Int): SolveResult = {
    val singularIn = createTempFile("singularGB.in").getAbsolutePath
    val singularOut = createTempFile("singularGB.out").getAbsolutePath

    val writer = new PrintWriter(Files.newBufferedWriter(Paths.get(singularIn)))
    try {
      for (line <- Source.fromFile(inFile).getLines.filterNot(_.startsWith("#")).filter(_.nonEmpty).take(limit)) {
        val tabDelim = line.split("\t")
        val problemId = tabDelim(0)
        val problemName = tabDelim(1)
        val gbData = GroebnerBasisData(problemName)
        val characteristic = tabDelim(2)
        val order = tabDelim(3).toLowerCase match {
          case "lex" => "lp"
          case "alex" => "rp"
          case "grevlex" => "dp"
          case "glex" => "Dp"
          case x@_ => throw new RuntimeException(s"Singular doesn't support this order: $x")
        }

        writer.println(
          s"""
             | option(redSB);
             | system("--ticks-per-sec",1000);
             | ring the_ring = $characteristic,(${gbData.ring.variables.mkString(",")}),$order;
             | link output = "$singularOut";
             | ideal the_ideal = ${gbData.basis.map(p => gbData.ring.show(p)).mkString(",")};
             | int  time_start = timer;
             | ideal the_basis = std(the_ideal);
             | int elapsed = timer - time_start;
             | fprintf(output, "%s %s %s", "$problemId", "$problemName", elapsed);
        """.stripMargin)
      }

      writer.write("quit;")
    } finally {
      writer.close()
    }

    import scala.sys.process._
    val start = System.nanoTime()
    runProcess(Seq(s"$executable", "-q", "--no-warn", "-c", s"""< "$singularIn";"""))
    val totalTime = System.nanoTime() - start

    // read results
    println(s"Reading $name results...")
    val result = SolveResult(importGBResults(singularOut,
      splitHelper = str => str.replace(",", " ").split(" "),
      timeUnit = MILLISECONDS), totalTime.nanoseconds)

    // remove tmp files
    if (tmpFileManager.deleteOnExit) {
      Files.delete(Paths.get(singularIn))
      Files.delete(Paths.get(singularOut))
    }

    result
  }
}

object SingularSolver {

  /** Command line options for Singular solver */
  trait Cli extends SolverCli {
    this: ScallopConfBase =>

    val withSingular = toggleSoft("Singular", "Singular (https://www.singular.uni-kl.de)")

    val singularExec = optExec("Singular", "singular")

    val singularLimit = optLimit("Singular")

    def mkSingularSolver()(implicit tempFileManager: TempFileManager): Option[Solver] =
      if (withSingular())
        Some(SingularSolver(singularExec()).withLimit(singularLimit.toOption))
      else
        None
  }
}