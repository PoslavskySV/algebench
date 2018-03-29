package cc.redberry.algebench

import java.nio.file.{Files, Paths}
import java.util

import cc.redberry.algebench.Problems.{PolynomialFactorization, PolynomialFactorizationConfiguration, PolynomialGCD, PolynomialGCDConfiguration, ProblemConfiguration, ProblemData}
import cc.redberry.algebench.Solvers.{SolveResult, Solver, StandardFactorizationSolver, StandardGcdSolver}
import cc.redberry.algebench.Util.{TempFileManager, createTempFile}
import org.rogach.scallop.ScallopConfBase

import scala.concurrent.duration._

/**
  * Rings solver
  */
case class RingsSolver(executable: String = "rings.repl")
                      (implicit tmpFileManager: TempFileManager)
  extends Solver
    with StandardGcdSolver
    with StandardFactorizationSolver {
  override val name: String = "Rings"

  /** Whether software is applicable to given problem */
  override def isApplicable(problem: ProblemConfiguration): Boolean = problem.problemType match {
    case PolynomialGCD | PolynomialFactorization => true
  }

  override def innerSolve(problem: ProblemData): SolveResult = {
    problem match {
      case ProblemData(conf: PolynomialGCDConfiguration, file) => solveGCD(conf, file)
      case ProblemData(conf: PolynomialFactorizationConfiguration, file) => solveFactorization(conf, file)
    }
  }

  private def solveGCD(conf: PolynomialGCDConfiguration, inFile: String): SolveResult = {
    val ringString = if (conf.characteristic.isZero) "Z" else s"Zp(${conf.characteristic})"
    val variables = conf.variables.map(v => s""""$v"""").mkString(",")

    val ringsOut = createTempFile("ringsGCD.out").getAbsolutePath
    val ringsTmp = createTempFile("ringsTmp.sc").getAbsolutePath

    val code =
      s"""
      import java.io._
      import scala.io._
      val output = new PrintWriter(new File("$ringsOut"))
      try {
        implicit val ring: MultivariateRing[IntZ] = MultivariateRing($ringString, Array($variables))
        for (line <- Source.fromFile("$inFile").getLines.filter(!_.startsWith("#")).filter(!_.isEmpty())) {
          val tabDelim = line.split("\\t")
          val problemId = tabDelim(0)

          val poly1 = ring(tabDelim(1))
          val poly2 = ring(tabDelim(2))
          val expectedGCD = ring(tabDelim(3))

          val start = System.nanoTime()
          val gcd = ring.gcd(poly1, poly2)
          val elapsed = System.nanoTime() - start

          output.println(Seq(problemId, elapsed, ring.show(gcd)).mkString("\\t"))
        }
      } finally {
        output.close()
      }
      exit
      """

    Files.write(Paths.get(ringsTmp), util.Arrays.asList(code.split("\n"): _*))
    println(s"Running $name process...")
    import scala.sys.process._
    val start = System.nanoTime()
    s"$executable $ringsTmp" !
    val totalTime = System.nanoTime() - start

    // read results
    println(s"Reading $name results...")
    val result = SolveResult(importGcdResults(conf, inFile, ringsOut), totalTime.nanoseconds)

    // remove tmp files
    if (tmpFileManager.deleteOnExit) {
      Files.delete(Paths.get(ringsOut))
      Files.delete(Paths.get(ringsTmp))
    }

    result
  }

  private def solveFactorization(conf: PolynomialFactorizationConfiguration, inFile: String): SolveResult = {
    val ringString = if (conf.characteristic.isZero) "Z" else s"Zp(${conf.characteristic})"
    val variables = conf.variables.map(v => s""""$v"""").mkString(",")

    val ringsOut = createTempFile("ringsFactor.out").getAbsolutePath
    val ringsTmp = createTempFile("ringsTmp.sc").getAbsolutePath

    val code =
      s"""
      import java.io._
      import scala.io._
      val output = new PrintWriter(new File("$ringsOut"))
      try {
        implicit val ring: MultivariateRing[IntZ] = MultivariateRing($ringString, Array($variables))
        for (line <- Source.fromFile("$inFile").getLines.filter(!_.startsWith("#")).filter(!_.isEmpty())) {
          val tabDelim = line.split("\\t")
          val problemId = tabDelim(0)

          val poly = ring(tabDelim(2))

          val start = System.nanoTime()
          val factors = ring.factor(poly).map(_._1)
          val elapsed = System.nanoTime() - start

          output.println( (Seq(problemId, elapsed) ++ factors.map(f => ring.show(f)) ).mkString("\\t"))
        }
      } finally {
        output.close()
      }
      exit
      """

    Files.write(Paths.get(ringsTmp), util.Arrays.asList(code.split("\n"): _*))
    println(s"Running $name process...")
    import scala.sys.process._
    val start = System.nanoTime()
    s"$executable $ringsTmp" !
    val totalTime = System.nanoTime() - start

    // read results
    println(s"Reading $name results...")
    val result = SolveResult(importFactorizationResults(conf, inFile, ringsOut), totalTime.nanoseconds)

    // remove tmp files
    if (tmpFileManager.deleteOnExit) {
      Files.delete(Paths.get(ringsOut))
      Files.delete(Paths.get(ringsTmp))
    }

    result
  }
}

object RingsSolver {

  /** Command line options for Rings solver */
  trait Cli {
    this: ScallopConfBase =>

    val withRings = toggle(
      name = "rings",
      noshort = true,
      default = Some(false),
      descrYes = "Rings (http://ringsalgebra.io)"
    )

    val ringsExec = opt[String](
      name = "rings-exec",
      descr = "Path to Rings executable",
      default = Some("rings.repl"),
      noshort = true
    )

    def mkRingsSolver()(implicit tempFileManager: TempFileManager): Option[RingsSolver] =
      if (withRings())
        Some(RingsSolver(ringsExec()))
      else
        None
  }
}