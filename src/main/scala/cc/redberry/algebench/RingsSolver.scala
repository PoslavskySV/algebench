package cc.redberry.algebench

import java.io.PrintWriter
import java.nio.file.{Files, Paths}

import cc.redberry.algebench.Problems.{GroebnerBasis, GroebnerBasisConfiguration, PolynomialFactorization, PolynomialFactorizationConfiguration, PolynomialGCD, PolynomialGCDConfiguration, ProblemConfiguration, ProblemData}
import cc.redberry.algebench.Solvers._
import cc.redberry.algebench.Util.{TempFileManager, createTempFile}
import cc.redberry.rings.scaladsl._
import org.rogach.scallop.ScallopConfBase

import scala.concurrent.duration._
import scala.io.Source

/**
  * Rings solver
  */
case class RingsSolver(executable: String = "rings.repl")
                      (implicit tmpFileManager: TempFileManager)
  extends Solver
    with StandardGcdSolver
    with StandardFactorizationSolver
    with StandardGBSolver {
  override val name: String = "Rings"

  /** Whether software is applicable to given problem */
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
    val ringString = if (conf.characteristic.isZero) "Z" else s"Zp(${conf.characteristic})"
    val variables = conf.variables.map(v => s""""$v"""").mkString(",")

    val ringsOut = createTempFile("ringsGCD.out").getAbsolutePath
    val ringsTmp = createTempFile("ringsTmp.sc").getAbsolutePath
    val code =
      s"""
         |
         | println("... compiled")
         |
         | import java.io._
         | import scala.io._
         | val output = new PrintWriter(new File("$ringsOut"))
         | try {
         | implicit val ring: MultivariateRing[IntZ] = MultivariateRing($ringString, Array($variables))
         | for (line <- Source.fromFile("$inFile").getLines.filter(!_.startsWith("#")).filter(!_.isEmpty()).take($limit)) {
         |   val tabDelim = line.split("\\t")
         |   val problemId = tabDelim(0)
         |
         |   val poly1 = ring(tabDelim(1))
         |   val poly2 = ring(tabDelim(2))
         |   val expectedGCD = ring(tabDelim(3))
         |
         |   val start = System.nanoTime()
         |   val gcd = ring.gcd(poly1, poly2)
         |   val elapsed = System.nanoTime() - start
         |
         |   output.println(Seq(problemId, elapsed, ring.show(gcd)).mkString("\\t"))
         |   }
         | } finally {
         |    output.close()
         | }
         |
         | exit
      """.stripMargin

    Files.write(Paths.get(ringsTmp), java.util.Arrays.asList(code.split("\n"): _*))
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

  private def solveFactorization(conf: PolynomialFactorizationConfiguration, inFile: String, limit: Int): SolveResult = {
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
        for (line <- Source.fromFile("$inFile").getLines.filter(!_.startsWith("#")).filter(!_.isEmpty()).take($limit)) {
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

    Files.write(Paths.get(ringsTmp), java.util.Arrays.asList(code.split("\n"): _*))
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

  private def solveGB(inFile: String, limit: Int): SolveResult = {
    val ringsOut = createTempFile("ringsGB.out").getAbsolutePath
    val ringsTmp = createTempFile("ringsTmp.sc").getAbsolutePath

    val writer = new PrintWriter(Files.newBufferedWriter(Paths.get(ringsTmp)))
    try {
      writer.println(
        s"""
           |
           | println("... compiled")
           |
           | import java.io._
           | import scala.io._
           | val output = new PrintWriter(new File("$ringsOut"))
           | try {
           | output.println("#problemId\tname\ttime")
       """.stripMargin)

      for (line <- Source.fromFile(s"$inFile").getLines.filter(!_.startsWith("#")).filter(!_.isEmpty).take(limit)) {
        val tabDelim = line.split("\t")
        val problemId = tabDelim(0)
        val name = tabDelim(1)
        val characteristic = Z(tabDelim(2))
        val order = tabDelim(3)
        val gbData = GroebnerBasisData(name)

        val ringString = if (characteristic.isZero) "Z" else s"Zp($characteristic)"

        val variables = gbData.ring.variables.map(v => s""""$v"""").mkString(",")
        writer.println(
          s"""
             |{
             |  implicit val ring: MultivariateRing[IntZ] = MultivariateRing($ringString, Array($variables))
             |  val basis = Seq(${gbData.basis.map(p => gbData.ring.show(p)).map(p => "ring(\"" + p + "\")").mkString(",")})
             |  val start = System.nanoTime()
             |  val gb = Ideal(basis, $order)
             |  val elapsed = System.nanoTime() - start
             |
             |  output.println(Seq("$problemId", "$name", elapsed).mkString("\\t"))
             |}
           """.stripMargin)
      }

      writer.println(
        """
          |} finally {
          | output.close()
          |}
        """.stripMargin)

    } finally {
      writer.close()
    }

    println(s"Running $name process...")
    import scala.sys.process._
    val start = System.nanoTime()
    s"$executable $ringsTmp" !
    val totalTime = System.nanoTime() - start

    // read results
    println(s"Reading $name results...")

    val result = SolveResult(importGBResults(ringsOut), totalTime.nanoseconds)

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
  trait Cli extends SolverCli {
    this: ScallopConfBase =>

    val withRings = toggleSoft("Rings", "Rings (http://ringsalgebra.io)")

    val ringsExec = optExec("Rings", "rings.repl")

    val ringsLimit = optLimit("Rings")

    def mkRingsSolver()(implicit tempFileManager: TempFileManager): Option[Solver] =
      if (withRings())
        Some(RingsSolver(ringsExec()).withLimit(ringsLimit.toOption))
      else
        None
  }
}