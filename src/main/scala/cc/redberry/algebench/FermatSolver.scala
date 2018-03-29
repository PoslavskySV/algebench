package cc.redberry.algebench

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}
import java.util.concurrent.TimeUnit

import cc.redberry.algebench.Problems.{PolynomialGCD, PolynomialGCDConfiguration, ProblemConfiguration, ProblemData}
import cc.redberry.algebench.Solvers.{SolveResult, Solver, StandardGcdSolver}
import cc.redberry.algebench.Util.TempFileManager
import cc.redberry.rings.scaladsl._
import cc.redberry.rings.scaladsl.syntax._
import org.rogach.scallop.ScallopConfBase

import scala.concurrent.duration._
import scala.io.Source

/**
  * Fermat solver
  */
case class FermatSolver(executable: String = "fer64")
                       (implicit tmpFileManager: TempFileManager)
  extends Solver with StandardGcdSolver {
  override val name: String = "Fermat"

  override def isApplicable(problem: ProblemConfiguration): Boolean = problem.problemType match {
    case PolynomialGCD => true
    case _ => false
  }

  override def innerSolve(problem: ProblemData): SolveResult = {
    problem match {
      case ProblemData(conf: PolynomialGCDConfiguration, file) => solveGCD(conf, file)
      case _ => ???
    }
  }

  private def solveGCD(conf: PolynomialGCDConfiguration, inFile: String): SolveResult = {
    // prepare input FORM file
    val ferIn = tmpFileManager.createTempFile("ferIn").getAbsolutePath
    val ferOut = tmpFileManager.createTempFile("ferOut").getAbsolutePath

    val ferWriter = new PrintWriter(Files.newBufferedWriter(Paths.get(ferIn)))
    try {
      ferWriter.println("&N; &t: &E:")
      if (!conf.characteristic.isZero)
        ferWriter.println(s"&(p = ${conf.characteristic}): ")

      // &(J=x1): &(J=x2): &(J=x3):
      ferWriter.println(conf.variables.map(v => s"&(J=$v):").mkString(" "))

      ferWriter.println("!('start_here'):")
      ferWriter.println("!;")

      for (line <- Source.fromFile(inFile).getLines.filter(!_.startsWith("#")).filter(!_.isEmpty())) {
        val tabDelim = line.split("\t")
        val problemId = tabDelim(0)
        val poly1 = tabDelim(1)
        val poly2 = tabDelim(2)

        ferWriter.println(s"a$problemId := $poly1:")
        ferWriter.println(s"b$problemId := $poly2:")
        ferWriter.println(s"start := &_m(&T): g$problemId := GCD(a$problemId,b$problemId): end := &_m(&T): elapsed := &_m(end - start):")
        ferWriter.println(s"!('problem_$problemId'): !('tab'): !(elapsed): !('tab'): !(g$problemId):")
        ferWriter.println("!;")

        ferWriter.println()
      }
      ferWriter.println("&q:")
    } finally {
      ferWriter.close()
    }

    println(s"Running $name process...")
    import sys.process._
    val start = System.nanoTime()
    // omg, I wish my eyes not seen this... will move this to Scala in future
    (s"$executable"
      #< new File(ferIn)
      #| Seq("awk", """/`$/{printf "%s ", $0; next} {print}""")
      #| Seq("sed", "s:`::g")
      #| "sed s:>::g"
      #| "grep problem_"
      #| Seq("sed", "s: ::g")
      #| Seq("sed", "s:tab: :g")
      #| Seq("tr", "[:blank:]", "\\t")
      #| "sed s:problem_::"
      #> new File(ferOut)) !
    val totalTime = System.nanoTime() - start

    // read results
    println(s"Reading $name results...")
    val result = SolveResult(
      importGcdResults(conf, inFile, ferOut,
        parseHelper = (s, r) => FermatSolver.parseFermat(s)(r),
        timeUnit = TimeUnit.MILLISECONDS),
      totalTime.nanoseconds)

    // remove tmp files
    if (tmpFileManager.deleteOnExit) {
      Files.deleteIfExists(Paths.get(ferIn))
      Files.deleteIfExists(Paths.get(ferOut))
    }

    result
  }
}

object FermatSolver {
  def parseFermat(str: String)(implicit ring: MultivariateRing[IntZ]): MultivariatePolynomial[IntZ] = {
    parseRecursiveForm0(str
      .replace(" ", "")
      .replaceAll("(\\))([a-zA-Z])", "$1*$2")
      .replaceAll("(\\d+)([a-zA-Z])", "$1*$2"))
  }

  def parseRecursiveForm(str: String)(implicit ring: MultivariateRing[IntZ]): MultivariatePolynomial[IntZ] =
    parseRecursiveForm0(str.replace(" ", ""))

  private def parseRecursiveForm0(str: String)(implicit ring: MultivariateRing[IntZ]): MultivariatePolynomial[IntZ] = {
    if (str.matches("^\\(.*\\)$"))
      return parseRecursiveForm0(str.substring(1, str.length - 1))

    if (!str.contains("("))
      return ring(str)

    var result = ring.getZero

    var brLevel = 0
    var iPrev = 0
    for (i <- 0 until str.length) {
      if (str(i) == '(')
        brLevel += 1

      if (str(i) == ')')
        brLevel -= 1

      if ((str(i) == '+' || str(i) == '-') && brLevel == 0 && i != 0) {
        result += parseTerm0(str.substring(iPrev, i))
        iPrev = i
      }
    }
    result += parseTerm0(str.substring(iPrev, str.length))
    result
  }

  private def parseTerm0(str: String)(implicit ring: MultivariateRing[IntZ]): MultivariatePolynomial[IntZ] = {
    if (str.matches("^\\(.*\\)$"))
      return parseTerm0(str.substring(1, str.length - 1))
    if (str.startsWith("+"))
      return parseTerm0(str.substring(1))
    if (str.startsWith("-"))
      return parseTerm0(str.substring(1)).negate()

    if (!str.contains("("))
      return ring(str)

    var result = ring.getOne

    var brLevel = 0
    var iPrev = 0
    for (i <- 0 until str.length) {
      if (str(i) == '(')
        brLevel += 1

      if (str(i) == ')')
        brLevel -= 1

      if (str(i) == '*' && brLevel == 0 && i != 0) {
        result *= parseRecursiveForm0(str.substring(iPrev, i))
        iPrev = i + 1
      }
    }
    result *= parseRecursiveForm0(str.substring(iPrev, str.length))
    result
  }

  /** Command line options for Fermat solver */
  trait Cli {
    this: ScallopConfBase =>

    val withFermat = toggle(
      name = "fermat",
      default = Some(false),
      descrYes = "Fermat (http://home.bway.net/lewis/)"
    )

    val fermatExec = opt[String](
      name = "fermat-exec",
      descr = "Path to Fermat executable",
      default = Some("fer64"),
      noshort = true
    )

    def mkFermatSolver()(implicit tempFileManager: TempFileManager): Option[FermatSolver] =
      if (withFermat())
        Some(FermatSolver(fermatExec()))
      else
        None
  }
}