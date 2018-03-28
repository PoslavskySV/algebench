package cc.redberry.algebench

import java.nio.file.{Files, Paths}

import cc.redberry.algebench.Problems.{PolynomialFactorization, PolynomialGCD, PolynomialGCDConfiguration, ProblemConfiguration, ProblemData}
import cc.redberry.algebench.Solvers.{SolveResult, Solver, StandardGcdSolver}
import cc.redberry.algebench.Util.{TempFileManager, createTempFile}
import scala.concurrent.duration._

/**
  * Rings solver
  */
case class RingsSolver(executable: String = "rings.repl")
                      (implicit tmpFileManager: TempFileManager)
  extends Solver with StandardGcdSolver {
  override val name: String = "Rings"

  /** Whether software is applicable to given problem */
  override def isApplicable(problem: ProblemConfiguration): Boolean = problem.problemType match {
    case PolynomialGCD | PolynomialFactorization => true
  }

  override def innerSolve(problem: ProblemData): SolveResult = {
    problem match {
      case ProblemData(conf: PolynomialGCDConfiguration, file) => solveGCD(conf, file)
      case _ => ???
    }
  }

  private def solveGCD(conf: PolynomialGCDConfiguration, inFile: String): SolveResult = {
    val ringString = if (conf.characteristic.isZero) "Z" else s"Zp(${conf.characteristic})"
    val variables = conf.variables.map(v => s""""$v"""").mkString(",")


    val ringsTempOut = createTempFile("ringsGCD.out").getAbsolutePath

    val code =
      s"""
      import java.io._
      import scala.io._
      val output = new PrintWriter(new File("$ringsTempOut"))
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

    val ringsProcess = new ProcessBuilder(executable)
      .redirectErrorStream(true)
      .start()

    val start = System.nanoTime()
    ringsProcess.getOutputStream.write(code.getBytes)
    ringsProcess.getOutputStream.flush()
    ringsProcess.getOutputStream.close()
    ringsProcess.waitFor()
    val totalTime = System.nanoTime() - start

    // read results
    val result = SolveResult(readResultsForGCD(conf, inFile, ringsTempOut), totalTime.nanoseconds)

    // remove tmp files
    if (tmpFileManager.deleteOnExit)
      Files.delete(Paths.get(ringsTempOut))

    result
  }
}