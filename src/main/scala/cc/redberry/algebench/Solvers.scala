package cc.redberry.algebench

import java.nio.file.Files

import cc.redberry.algebench.Problems._
import cc.redberry.algebench.Util._
import cc.redberry.rings.scaladsl._
import cc.redberry.rings.scaladsl.syntax._

import scala.collection.mutable
import scala.concurrent.duration.{Duration, _}
import scala.io.Source

/**
  *
  */
object Solvers {

  /**
    * Imports results of GCD solvers from temp file
    *
    * @param conf      configurateion
    * @param inFile    file with problems
    * @param tmpOutput solver output
    */
  private def readResultsForGCD(conf: PolynomialGCDConfiguration,
                                inFile: String, tmpOutput: String): Map[Int, (Boolean, FiniteDuration)] = {
    implicit val ring: MultivariateRing[IntZ] = MultivariateRing(Zp(conf.characteristic), conf.variables)

    // problem set
    val problems = Source.fromFile(inFile).getLines.filter(!_.startsWith("#"))
    // solutions
    val solutions = Source.fromFile(tmpOutput).getLines.filter(!_.startsWith("#"))

    val result = mutable.Map.empty[Int, (Boolean, FiniteDuration)]
    while (problems.hasNext && solutions.hasNext) {
      val problem = problems.next().split("\t")
      val solution = solutions.next().split("\t")

      val id = problem(0).toInt
      // same problem IDs
      assert(id == solution(0).toInt)

      val expectedGCD = ring(problem(3))
      val actualGCD = ring(solution(2))

      result += id -> ((actualGCD % expectedGCD).isZero, solution(1).toLong.nanoseconds)
    }
    result.toMap
  }

  sealed trait Solver {
    val name: String

    def isApplicable(problem: ProblemConfiguration): Boolean

    def solve(problem: ProblemData): SolveResult
  }

  final case class SolveResult(individualResults: Map[Int, (Boolean, FiniteDuration)],
                               totalTime: FiniteDuration)


  case class RingsSolver(executable: String = "rings.repl")(implicit tmpFileManager: TempFileManager) extends Solver {
    override val name: String = "Rings"

    override def isApplicable(problem: ProblemConfiguration): Boolean = problem.problemType match {
      case PolynomialGCD | PolynomialFactorization => true
    }

    override def solve(problem: ProblemData): SolveResult = {
      problem match {
        case ProblemData(conf: PolynomialGCDConfiguration, file) => solveGCD(conf, file)
        case _ => ???
      }
    }

    private def solveGCD(conf: PolynomialGCDConfiguration, inFile: String): SolveResult = {
      val characteristic = conf.characteristic.toString
      val variables = conf.variables.mkString(",")

      val ringsTempOut = createTempFile("ringsGCD.out").getAbsolutePath
      val code =
        s"""
      val output = new PrintWriter(new File($ringsTempOut))
      try {
        implicit val ring: MultivariateRing[IntZ] = MultivariateRing(Zp(new IntZ($characteristic)), Array($variables))
        for (line <- Source.fromFile($inFile).getLines.filter(!_.startsWith("#"))) {
          val tabDelim = line.split("\t")
          val problemId = tabDelim(0)

          val poly1 = ring(tabDelim(1))
          val poly2 = ring(tabDelim(2))
          val expectedGCD = ring(tabDelim(3))

          val start = System.nanoTime()
          val gcd = ring.gcd(poly1, poly2)
          val elapsed = System.nanoTime() - start

          output.println(Seq(problemId, elapsed, ring.show(gcd)).mkString("\t"))
        }
      } finally {
        output.close()
      }
      exit()
      """

      val ringsTempIn = createTempFile("ringsGCD.in")
      Files.write(ringsTempIn.toPath, java.util.Collections.singleton(code))

      val start = System.nanoTime()
      val ringsProcess = new ProcessBuilder(executable, s"<${ringsTempIn.getAbsolutePath}", ">/dev/null", "2>&1").start()
      ringsProcess.waitFor()
      val totalTime = System.nanoTime() - start

      SolveResult(readResultsForGCD(conf, inFile, ringsTempOut), totalTime.nanoseconds)
    }
  }
}
