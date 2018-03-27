package cc.redberry.algebench


import java.io.{BufferedReader, InputStreamReader}

import cc.redberry.algebench.Problems._
import cc.redberry.algebench.Util._
import cc.redberry.rings.scaladsl._
import cc.redberry.rings.scaladsl.syntax._

import scala.collection.mutable
import scala.concurrent.duration._
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
    val cfRing = if (conf.characteristic.isZero) Z else Zp(conf.characteristic)
    implicit val ring: MultivariateRing[IntZ] = MultivariateRing(cfRing, conf.variables)

    // problem set
    val problems = Source.fromFile(inFile).getLines.filter(!_.startsWith("#")).filter(!_.isEmpty)
    // solutions
    val solutions = Source.fromFile(tmpOutput).getLines

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
      println("Solving GCD for Rings")
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
      println(code)


      val ringsProcess = new ProcessBuilder(executable)
        .redirectErrorStream(true)
        .start()

      val start = System.nanoTime()
      ringsProcess.getOutputStream.write(code.getBytes)
      ringsProcess.getOutputStream.flush()
      ringsProcess.getOutputStream.close()
      ringsProcess.waitFor()

      new BufferedReader(new InputStreamReader(ringsProcess.getInputStream())).lines().forEach(l => println(l))

      val totalTime = System.nanoTime() - start

      SolveResult(readResultsForGCD(conf, inFile, ringsTempOut), totalTime.nanoseconds)
    }
  }

  final case class MathematicaSolver() extends Solver {
    override val name: String = "mathematica"

    override def isApplicable(problem: ProblemConfiguration): Boolean = false

    override def solve(problem: ProblemData): SolveResult = ???
  }
}
