package cc.redberry.algebench


import java.util.concurrent.TimeUnit

import cc.redberry.algebench.Problems._
import cc.redberry.rings.scaladsl._
import cc.redberry.rings.scaladsl.syntax._

import scala.collection.mutable
import scala.concurrent.duration._
import scala.io.Source

/**
  *
  */
object Solvers {


  trait Solver {
    val name: String

    /** Whether software is applicable to given problem */
    def isApplicable(problem: ProblemConfiguration): Boolean

    /** Solves the problem */
    def solve(problem: ProblemData): SolveResult
  }

  trait StandardGcdSolver {
    /**
      * Imports results of GCD solvers from temp file
      *
      * @param conf      configurateion
      * @param inFile    file with problems
      * @param tmpOutput solver output
      */
    def readResultsForGCD(conf: PolynomialGCDConfiguration,
                          inFile: String, tmpOutput: String,
                          timeUnit: TimeUnit = TimeUnit.NANOSECONDS): Map[Int, (Boolean, FiniteDuration)] = {
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

        result += id -> ((actualGCD % expectedGCD).isZero, FiniteDuration(solution(1).toLong, timeUnit))
      }
      result.toMap
    }
  }
  final case class SolveResult(individualResults: Map[Int, (Boolean, FiniteDuration)],
                               totalTime: FiniteDuration)
}
