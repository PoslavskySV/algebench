package cc.redberry.algebench


import java.util.concurrent.TimeUnit

import cc.redberry.algebench.Problems._
import cc.redberry.rings.scaladsl._
import cc.redberry.rings.scaladsl.syntax._
import org.rogach.scallop.ScallopConfBase

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.io.Source
import scala.sys.process._

/**
  * Abstraction for solvers
  */
object Solvers {
  /** Solver interface */
  trait Solver {
    /** Displayed name */
    val name: String

    /** Whether software is applicable to given problem */
    def isApplicable(problem: ProblemConfiguration): Boolean

    private var defaultLimit: Int = Int.MaxValue

    final def withLimit(limit: Int): Solver = {
      defaultLimit = limit
      this
    }

    final def withLimit(limit: Option[Int]): Solver =
      limit match {
        case None => this
        case Some(l) => withLimit(l)
      }

    private var timeout: Option[Duration] = None

    final def setTimeout(timeout: Duration): Solver = {
      this.timeout = Some(timeout)
      this
    }

    final def setTimeout(timeout: Option[Duration]): Solver = {
      this.timeout = timeout
      this
    }

    /** Solves the problem set
      *
      * @param problemData the problem set
      **/
    final def solve(problemData: ProblemData): SolveResult = solve(problemData, defaultLimit)

    /** Solves the problem set
      *
      * @param problemData the problem set
      * @param limit       limit number of problems in the set
      **/
    final def solve(problemData: ProblemData, limit: Int): SolveResult = {
      println(s"Running $name...")
      val result = innerSolve(problemData, limit = limit)
      println(s"...     $name total benchmarking time: " + Util.prettyDuration(result.individualResults.map(_._2._2.toNanos).sum.nanoseconds) + "")
      result
    }

    /** Solves the problem */
    protected def innerSolve(problem: ProblemData, limit: Int): SolveResult

    /** runs process */
    def runProcess(pb: String): Int = runProcess(Process(pb))

    /** runs process */
    def runProcess(pb: ProcessBuilder): Int = {
      println(s"Running $name process...")
      val start = System.nanoTime()
      val result = timeout match {
        case None => pb !
        case Some(t) => runProcessWithTimeout(pb, t)
      }
      val elapsed = System.nanoTime() - start
      println(s"...    $name process complete in ${Util.prettyDuration(elapsed.nanoseconds)}")

      result
    }

    /** runs process with timeout */
    def runProcessWithTimeout(pb: ProcessBuilder, timeout: Duration) = {
      // start asynchronously
      val proc: Process = pb.run()
      // wrap in blocking Future
      val future: Future[Int] = Future(blocking(proc.exitValue()))

      try {
        Await.result(future, timeout)
      } catch {
        case _: TimeoutException =>
          println(s"$name stopped due timeout")
          proc.destroy()
          proc.exitValue()
      }
    }
  }

  /**
    * Solver for GCD problems
    */
  trait StandardGcdSolver {
    /**
      * Imports results of GCD solvers from solver output
      *
      * @param conf         configuration
      * @param problemsFile file with problems
      * @param solverOutput solver output
      */
    def importGcdResults(conf: PolynomialGCDConfiguration,
                         problemsFile: String,
                         solverOutput: String,

                         splitHelper: String => Array[String] = s => s.split("\t"),
                         parseHelper: (String, MultivariateRing[IntZ]) => MultivariatePolynomial[IntZ] = null,
                         timeUnit: TimeUnit = TimeUnit.NANOSECONDS): Map[Int, (Boolean, FiniteDuration)] = {
      // coefficient ring
      val cfRing = if (conf.characteristic.isZero) Z else Zp(conf.characteristic)
      // polynomial ring
      implicit val ring: MultivariateRing[IntZ] = MultivariateRing(cfRing, conf.variables)

      // parse polynomials
      val parser: String => MultivariatePolynomial[IntZ] =
        if (parseHelper != null)
          s => parseHelper(s, ring)
        else
          s => ring(s)

      // problem set
      val problems = Source.fromFile(problemsFile).getLines.filter(!_.startsWith("#")).filter(!_.isEmpty)
      // solutions
      val solutions = Source.fromFile(solverOutput).getLines

      // individual results
      val result = mutable.Map.empty[Int, (Boolean, FiniteDuration)]
      while (problems.hasNext && solutions.hasNext) {
        try {
          val problem = problems.next().split("\t")
          val solution = splitHelper(solutions.next())

          val id = problem(0).toInt
          // same problem IDs
          assert(id == solution(0).toInt)

          val expectedGCD = parser(problem(3))
          val actualGCD = parser(solution(2))

          result += id -> ((actualGCD % expectedGCD).isZero, FiniteDuration(solution(1).toLong, timeUnit))
        } catch {
          case _: Throwable => // broken line
        }
      }
      result.toMap
    }
  }

  /**
    * Solver for factorization problems
    */
  trait StandardFactorizationSolver {
    /**
      * Imports results of GCD solvers from solver output
      *
      * @param conf         configuration
      * @param problemsFile file with problems
      * @param solverOutput solver output
      */
    def importFactorizationResults(conf: PolynomialFactorizationConfiguration,
                                   problemsFile: String,
                                   solverOutput: String,

                                   splitHelper: String => Array[String] = s => s.split("\t"),
                                   parseHelper: (String, MultivariateRing[IntZ]) => MultivariatePolynomial[IntZ] = null,
                                   timeUnit: TimeUnit = TimeUnit.NANOSECONDS): Map[Int, (Boolean, FiniteDuration)] = {
      // coefficient ring
      val cfRing = if (conf.characteristic.isZero) Z else Zp(conf.characteristic)
      // polynomial ring
      implicit val ring: MultivariateRing[IntZ] = MultivariateRing(cfRing, conf.variables)

      // parse polynomials
      val parser: String => MultivariatePolynomial[IntZ] =
        if (parseHelper != null)
          s => parseHelper(s, ring)
        else
          s => ring(s)

      // problem set
      val problems = Source.fromFile(problemsFile).getLines.filter(!_.startsWith("#")).filter(!_.isEmpty)
      // solutions
      val solutions = Source.fromFile(solverOutput).getLines

      // individual results
      val result = mutable.Map.empty[Int, (Boolean, FiniteDuration)]
      while (problems.hasNext && solutions.hasNext) {
        try {
          val problem = problems.next().split("\t")
          val solution = splitHelper(solutions.next())

          val id = problem(0).toInt
          // same problem IDs
          assert(id == solution(0).toInt)

          val expectedFactors = problem.drop(3).map(ring.apply).filterNot(p => p.isConstant || p.isMonomial).length
          val actualFactors = solution.drop(2).map(ring.apply).filterNot(p => p.isConstant || p.isMonomial).length

          result += id -> (expectedFactors == actualFactors, FiniteDuration(solution(1).toLong, timeUnit))
        } catch {
          case _: Throwable => // broken line
        }
      }
      result.toMap
    }
  }

  /**
    * Solver for GCD problems
    */
  trait StandardGBSolver {
    /**
      * Imports results of GB solvers from solver output
      */
    def importGBResults(solverOutput: String,
                        splitHelper: String => Array[String] = s => s.split("\t"),
                        timeUnit: TimeUnit = TimeUnit.NANOSECONDS): Map[Int, (Boolean, FiniteDuration)] =
      Source.fromFile(solverOutput).getLines().filterNot(_.startsWith("#"))
        .map(splitHelper)
        .map(s => (s(0).toInt, (true, FiniteDuration(s(2).toLong, timeUnit))))
        .toMap
  }
  /**
    *
    * @param individualResults result for each problem: problemId => (success, elapsedTime)
    * @param totalTime         total time including reading the data etc
    */
  final case class SolveResult(individualResults: Map[Int, (Boolean, FiniteDuration)],
                               totalTime: FiniteDuration)


  trait SolverCli {
    this: ScallopConfBase =>

    final def toggleSoft(shortName: String, desr: String) = toggle(
      name = shortName.toLowerCase,
      noshort = true,
      default = Some(false),
      descrYes = desr)

    final def optExec(shortName: String, default: String) = opt[String](
      name = s"${shortName.toLowerCase}-exec",
      descr = s"Path to ${shortName} executable",
      default = Some(default),
      noshort = true
    )

    final def optLimit(shortName: String) = opt[Int](
      name = s"${shortName.toLowerCase}-limit",
      descr = s"Limit number of problems to solve by $shortName",
      default = None,
      noshort = true
    )
  }
}
