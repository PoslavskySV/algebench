package cc.redberry.algebench

import java.io.{OutputStream, PrintWriter}
import java.nio.file.{Files, Paths}

import cc.redberry.algebench.Problems.{PolynomialFactorization, PolynomialGCD, PolynomialGCDConfiguration, ProblemConfiguration, ProblemData}
import cc.redberry.algebench.Solvers.{SolveResult, Solver, StandardGcdSolver}
import cc.redberry.algebench.Util.{TempFileManager, createTempFile}

import scala.concurrent.duration._
import scala.io.Source

/**
  *
  */
case class SingularSolver(executable: String = "Singular")
                         (implicit tmpFileManager: TempFileManager)
  extends Solver with StandardGcdSolver {
  override val name: String = "Singular"

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

      for (line <- Source.fromFile(inFile).getLines.filter(!_.startsWith("#")).filter(!_.isEmpty())) {
        val tabDelim = line.split("\t")
        val problemId = tabDelim(0)
        val poly1 = tabDelim(1)
        val poly2 = tabDelim(2)

        writer.println(
          s"""
             | poly a = $poly1;
             | poly b = $poly2;
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
    (Seq(s"$executable", "-q", "-c", s"""< "$singularIn";""") #> devNull) !
    val totalTime = System.nanoTime() - start

    // read results
    val result = SolveResult(readResultsForGCD(conf, inFile, singularOut, separator = " "), totalTime.milliseconds)

    // remove tmp files
    if (tmpFileManager.deleteOnExit) {
      Files.delete(Paths.get(singularIn))
      Files.delete(Paths.get(singularOut))
    }

    result
  }
}
