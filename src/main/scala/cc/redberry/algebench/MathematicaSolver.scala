package cc.redberry.algebench

import java.nio.file.{Files, Paths}

import cc.redberry.algebench.Problems.{PolynomialFactorization, PolynomialGCD, PolynomialGCDConfiguration, ProblemConfiguration, ProblemData}
import cc.redberry.algebench.Solvers.{SolveResult, Solver, StandardGcdSolver}
import cc.redberry.algebench.Util.{TempFileManager, createTempFile}
import scala.concurrent.duration._

/**
  * Mathematica solver
  */
final case class MathematicaSolver(executable: String = "MathematicaScript")
                                  (implicit tmpFileManager: TempFileManager)
  extends Solver with StandardGcdSolver {
  override val name: String = "Mathematica"

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
    println(s"Solving GCD for $name")

    val mmaTempOut = createTempFile("mmaGCD.out").getAbsolutePath
    val code =
      s"""
      modulus = ${conf.characteristic};
      in = OpenRead["$inFile"];
      out = OpenWrite["$mmaTempOut"];
      While[True,
        line = ReadLine[in];
        If[line == EndOfFile, Break[]];
        If[StringStartsQ[line, "#"], Continue[]];

        tabDelim = StringSplit[line, "\t"];

        problemId = tabDelim[[1]];
        poly1 = tabDelim[[2]] // ToExpression;
        poly2 = tabDelim[[3]] // ToExpression;
        gcd = Timing[PolynomialGCD[poly1, poly2, Modulus -> modulus]];

        timeNanos = Round[gcd[[1]] 10^9] // ToString;
        result = StringReplace[gcd[[2]] // InputForm // ToString, " " -> ""];

        WriteString[out,
          problemId <> "\t" <> timeNanos <> "\t" <> result <> "\n"];
        ];
      Close[in];
      Close[out];
      Quit[];
      """

    val mmaProcess = new ProcessBuilder(executable)
      .redirectErrorStream(true)
      .start()

    val start = System.nanoTime()
    mmaProcess.getOutputStream.write(code.getBytes)
    mmaProcess.getOutputStream.flush()
    mmaProcess.getOutputStream.close()
    mmaProcess.waitFor()
    val totalTime = System.nanoTime() - start

    // read results
    val result = SolveResult(readResultsForGCD(conf, inFile, mmaTempOut), totalTime.nanoseconds)

    // remove tmp files
    if (tmpFileManager.deleteOnExit)
      Files.delete(Paths.get(mmaTempOut))

    result
  }
}
