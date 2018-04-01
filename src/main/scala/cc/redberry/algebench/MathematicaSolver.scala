package cc.redberry.algebench

import java.nio.file.{Files, Paths}
import java.util

import cc.redberry.algebench.Problems.{PolynomialFactorizationConfiguration, PolynomialGCDConfiguration, ProblemConfiguration, ProblemData}
import cc.redberry.algebench.Solvers._
import cc.redberry.algebench.Util.{TempFileManager, createTempFile}
import org.rogach.scallop.ScallopConfBase

import scala.concurrent.duration._

/**
  * Wolfram Mathematica solver
  */
final case class MathematicaSolver(executable: String = "wolframscript")
                                  (implicit tmpFileManager: TempFileManager)
  extends Solver
    with StandardGcdSolver
    with StandardFactorizationSolver {
  override val name: String = "Mathematica"

  override def isApplicable(problem: ProblemConfiguration): Boolean = problem match {
    case _: PolynomialGCDConfiguration => true
    case p: PolynomialFactorizationConfiguration => p.characteristic.isZero
    case _ => false
  }

  override def innerSolve(problem: ProblemData, limit: Int): SolveResult = {
    problem match {
      case ProblemData(conf: PolynomialGCDConfiguration, file) => solveGCD(conf, file, limit)
      case ProblemData(conf: PolynomialFactorizationConfiguration, file) => solveFactorization(conf, file, limit)
    }
  }

  private def solveGCD(conf: PolynomialGCDConfiguration, inFile: String, limit: Int): SolveResult = {
    val mmaOut = createTempFile("mmaGCD.out").getAbsolutePath
    val mmaTmp = createTempFile("mmaTmp.ws").getAbsolutePath
    val code =
      s"""
         | modulus = ${conf.characteristic};
         | in = OpenRead["$inFile"];
         | out = OpenWrite["$mmaOut"];
         | count = 0;
         | While[True,
         |   line = ReadLine[in];
         |   If[line == EndOfFile, Break[]];
         |   If[StringStartsQ[line, "#"], Continue[]];
         |   If[count == $limit, Break[]];
         |
         |   tabDelim = StringSplit[line, "\t"];
         |
         |   problemId = tabDelim[[1]];
         |   poly1 = tabDelim[[2]] // ToExpression;
         |   poly2 = tabDelim[[3]] // ToExpression;
         |   gcd = Timing[PolynomialGCD[poly1, poly2, Modulus -> modulus]];
         |
         |   timeNanos = Round[gcd[[1]] 10^9] // ToString;
         |   result = StringReplace[gcd[[2]] // InputForm // ToString, " " -> ""];
         |
         |   WriteString[out, problemId <> "\t" <> timeNanos <> "\t" <> result <> "\n"];
         |   count = count + 1;
         | ];
         | Close[in];
         | Close[out];
         | Quit[];
      """.stripMargin

    println(s"Running $name process...")
    Files.write(Paths.get(mmaTmp), util.Arrays.asList(code.split("\n"): _*))
    import scala.sys.process._
    val start = System.nanoTime()
    s"$executable -script $mmaTmp" !
    val totalTime = System.nanoTime() - start

    // read results
    println(s"Reading $name results...")
    val result = SolveResult(importGcdResults(conf, inFile, mmaOut), totalTime.nanoseconds)

    // remove tmp files
    if (tmpFileManager.deleteOnExit) {
      Files.delete(Paths.get(mmaOut))
      Files.delete(Paths.get(mmaTmp))
    }

    result
  }

  private def solveFactorization(conf: PolynomialFactorizationConfiguration, inFile: String, limit: Int): SolveResult = {
    val mmaOut = createTempFile("mmaFactor.out").getAbsolutePath
    val mmaTmp = createTempFile("mmaTmp.ws").getAbsolutePath
    val code =
      s"""
         | in = OpenRead["$inFile"];
         | out = OpenWrite["$mmaOut"];
         | count = 0;
         | While[True,
         |   line = ReadLine[in];
         |   If[line == EndOfFile, Break[]];
         |   If[StringStartsQ[line, "#"], Continue[]];
         |   If[count == $limit, Break[]];
         |
         |   tabDelim = StringSplit[line, "\t"];
         |
         |   problemId = tabDelim[[1]];
         |   poly  = tabDelim[[3]] // ToExpression;
         |   factors = Timing[FactorList[poly]];
         |
         |   timeNanos = Round[factors[[1]] 10^9] // ToString;
         |   result = #[[1]] & /@ factors[[2]];
         |   result = StringReplace[result // InputForm // ToString, {" " -> "", "{" -> "", "}" -> "", "," -> "\t"}];
         |
         |   WriteString[out, problemId <> "\t" <> timeNanos <> "\t" <> result <> "\n"];
         |   count = count + 1;
         | ];
         | Close[in];
         | Close[out];
         | Quit[];
      """.stripMargin

    println(s"Running $name process...")
    Files.write(Paths.get(mmaTmp), util.Arrays.asList(code.split("\n"): _*))
    import scala.sys.process._
    val start = System.nanoTime()
    s"$executable -script $mmaTmp" !
    val totalTime = System.nanoTime() - start

    // read results
    println(s"Reading $name results...")
    val result = SolveResult(importFactorizationResults(conf, inFile, mmaOut), totalTime.nanoseconds)

    // remove tmp files
    if (tmpFileManager.deleteOnExit) {
      Files.delete(Paths.get(mmaOut))
      Files.delete(Paths.get(mmaTmp))
    }

    result
  }
}


object MathematicaSolver {

  /** Command line options for Mathematica solver */
  trait Cli extends SolverCli {
    this: ScallopConfBase =>

    val withMma = toggleSoft("Mathematica", "Wolfram Mathematica (http://www.wolfram.com/mathematica/)")

    val mmaExec = optExec("Mathematica", "wolframscript")

    val mmaLimit = optLimit("Mathematica")

    def mkMathematicaSolver()(implicit tempFileManager: TempFileManager): Option[Solver] =
      if (withMma())
        Some(MathematicaSolver(mmaExec()).withLimit(mmaLimit.toOption))
      else
        None
  }
}