package cc.redberry.algebench

import java.io.PrintWriter
import java.nio.file.{Files, Paths}
import java.util

import cc.redberry.algebench.Problems.{GroebnerBasisConfiguration, PolynomialFactorizationConfiguration, PolynomialGCDConfiguration, ProblemConfiguration, ProblemData}
import cc.redberry.algebench.Solvers._
import cc.redberry.algebench.Util.{TempFileManager, createTempFile}
import org.rogach.scallop.ScallopConfBase

import scala.concurrent.duration._
import scala.io.Source

/**
  * Wolfram Mathematica solver
  */
final case class MathematicaSolver(executable: String = "wolframscript")
                                  (implicit tmpFileManager: TempFileManager)
  extends Solver
    with StandardGcdSolver
    with StandardFactorizationSolver
    with StandardGBSolver {
  override val name: String = "Mathematica"

  override def isApplicable(problem: ProblemConfiguration): Boolean = problem match {
    case _: PolynomialGCDConfiguration => true
    case _: GroebnerBasisConfiguration => true
    case p: PolynomialFactorizationConfiguration => p.characteristic.isZero
    case _ => false
  }

  override def innerSolve(problem: ProblemData, limit: Int): SolveResult = {
    problem match {
      case ProblemData(conf: PolynomialGCDConfiguration, file) => solveGCD(conf, file, limit)
      case ProblemData(conf: PolynomialFactorizationConfiguration, file) => solveFactorization(conf, file, limit)
      case ProblemData(conf: GroebnerBasisConfiguration, file) => solveGB(file, limit)
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

    Files.write(Paths.get(mmaTmp), util.Arrays.asList(code.split("\n"): _*))
    val start = System.nanoTime()
    runProcess(s"$executable -script $mmaTmp")
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

    Files.write(Paths.get(mmaTmp), util.Arrays.asList(code.split("\n"): _*))
    val start = System.nanoTime()
    runProcess(s"$executable -script $mmaTmp")
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

  private def solveGB(inFile: String, limit: Int): SolveResult = {
    val mmaOut = createTempFile("mmaGB.out").getAbsolutePath
    val mmaTmp = createTempFile("mmaTmp.ws").getAbsolutePath

    val writer = new PrintWriter(Files.newBufferedWriter(Paths.get(mmaTmp)))
    try {
      writer.println(s"""out = OpenWrite["$mmaOut"];""")
      for (line <- Source.fromFile(inFile).getLines.filterNot(_.startsWith("#")).filter(_.nonEmpty).take(limit)) {
        val tabDelim = line.split("\t")
        val problemId = tabDelim(0)
        val problemName = tabDelim(1)
        val gbData = GroebnerBasisData(problemName)
        val characteristic = tabDelim(2)
        val order = tabDelim(3).toLowerCase match {
          case "lex" => "Lexicographic"
          case "grevlex" => "DegreeReverseLexicographic"
          case "glex" => "DegreeLexicographic"
          case x@_ => throw new RuntimeException(s"Mathematica doesn't support this order: $x")
        }

        writer.println(
          s"""
             | gb = Timing[
             |        GroebnerBasis[
             |          {${gbData.basis.map(p => gbData.ring.show(p)).mkString(",")}},
             |          {${gbData.ring.variables.mkString(",")}},
             |          MonomialOrder -> $order,
             |          Modulus -> $characteristic
             |        ]
             |      ];
             |
             |  timeNanos = Round[gb[[1]] 10^9] // ToString;
             |  WriteString[out, "$problemId" <> "\t" <> "$problemName" <> "\t" <> timeNanos <> "\n"];
             |
        """.stripMargin)
      }

      writer.write(
        """
          | Close[out];
          | Quit[];
          |""".stripMargin)
    } finally {
      writer.close()
    }

    val start = System.nanoTime()
    runProcess(s"$executable -script $mmaTmp")
    val totalTime = System.nanoTime() - start

    // read results
    println(s"Reading $name results...")
    val result = SolveResult(importGBResults(mmaOut), totalTime.nanoseconds)

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