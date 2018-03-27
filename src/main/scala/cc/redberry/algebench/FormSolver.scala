package cc.redberry.algebench

import java.io.{OutputStream, PrintWriter}
import java.nio.file.{Files, Paths}
import java.util.concurrent.TimeUnit

import cc.redberry.algebench.Problems._
import cc.redberry.algebench.Solvers._
import cc.redberry.algebench.Util._

import scala.concurrent.duration._
import scala.io.Source

/**
  *
  */
case class FormSolver(executable: String = "form")
                     (implicit tmpFileManager: TempFileManager)
  extends Solver with StandardGcdSolver {
  override val name: String = "FORM"

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

    val variables = conf.variables.mkString(",")

    // prepare input FORM file
    val formIn = tmpFileManager.createTempFile("formIn", ".frm").getAbsolutePath
    val formOut = tmpFileManager.createTempFile("formOut").getAbsolutePath
    val formTmp1 = tmpFileManager.createTempFile("formTmp").getAbsolutePath
    val formTmp2 = tmpFileManager.createTempFile("formTmp").getAbsolutePath

    val formWriter = new PrintWriter(Files.newBufferedWriter(Paths.get(formIn)))
    try {
      formWriter.println("Off statistics;")
      if (!conf.characteristic.isZero)
        formWriter.println(s"modulus ${conf.characteristic};")

      formWriter.println(s"Symbols $variables;")
      for (line <- Source.fromFile(inFile).getLines.filter(!_.startsWith("#")).filter(!_.isEmpty())) {
        val tabDelim = line.split("\t")
        val problemId = tabDelim(0)
        val poly1 = tabDelim(1)
        val poly2 = tabDelim(2)

        formWriter.println(s"Local a$problemId = $poly1;")
        formWriter.println(s"Local b$problemId = $poly2;")
        formWriter.println(".sort")

        formWriter.println()

        formWriter.println("#$tstart%s = `timer_';".format(problemId))
        formWriter.println(s"Local gcd$problemId = gcd_(a$problemId, b$problemId);")
        formWriter.println(".sort")
        formWriter.println("#$telapsed%s = (`timer_' - `$tstart%s');".format(problemId, problemId))

        formWriter.println()

        formWriter.println(".sort")
        formWriter.println("Format nospaces;")
        formWriter.println(s"#write <$formTmp1> " + "\"%s\\t%$\\t%E\" ," + s"$problemId," + "$telapsed%s".format(problemId) + s",gcd$problemId")
        formWriter.println("#system tr -d \" \\n\\r\" " + s"< $formTmp1 > $formTmp2")
        formWriter.println("#system tr -d \"\\\\\" " + s"< $formTmp2 >> $formOut")
        formWriter.println("#system echo \"\" >> " + formOut)
        formWriter.println(s"#remove <$formTmp1>")
        formWriter.println(".sort")

        formWriter.println()
      }

      formWriter.println(".end")
    } finally {
      formWriter.close()
    }

    val dummy = new OutputStream {
      override def write(b: Int): Unit = {}
    }

    import sys.process._
    val start = System.nanoTime()

    // overcome huge output of FORM which can't be switched off
    (s"$executable -q $formIn" #> dummy) !
    val totalTime = System.nanoTime() - start

    // read results
    val result = SolveResult(readResultsForGCD(conf, inFile, formOut, timeUnit = TimeUnit.MILLISECONDS), totalTime.nanoseconds)

    // remove tmp files
    if (tmpFileManager.deleteOnExit) {
      Files.deleteIfExists(Paths.get(formIn))
      Files.deleteIfExists(Paths.get(formOut))
      Files.deleteIfExists(Paths.get(formTmp1))
      Files.deleteIfExists(Paths.get(formTmp2))
    }

    result
  }
}
