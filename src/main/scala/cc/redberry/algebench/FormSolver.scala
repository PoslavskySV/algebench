package cc.redberry.algebench

import java.io.PrintWriter
import java.nio.file.{Files, Paths}
import java.util.concurrent.TimeUnit

import cc.redberry.algebench.Problems._
import cc.redberry.algebench.Solvers._
import cc.redberry.algebench.Util._
import org.rogach.scallop.ScallopConfBase

import scala.concurrent.duration._
import scala.io.Source

/**
  * FORM solver
  */
case class FormSolver(executable: String = "form")
                     (implicit tmpFileManager: TempFileManager)
  extends Solver with StandardGcdSolver {
  override val name: String = "FORM"

  /** Applicable only for characteristic zero problems */
  override def isApplicable(problem: ProblemConfiguration): Boolean = problem match {
    case p: PolynomialGCDConfiguration => p.characteristic.isZero
    case p: PolynomialFactorizationConfiguration => p.characteristic.isZero
    case _ => false
  }

  override def innerSolve(problem: ProblemData, limit: Int): SolveResult = {
    problem match {
      case ProblemData(conf: PolynomialGCDConfiguration, file) => solveGCD(conf, file, limit)
      case _ => ???
    }
  }

  private def solveGCD(conf: PolynomialGCDConfiguration, inFile: String, limit: Int): SolveResult = {
    val variables = conf.variables.mkString(",")

    // prepare input FORM file
    val formIn = tmpFileManager.createTempFile("formIn", ".frm").getAbsolutePath
    val formOut = tmpFileManager.createTempFile("formOut").getAbsolutePath
    val formTmp1 = tmpFileManager.createTempFile("formTmp").getAbsolutePath
    val formTmp2 = tmpFileManager.createTempFile("formTmp").getAbsolutePath

    val formWriter = new PrintWriter(Files.newBufferedWriter(Paths.get(formIn)))
    try {
      formWriter.println("#-")
      formWriter.println("Off statistics;")
      if (!conf.characteristic.isZero)
        formWriter.println(s"modulus ${conf.characteristic};")

      formWriter.println(s"Symbols $variables;")
      for (line <- Source.fromFile(inFile).getLines.filter(!_.startsWith("#")).filter(!_.isEmpty).take(limit)) {
        val tabDelim = line.split("\t")
        val problemId = tabDelim(0)
        val poly1 = tabDelim(1)
        val poly2 = tabDelim(2)


        formWriter.write("#$poly1 = ")
        formWriter.write(poly1)
        formWriter.write(";\n")

        formWriter.write("#$poly2 = ")
        formWriter.write(poly2)
        formWriter.write(";\n")

        val code =
          s"""
             |
             | #$$tstart = `timer_';
             | #$$gcd = gcd_($$poly1, $$poly2);
             | #$$tend = `timer_';
             |
             | #$$dt = ($$tend - $$tstart);
             | #write <$formTmp1> "%s\\t%$$\\t%$$" , $problemId, $$dt, $$gcd
             | #system tr -d " \\n\\r" < $formTmp1 > $formTmp2
             | #system tr -d "\\\\" < $formTmp2 >> $formOut
             | #system echo "" >> $formOut
             | #remove <$formTmp1>
             |
        """.stripMargin

        formWriter.println(code)
      }

      formWriter.println(".end")
    } finally {
      formWriter.close()
    }

    val start = System.nanoTime()
    runProcess(s"$executable -q $formIn")
    val totalTime = System.nanoTime() - start

    // read results
    println(s"Reading $name results...")
    val result = SolveResult(importGcdResults(conf, inFile, formOut, timeUnit = TimeUnit.MILLISECONDS), totalTime.nanoseconds)

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


object FormSolver {

  /** Command line options for FORM solver */
  trait Cli extends SolverCli {
    this: ScallopConfBase =>

    val withForm = toggleSoft("FORM", "FORM (https://www.nikhef.nl/~form/)")

    val formExec = optExec("FORM", "form")

    val formLimit = optLimit("FORM")

    def mkFORMSolver()(implicit tempFileManager: TempFileManager): Option[Solver] =
      if (withForm())
        Some(FormSolver(formExec()).withLimit(formLimit.toOption))
      else
        None
  }
}