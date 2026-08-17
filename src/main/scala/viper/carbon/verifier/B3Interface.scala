// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2021 ETH Zurich.

package viper.carbon.verifier

import viper.carbon.b3.B3Nodes.{Assert, Check, Program}
import viper.carbon.b3.B3Adapter.{runB3, printRawAst}
import viper.carbon.b3.B3Naming
import viper.carbon.b3.B3Development
import viper.silver.reporter.BackendSubProcessStages._
import viper.silver.reporter.Reporter // import viper.silver.reporter.{BackendSubProcessReport, Reporter}
import viper.silver.verifier.errors.Internal
import viper.silver.verifier.reasons.InternalReason
import viper.silver.verifier._

import java.io._

class B3Dependency(_location: String) extends Dependency {
  def name = "B3"
  def location = _location
  var version = "" // filled-in when B3 is invoked
}

class B3InputStreamConsumer(val is: InputStream, actionBeforeConsumption: () => Unit) extends Runnable {
  var result : Option[String] = None

  private def convertStreamToString(is: InputStream) = {
    val s = new java.util.Scanner(is).useDelimiter("\\A")
    if (s.hasNext) s.next() else ""
  }

  def run(): Unit = {
    actionBeforeConsumption()
    result = Some(convertStreamToString(is))
    is.close()
  }
}

case class B3FailureContextImpl(counterExample: Option[Counterexample]) extends FailureContext

/**
  * Defines a clean interface to invoke B3 and get a list of errors back.
  */

trait B3Interface {

  def reporter: Reporter

  /** The (resolved) path where B3 is supposed to be located. */
  def verifierPath: String

  /** The (resolved) path where Z3 is supposed to be located. */
  def z3Path: String

  /** The level of development information to be shown. */
  def devLvl: Int

  /** Whether or not the B3 code should be printed. */
  def printOut: Boolean


  var errormap: Map[Int, VerificationError] = Map()
  var models : collection.mutable.ListBuffer[String] = new collection.mutable.ListBuffer[String]
  /**
   * This will setup and run B3 on the given program using the specified options. 
   * Timeout currently not suppported.
   * 
   * @param program The Program (B3 AST (RawAst)) we want to verify
   * @param options Sequence containing B3 flags. These MUST be valid B3 flags in "--flagName" format.
   * @param timeout Currently does nothing. (B3 ADVANCED)
   * @return Currently always ("?", Success), because we dont do error parsing yet
   */
  def invokeB3(program: Program, options: Seq[String], timeout: Option[Int], printToFile: String = null): (String,VerificationResult) = {   
    // find all errors and assign everyone a unique id
    errormap = Map()
    program.visit {
      case a@Assert(_, error, _) =>
        errormap += (a.id -> error)
      case a@Check(_, error) =>
        // B3 ADVANCED: Maybe need to go over the "VerificationError"s (Check != Assert => Check might need its own VerificationError type)
        errormap += (a.id -> error)
    }

    val rawB3Ast = program.b3fy

    // invoke B3 and capture any output in outStream (-> output)
    val outStream = new ByteArrayOutputStream()
    val newOut = new PrintStream(outStream)
    val oldOut = System.out
    println("=============== NOW RUNNING B3 VERIFIER ===============")
    try {
      System.setOut(newOut)
      runB3(rawB3Ast, options) // B3 ADVANCED: timeout mechanism
      newOut.flush()
    } finally {
      System.setOut(oldOut)
    }
    val output = outStream

    // Possibly print RawAst to output (if '--printOut' was used)
    if (printOut) {
      // Show Field Mappings (when using shortened form)
      println("//Field Mappings:")
      B3Naming.printTypVarMapping
      println()
      printRawAst(rawB3Ast)
    }
    
    // Possibly save RawAst-printOut to a file (if one is specified using '--print FILEPATH')
    if(printToFile != null) {
      val printOutStream = new ByteArrayOutputStream()
      val newPrintOut = new PrintStream(printOutStream)
      val oldPrintOut = System.out
      try {
        System.setOut(newPrintOut)
        printRawAst(rawB3Ast)
        newPrintOut.flush()
      } finally {
        System.setOut(oldPrintOut)
      }
      // write Boogie program to the specified file
      val f = new File(printToFile)
      val stream = new BufferedOutputStream(new FileOutputStream(f))
      stream.write(B3Naming.returnTypVarMapping.getBytes ++ printOutStream.toByteArray())
      stream.close()
    }

    // Print B3's output
    if (devLvl >= 1) {
      // If --print is used then this info will be helpful! //B3 ADVANCED: dont print this if names are not shortened with numbers
      if (options.contains("--print")) B3Naming.printTypVarMapping
      println("//*************************")
      print(output)
      println("//*************************")
    }

    // parse B3 output (B3 ADVANCED: improve this)
    val parsedOutputResult = parse(output.toString()) match {
      case (version, Nil) =>
        (version, Success)
      case (version, errorIds) => {
        val errors = (0 until errorIds.length).map(i => {
          val id = errorIds(i)
          val error = errormap.get(id).get
          if (models.nonEmpty) {
            error.failureContexts = Seq(FailureContextImpl(Some(SimpleCounterexample(Model(models(i))))))
          }
          error
        })
        (version, Failure(errors))
      }
    }

    // (printing some additional infos for development)
    if (devLvl >= 1) {
      B3Development.printALL()
    }
    
    // cannot get b3 version. Since we currently don't parse/handle errors we always return Success
    parsedOutputResult
  }

  /**
    * Parse the output of Boogie. Returns a pair of the detected version number and a sequence of error identifiers.
    */
  private def parse(output: String): (String, Seq[Int]) = {
    // val LogoPattern = "Boogie program verifier version ([0-9.]+),.*".r
    // val SummaryPattern = "Boogie program verifier finished with ([0-9]+) verified, ([0-9]+) error.*".r
    val ErrorPattern = ".+ \\[([0-9]+)\\]: .+".r
    val CurrentProcedurePattern = "Verifying (.+) ...".r
    val AlternativePattern = "  choose alternative ([0-9]+)".r
    val errors = collection.mutable.ListBuffer[Int]()
    var otherErrId = 0
    var version_found: String = "?" // B3 ADVANCED: check if we can get B3's version somehow
    var procName: String = null

    val unexpected : (String => Unit) = (msg:String) => {
      otherErrId -= 1
      errors += otherErrId
      val internalError = Internal(InternalReason(DummyNode, msg))
      errormap += (otherErrId -> internalError)
    }

    val precollection = collection.mutable.ListBuffer[String]()

    var ignoreNonVerification = true
    for (l <- output.linesIterator) {
      // B3 ADVANCED: add cases for errors in B3 (e.g. if some type/function/... is not declared)
      l match {
        case CurrentProcedurePattern(n) =>
          ignoreNonVerification = false
          procName = n
        case s if ignoreNonVerification => //ignore everything before verification begins (e.g. printout of program)
          precollection += s
        case ErrorPattern(id) =>
          errors += id.toInt
        case AlternativePattern(_) =>
          null // B3 ADVANCED: show the different "paths" of the traces that lead to verification errors
        case "" => // ignore empty lines
        case _ =>
          unexpected(s"Found an unparsable output from B3: $l")
      }
    }
    if (ignoreNonVerification) unexpected(s"No method (/procedure) verified; either no method (/procedure) exists, or other problem such as parsing error. This was the only output: $precollection")
    (version_found,errors.toSeq)
  }
}
