// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2021 ETH Zurich.

package viper.carbon.verifier

import viper.carbon.boogie.Program // import viper.carbon.boogie.{Assert, Program}
import viper.carbon.b3.B3Adapter.runB3
import viper.carbon.b3.BoogieToB3Transformer
// import viper.silver.reporter.BackendSubProcessStages._
import viper.silver.reporter.Reporter // import viper.silver.reporter.{BackendSubProcessReport, Reporter}
// import viper.silver.verifier.errors.Internal
// import viper.silver.verifier.reasons.InternalReason                //TODO/LATER? reimplement the error reporting system in the same way as done for Boogie
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

  def b3defaultOptions = Seq("--print") //<-for now, to definitely have an output; later replace with: Seq.empty[String]  // There are no default options needed for B3

  /** The (resolved) path where B3 is supposed to be located. */
  def verifierPath: String

  /** The (resolved) path where Z3 is supposed to be located. */
  def z3Path: String


  /**
   * This will setup and run B3 on the given program using the specified options. 
   * Includes transformation step to (raw) B3 AST. Timeout currently not working.
   * 
   * @param program The Program (Boogie AST) we want to verify
   * @param options Sequence containing B3 flags. These MUST be valid B3 flags in "--flagName" format.
   * @param timeout Currently does noting.
   * @return Currently always ("?", Success), because we dont do error parsing yet
   */
  def invokeB3(program: Program, options: Seq[String], timeout: Option[Int], verifier: viper.carbon.verifier.Verifier): (String,VerificationResult) = {
    val B3Transformer = new BoogieToB3Transformer(verifier)
    // translate Boogie AST to a B3 raw AST
    val rawB3prog = B3Transformer.transformProgram(program)
    // print(program.toString)

    // invoke B3 and capture any output in outStream (-> output)
    val outStream = new ByteArrayOutputStream()
    val newOut = new PrintStream(outStream)
    val oldOut = System.out
    try {
      System.setOut(newOut)
      runB3(rawB3prog, b3defaultOptions ++ options) // [B3 todo?: currently no timeout mechanism]
      newOut.flush()
    } finally {
      System.setOut(oldOut)
    }
    val output = outStream

    // Output B3 output
    println("*************************")
    print(output) // [B3 base: an extension goal would be to implement error parsing here, see BoogieInterface.scala -> parse]
    println("*************************")

    B3Transformer.printInfo()
    
    // cannot get b3 version. Since we currently don't parse/handle errors we always return Success
    ("?", Success)
  }
}
