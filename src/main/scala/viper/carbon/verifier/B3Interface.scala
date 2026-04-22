// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2021 ETH Zurich.

package viper.carbon.verifier

import viper.carbon.boogie.{Assert, Program}
import viper.silver.reporter.BackendSubProcessStages._
import viper.silver.reporter.{BackendSubProcessReport, Reporter}
import viper.silver.verifier.errors.Internal
import viper.silver.verifier.reasons.InternalReason
import viper.silver.verifier._

import java.io._
import scala.jdk.CollectionConverters._

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

  def b3defaultOptions = Seq("--stdin") // [B3: will need to replace with Seq.empty[String] to use ASTs directly]

  /** The (resolved) path where B3 is supposed to be located. */
  def verifierPath: String

  /** The (resolved) path where Z3 is supposed to be located. */
  def z3Path: String

  private var _b3Process: Option[Process] = None
  private var _b3ProcessPid: Option[Long] = None

  //[B3 todo: check in what form this is still true:]
  // Z3 processes cannot be collected this way, perhaps because they are not created using Java 9 API (but via Boogie).
  // Hence, for now we have to trust Boogie to manage its own sub-processes.
  // private var _z3ProcessStream: Option[LazyList[ProcessHandle]] = None

  var b3errormap: Map[Int, VerificationError] = Map()
  var b3models : collection.mutable.ListBuffer[String] = new collection.mutable.ListBuffer[String]
  /**
   * This will setup and run B3 on the given program using the specified options
   * 
   * 
   * 
   */
  def invokeB3(program: Program, options: Seq[String], timeout: Option[Int]): (String,VerificationResult) = {
    // find all errors and assign everyone a unique id
    b3errormap = Map()
    program.visit {
      case a@Assert(exp, error) =>
        b3errormap += (a.id -> error)
    }

    println(b3defaultOptions ++ options)

    // invoke B3
    val optOutput = run(program.toString, b3defaultOptions ++ options, timeout)
    // print(optOutput)
    optOutput match {
      case None =>
        // Timeout
        (null, Failure(Seq(TimeoutOccurred(timeout.get, "second(s)"))))
      case Some(output) =>
        // parse the output
        print(output) // [B3 base: for now we just print B3's response as-is]
        ("unknown", Success)

        // [B3 base: Better error parsing should be implemented here later (extension goal). See BoogieInterface.scala]
    }
  }

  // [B3 base: currently we just print B3's response 1-by-1. Better error parsing is an extension goal.]
  // (parser def would come here, see BoogieInterface.scala)


  /**
    * Invoke B3.
    * Returns None if there was a timeout, otherwise the B3 output.
    */
  private def run(input: String, options: Seq[String], timeout: Option[Int]) = {
    reporter report BackendSubProcessReport("carbon", verifierPath, BeforeInputSent, _b3ProcessPid)

    val cmd: Seq[String] = Seq("java", "b3", "verify") ++ options
    val pb: ProcessBuilder = new ProcessBuilder(cmd.asJava)
    val env = pb.environment()
    env.put("CLASSPATH", verifierPath)
    val proc: Process = pb.start()
    _b3Process = Some(proc)
    _b3ProcessPid = Some(proc.pid)

    //proverShutDownHook approach taken from Silicon's codebase
    val proverShutdownHook = new Thread {
      override def run(): Unit = {
        destroyProcessAndItsChildren(proc, verifierPath)
      }
    }
    Runtime.getRuntime.addShutdownHook(proverShutdownHook)

    // _z3ProcessStream = Some(proc.descendants().toScala(LazyList))
    reporter report BackendSubProcessReport("carbon", verifierPath, AfterInputSent, _b3ProcessPid)

    val errorConsumer =
      new InputStreamConsumer(proc.getErrorStream, () => reporter report BackendSubProcessReport("carbon", verifierPath, OnError, _b3ProcessPid))
    val errorStreamThread = new Thread(errorConsumer)
    val inputConsumer =
      new InputStreamConsumer(proc.getInputStream, () => reporter report BackendSubProcessReport("carbon", verifierPath, OnOutput, _b3ProcessPid))
    val inputStreamThread = new Thread(inputConsumer)

    errorStreamThread.start()
    inputStreamThread.start()

    // Send the program to B3
    proc.getOutputStream.write(input.getBytes);
    proc.getOutputStream.close()

    var b3Timeout = false

    try {
      timeout match {
        case Some(t) if t > 0 =>
          b3Timeout = !proc.waitFor(t, java.util.concurrent.TimeUnit.SECONDS)
        case _ =>
          proc.waitFor()
      }
    } finally {
      destroyProcessAndItsChildren(proc, verifierPath)
    }

    // Deregister the shutdown hook, otherwise the prover process that has been stopped cannot be garbage collected.
    // Explanation: https://blog.creekorful.org/2020/03/classloader-and-memory-leaks/
    // Bug report: https://github.com/viperproject/silicon/issues/579
    Runtime.getRuntime.removeShutdownHook(proverShutdownHook)

    errorStreamThread.join()
    inputStreamThread.join()

    try {
      val errorOutput = errorConsumer.result.get
      val normalOutput = inputConsumer.result.get
      reporter report BackendSubProcessReport("carbon", verifierPath, OnExit, _b3ProcessPid)

      if (b3Timeout)
        None
      else
        Some(errorOutput + normalOutput)
    } catch {
      case _: NoSuchElementException => sys.error("Could not retrieve output from B3")
    }
  }

  private def destroyProcessAndItsChildren(proc: Process, processPath: String) : Unit = {
    if(proc.isAlive) {
      reporter report BackendSubProcessReport("carbon", processPath, BeforeTermination, _b3ProcessPid)
      proc.children().forEach(_.destroy() : Unit)
      proc.destroy()
      reporter report BackendSubProcessReport("carbon", processPath, AfterTermination, _b3ProcessPid)
    }
  }

  def stopB3(): Unit = {
    _b3Process match {
      case Some(proc) =>
        destroyProcessAndItsChildren(proc, verifierPath)
      case None =>
    }
  }
}
