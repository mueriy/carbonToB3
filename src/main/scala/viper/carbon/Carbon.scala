// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2021 ETH Zurich.

package viper.carbon

import ch.qos.logback.classic.Logger
import viper.silver.frontend.{MinimalViperFrontendAPI, SilFrontend, SilFrontendConfig, ViperFrontendAPI}
import viper.silver.logger.ViperStdOutLogger
import viper.silver.reporter.{Reporter, StdIOReporter}
import viper.silver.verifier.{Verifier => SilVerifier}
import viper.silver.utility.{FileProgramSubmitter}

/**
 * The main object for Carbon containing the execution start-point.
 */
object Carbon extends CarbonFrontend(StdIOReporter("carbon_reporter"), ViperStdOutLogger("Carbon", "INFO").get) {
  def main(args: Array[String]): Unit = {
    val submitter = new FileProgramSubmitter(this)
    submitter.setArgs(args)

    execute(args)
    specifyAppExitCode()

    submitter.submit()
    sys.exit(appExitCode)
  }
}

class CarbonFrontend(override val reporter: Reporter,
                     override val logger: Logger) extends SilFrontend {

  private var carbonInstance: CarbonVerifier = _

  override def backendTypeFormat: Option[String] = Some("Boogie")

  def createVerifier(fullCmd: String) = {
    carbonInstance = CarbonVerifier(reporter, Seq("Arguments: " -> fullCmd))

    carbonInstance
  }

  def configureVerifier(args: Seq[String]) = {
  	carbonInstance.parseCommandLine(args)

    carbonInstance.config
  }

  override def init(verifier: SilVerifier): Unit = {
    verifier match {
      case carbon: CarbonVerifier =>
        carbonInstance = carbon
      case _ =>
        sys.error( "Expected verifier to be an instance of CarbonVerifier but got an instance " +
                  s"of ${verifier.getClass}")
    }

    super.init(verifier)

    _config = carbonInstance.config
  }
}

/**
  * Carbon "frontend" for use by actual Viper frontends.
  * Performs consistency check and verification.
  * See [[viper.silver.frontend.ViperFrontendAPI]] for usage information.
  */
class CarbonFrontendAPI(override val reporter: Reporter)
  extends CarbonFrontend(reporter, ViperStdOutLogger("CarbonFrontend", "INFO").get) with ViperFrontendAPI

/**
  * Carbon "frontend" for use by actual Viper frontends.
  * Performs only verification (no consistency check).
  * See [[viper.silver.frontend.ViperFrontendAPI]] for usage information.
  */
class MinimalCarbonFrontendAPI(override val reporter: Reporter)
  extends CarbonFrontend(reporter, ViperStdOutLogger("CarbonFrontend", "INFO").get) with MinimalViperFrontendAPI

class CarbonConfig(args: Seq[String]) extends SilFrontendConfig(args, "Carbon") {
  // val boogieProverLog = opt[String]("proverLog",
  //   descr = "Prover log file written by Boogie (default: none)",
  //   default = None,
  //   noshort = true
  // )

  // B3 does not support custom locations, so z3 must be on PATH. Setting this does nothing.
  // B3 ADVANCED: Remove this, or implement that the given path is temporarily added to PATH.
  val Z3executable = opt[String]("z3Exe",
    descr = ("CURRENTLY NOT SUPPORTED!"),
    // descr = "Manually-specified full path to Z3.exe executable (default: ${Z3_EXE}). DO NOT USE!",
    default = None,
    noshort = true
  )

  val disableAllocEncoding = opt[Boolean]("disableAllocEncoding",
    descr = "Disable Allocation-related assumptions (default: enabled)",
    default = None,
    noshort = true
  )

  // B3 ADVANCED: change description again after supporting it.
  val timeout = opt[Int]("timeout",
    descr = ("CURRENTLY NOT SUPPORTED!"),
    // descr = ("Time out after approx. n seconds. The timeout is for the whole verification in Boogie, "
    //        + "not per method or proof obligation (default: 0, i.e. no timeout)."), 
    default = None,
    noshort = true
  )

  val b3Opt = opt[String]("b3Opt",
  descr = ("Option(s) to pass-through as options to B3. Currently all B3 flags except --stdin are supported. "
        + "Must be provided as space-separated string of B3 flags (e.g. \\\"--z3 --rprint\\\") (default: none)"),
  default = None,
  noshort = true
  )

  val b3Executable = opt[String]("b3Jar",
    descr = "Manually-specified full path to B3.jar (default: ${B3_JAR})",
    default = None,
    noshort = true
  )

  val checkNumbers = opt[List[Int]]("check",
    descr = "Given comma-separated list of integers (e.g. '1, 5, 6'), all Assert-Stmts matching that number will be transformed into a Check-Stmt.",
    default = None,
    noshort = true
  )

  val developerLevel = opt[Int]("dev",
    descr = "The amount of development information shown. (0 = none, 1 = some (allows running B3 on the printed code after removing all '['s and ']'s), 2 = reserve, 3 = all)",
    default = Some(0),
    noshort = true
  )

  val printOut = opt[Boolean]("printOut",
    descr = "If used, the B3 program is printed to stdout.",
    default = Some(false),
    noshort = true
  )

  val b3Out = opt[String]("print",
    descr = "Write the B3 program printout to the provided filename (default: none)",
    default = None,
    noshort = true
  )

  // B3 ADVANCED: clean up these config option & add other options 
  // (e.g. one option could set whether names should be shortened to a number or not ("HeapType%%NormalField%Ref" vs "HeapType%%0" and "functionName%FSomeType%Int%Int" vs "functionName%F0"))


  verify()
}
