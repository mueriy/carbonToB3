// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2021 ETH Zurich.

package viper.carbon

import boogie.BoogieModelTransformer
import b3.Namespace
import modules.impls._
import viper.silver.ast.{MagicWand, Program, Quasihavoc, Quasihavocall}
import viper.silver.utility.Paths
import viper.silver.verifier._
import verifier.{BoogieDependency, BoogieInterface, B3Dependency, B3Interface, Verifier}

import java.io.{BufferedOutputStream, File, FileOutputStream, IOException}
import viper.silver.frontend.{MissingDependencyException, NativeModel, VariablesModel}
import viper.silver.reporter.Reporter


/**
 * The main class to perform verification of Viper programs.  Deals with command-line arguments, configuration
 * of modules and choosing which module implementations to use.
 *
 * Debug information can either be set using the constructor argument or the setter.
 */
case class CarbonVerifier(override val reporter: Reporter,
                          private var _debugInfo: Seq[(String, Any)] = Nil) extends Verifier with viper.silver.verifier.Verifier with B3Interface {

  var env = null

  private var _config: CarbonConfig = _
  def config = _config


  def start(): Unit = {}
  def stop(): Unit = {
    if (allModules != null) {
      allModules foreach (m => {
        m.stop()
      })
    }
    // stopBoogie()
  }

  private var namespaceId = 0
  override def freshNamespace(name: String): Namespace = {
    namespaceId += 1
    Namespace(name, namespaceId)
  }

  val stmtModule = new DefaultStmtModule(this)
  val expModule = new DefaultExpModule(this)
  val typeModule = new DefaultTypeModule(this)
  val exhaleModule = new DefaultExhaleModule(this)
  val inhaleModule = new DefaultInhaleModule(this)
  val heapModule = new DefaultHeapModule(this)
  val funcPredModule = new DefaultFuncPredModule(this)
  val permModule = new QuantifiedPermModule(this)
  val mainModule = new DefaultMainModule(this)
  val stateModule = new DefaultStateModule(this)
  val domainModule = new DefaultDomainModule(this)
  val seqModule = new DefaultSeqModule(this)
  val setModule = new DefaultSetModule(this)
  val mapModule = new DefaultMapModule(this)
  val wandModule = new DefaultWandModule(this)
  val loopModule = new DefaultLoopModule(this)

  // initialize all modules
  allModules foreach (m => {
    m.start()
  })


  /** The default location for Z3 (the environment variable ${Z3_EXE}). */
  lazy val z3Default: String = new File(Paths.resolveEnvVars("${Z3_EXE}")).getAbsolutePath

  /** The default location for B3 (the environment variable ${B3_JAR}). */
  lazy val b3Default: String = new File(Paths.resolveEnvVars("${B3_JAR}")).getAbsolutePath

  /** The (resolved) path where Boogie/B3 is supposed to be located. */
  def verifierPath = if (config != null) config.b3Executable.toOption match {
      case Some(path) => {new File(path).getAbsolutePath}
      case None => b3Default
  } else b3Default

  /** The (resolved) path where Z3 is supposed to be located. */
  def z3Path = if (config != null) config.Z3executable.toOption match {
    case Some(path) => {new File(path).getAbsolutePath}
    case None => z3Default
  } else z3Default

  def assumeInjectivityOnInhale = if (config != null) config.assumeInjectivityOnInhale.toOption match {
    case Some(b) => b
    case None => false
  }
  else false

  def respectFunctionPrecPermAmounts: Boolean = if (config != null) config.respectFunctionPrePermAmounts.toOption match {
    case Some(b) => b
    case None => false
  }
  else false

  override def usePolyMapsInEncoding =
    if (config != null) {
      config.desugarPolymorphicMaps.toOption match {
        case Some(b) => !b
        case None => true
      }
    } else {
      true
    }

  def name: String = "carbon"
  def version: String = "1.0"
  def buildVersion = version
  def copyright: String = "(c) 2013 ETH Zurich"

  def getDebugInfo = _debugInfo
  def debugInfo(info: Seq[(String, Any)]): Unit = {
    _debugInfo = info
  }

  def toolDesc = s"$name $version"
  def dependencyDescs = {
    (dependencies map (dep => {
      s"${dep.name} ${dep.version}, located at ${dep.location}."
    }))
  }

  def parseCommandLine(options: Seq[String]): Unit = {
    _config = new CarbonConfig(options)
  }

  //B3 LATER: Add B3Jar as a Dependency 
  lazy val dependencies: Seq[Dependency] = {
    import scala.sys.process._
    val unknownVersion = "(?)"
    List(new BoogieDependency(verifierPath), new Dependency {
      def name = "Z3"
      def version = {
        try {
          val v = List(z3Path, "-version").lazyLines.to(List)
          if (v.size == 1 && v(0).startsWith("Z3 version ")) {
            v(0).substring("Z3 version ".size)
          } else {
            unknownVersion
          }
        }
        catch {
          case _: IOException => throw MissingDependencyException("Z3 couldn't be found.")
        }

      }
      def location = z3Path
    })
  }

  def verify(program: Program) : VerificationResult = {
    _program = program

    val unsupportedFeatures : Seq[AbstractError] =
      program.shallowCollect(
        n =>
          n match {
            case q: Quasihavocall =>
              ConsistencyError("Carbon does not support quasihavocall", q.pos)
            case q@Quasihavoc(_, MagicWand(_, _)) =>
              ConsistencyError("Carbon does not support quasihavoc of magic wands", q.pos)
          }
      )

    if(unsupportedFeatures.nonEmpty) {
      return Failure(unsupportedFeatures)
    }

    // reset all modules
    allModules map (m => m.reset())
    heapModule.enableAllocationEncoding = config == null || !config.disableAllocEncoding.isSupplied // NOTE: config == null happens on the build server / via sbt test

    var transformNames = false
    if (config == null) Seq() else config.counterexample.toOption match {
      case Some(NativeModel) =>
      case Some(VariablesModel) => transformNames = true
      case None =>
      case Some(v) => sys.error("Invalid option: " + v)
    }

    val (tProg, translatedNames) = mainModule.translate(program, reporter)
    _translated = tProg.b3fy


    val options = if (config == null) {
                    Nil
                  } else {
                    config.b3Opt.toOption match {
                      case Some(l) => l.split(" ").toSeq
                      case None => Nil
                    }
                  }

    var timeout: Option[Int] = None


    val invokeResult = invokeB3(_translated, options, timeout)

    invokeResult match {
      case (version,result) =>
        if (version!=null) { dependencies.foreach(_ match {
          case b:B3Dependency => b.version = version
          case _ => }) }

        result match {
          // [B3 base: Just dont use 'variables' counterexample mode. Later we could add a "B3ModelTransformer" here, modify BoogieModelTransformer, or not allow it at all.]
          case Failure(errors) if transformNames => {
            throw new UnsupportedOperationException("Counterexample model 'variables' is currently not supported when using B3")
            errors.foreach(e =>  BoogieModelTransformer.transformCounterexample(e, translatedNames))
          }
          case _ => result
        }
        result
    }
  }



  private var _translated: RawAst.Program = null
  def translated = _translated

  private var _program: Program = null
  def program = _program
  def program_=(p : Program): Unit = {
    _program = p
  }

  def replaceProgram(prog : Program) = {this.program = prog}
}
