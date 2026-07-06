// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2021 ETH Zurich.

package viper.carbon.modules

import viper.silver.{ast => sil}
import viper.carbon.b3.B3Nodes._
import viper.carbon.b3.Namespace
import viper.carbon.verifier.Environment
import viper.silver.reporter.Reporter

/**
 * A module for translating Viper programs by invoking the right modules and
 * gathering all the preambles, etc.
 */
trait MainModule extends Module {
  /**
   * Translate a Viper program into a B3 program.
   * Returns a B3 program along with a map that maps Viper names to their respective B3 names,
   * i.e. Viper member name -> (Viper variable name -> B3 variable name)
   */
  def translate(p: sil.Program, reporter: Reporter): (Program, Map[String, Map[String, String]])

  // B3 TODO: Check if these options make sense. Maybe we need P/F-Parameter creation AND var-USAGE creation.
  /** 
   * Translate a local variable along with its type (into a B3 declaration).  Assumes that the variable is already
   * defined in the current environment.
   */
  def translateLocalVarSig(typ:sil.Type, v:sil.LocalVar, isMutable: Boolean = true): Variable
  def translateLocalVarDecl(l: sil.LocalVarDecl, isMutable: Boolean = true): Variable = {
    translateLocalVarSig(l.typ,l.localVar, isMutable)
  }

  /**
   * Translate a method parameter (local variable) along with its type (into a B3 procedure parameter).  Assumes that the variable is already
   * defined in the current environment.
   */
  def translateLocalVarSigMethodParam(typ:sil.Type, v:sil.LocalVar, inoutMode: RawAst.ParameterMode = IN): PParameter
  def translateLocalVarDeclMethodParam(l: sil.LocalVarDecl, inoutMode: RawAst.ParameterMode = IN): PParameter = {
    translateLocalVarSigMethodParam(l.typ,l.localVar, inoutMode)
  }

  /**
   * Translate a function parameter (local variable) along with its type (into a B3 function parameter).  Assumes that the variable is already
   * defined in the current environment.
   */
  def translateLocalVarSigFuncParam(typ:sil.Type, v:sil.LocalVar, isInjective:Boolean = false): FParameter
  def translateLocalVarDeclFuncParam(l: sil.LocalVarDecl, isInjective: Boolean = false): FParameter = {
    translateLocalVarSigFuncParam(l.typ,l.localVar, isInjective)
  }

  /** The current environment. */
  var env: Environment = null

  /** The namespace for Viper local variables. */
  def silVarNamespace: Namespace

  /** Used to encode assumptions made about valid values of a given type */
  /** the "isParameter" flag can be used to select assumptions which only apply to parameters */
  def allAssumptionsAboutValue(typ:sil.Type, arg: Variable, isParameter: Boolean): Stmt
  def allAssumptionsAboutValue(arg:sil.LocalVarDecl, isParameter: Boolean) : Stmt = {
    allAssumptionsAboutValue(arg.typ,translateLocalVarDecl(arg),isParameter)
  }

}
