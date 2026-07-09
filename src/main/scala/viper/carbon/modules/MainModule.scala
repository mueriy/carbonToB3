// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2021 ETH Zurich.

package viper.carbon.modules

import viper.silver.{ast => sil}
import viper.carbon.b3.B3Nodes._
import viper.carbon.b3.B3Naming.Namespace
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

  // B3 TODO: Check if these options make sense. Maybe we need P/F-Parameter creation and Quantifier Bindings.
  /**
   * Translate a local variable along with its type (into a B3 VarDecl).  Assumes that the variable is already
   * defined in the current environment.
   */
  def translateLocalVarSigToVarDecl(typ:sil.Type, v:sil.LocalVar, isMutable: Boolean = true): VarDecl
  def translateLocalVarDeclToVarDecl(l: sil.LocalVarDecl, isMutable: Boolean = true): VarDecl = {
    translateLocalVarSigToVarDecl(l.typ,l.localVar, isMutable)
  }

  /**
   * Translate a local variable along with its type (into a B3 Predicate parameter).  Assumes that the variable is already
   * defined in the current environment.
   */
  def translateLocalVarSigToPParameter(typ:sil.Type, v:sil.LocalVar, inoutMode: RawAst.ParameterMode = IN): PParameter
  def translateLocalVarDeclToPParameter(l: sil.LocalVarDecl, inoutMode: RawAst.ParameterMode = IN): PParameter = {
    translateLocalVarSigToPParameter(l.typ,l.localVar, inoutMode)
  }

  /**
   * Translate a local variable along with its type (into a B3 Function parameter).  Assumes that the variable is already
   * defined in the current environment.
   */
  def translateLocalVarSigToFParameter(typ:sil.Type, v:sil.LocalVar, isInjective: Boolean = false): FParameter
  def translateLocalVarDeclToFParameter(l: sil.LocalVarDecl, isInjective: Boolean = false): FParameter = {
    translateLocalVarSigToFParameter(l.typ,l.localVar, isInjective)
  }

  /**
   * Translate a local variable along with its type (into a B3 Binding (Forall/Exists)).  Assumes that the variable is already
   * defined in the current environment.
   */
  def translateLocalVarSigToBinding(typ:sil.Type, v:sil.LocalVar): Binding
  def translateLocalVarDeclToBinding(l: sil.LocalVarDecl): Binding = {
    translateLocalVarSigToBinding(l.typ,l.localVar)
  }

  /** The current environment. */
  var env: Environment = null

  /** The namespace for Viper local variables. */
  def silVarNamespace: Namespace

  /** Used to encode assumptions made about valid values of a given type */
  /** the "isParameter" flag can be used to select assumptions which only apply to parameters */
  def allAssumptionsAboutValue(typ:sil.Type, arg: LocalVarDecl, isParameter: Boolean): Stmt
  def allAssumptionsAboutBoundValue(arg:sil.LocalVarDecl, isParameter: Boolean) : Stmt = {
    allAssumptionsAboutValue(arg.typ,translateLocalVarDeclToBinding(arg),isParameter)
  }
  def allAssumptionsAboutPParameter(arg:sil.LocalVarDecl, isParameter: Boolean, inoutMode: RawAst.ParameterMode = IN) : Stmt = {
    allAssumptionsAboutValue(arg.typ,translateLocalVarDeclToPParameter(arg, inoutMode),isParameter)
  }
  def allAssumptionsAboutFParameter(arg:sil.LocalVarDecl, isParameter: Boolean, isInjective: Boolean = false) : Stmt = {
    allAssumptionsAboutValue(arg.typ,translateLocalVarDeclToFParameter(arg, isInjective),isParameter)
  }

}
