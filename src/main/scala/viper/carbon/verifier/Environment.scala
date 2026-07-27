// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2021 ETH Zurich.

package viper.carbon.verifier

import viper.silver.{ast => sil}
import viper.carbon.b3.B3NameGenerator
import viper.carbon.b3.B3Nodes.IdExpr
import viper.carbon.b3.B3Naming._

/**
 * An environment that assigns unique names to Viper variables;  in SIL, loops can have
 * local variables and thus a method might have two declarations of a local variable
 * with the same name (in different loops). While B3 does not require all Variable names
 * to be unique (it supports variable scopes and even shadowing), we cannot just use the
 * SIL names because we generate new variables (variables for if conditions; variables 
 * for verifying Loops; ...), which have to be unique. Therefore it makes more sense to 
 * just keep the current system of ensuring unique variable names. This is less complicated
 * and can at most slow down the transformation step by some minicule amount of time.
 */
case class Environment(verifier: Verifier, member: sil.Node) {

  private val names = new B3NameGenerator()

  /** The current mapping of variables. */
  private val currentMapping = collection.mutable.HashMap[sil.LocalVar, IdExpr]()

  /** Records the generated B3 names of all translated Viper variables. */
  private val allUsedNames = collection.mutable.HashMap[String, String]()

  // register types from member
  member match {
    case sil.Method(_, args, returns, _, _, _) =>
      for (v <- args ++ returns) {
        define(v.localVar)
      } 
    case f@sil.Function(_, args, _, _, _, _) =>
      for (v <- args) {
        define(v.localVar)
      }
    case sil.Predicate(_, args, _) =>
      for (v <- args) {
        define(v.localVar)
      }
    case f@sil.DomainFunc(_, args, _, _, _) =>
      for (v <- args) {
        v match {
          case n: sil.LocalVarDecl => define(n.localVar)
          case u: sil.UnnamedLocalVarDecl => define(sil.LocalVar(uniqueName(f.name + "_param"), u.typ)(u.pos, u.info, u.errT))
        }
      //? for (v <- args if (v.isInstanceOf[sil.LocalVarDecl])) {
      //?   define(v.asInstanceOf[sil.LocalVarDecl].localVar)
      }
    case _ =>
  }

  def currentNameMapping : Map[String, String] = allUsedNames.toMap

  /**
   * Returns the B3 variable for a given Viper variable (it has to be defined first,
   * otherwise an error is thrown).
   */
  def get(variable: sil.LocalVar): IdExpr = {
    currentMapping.get(variable) match {
      case Some(t) => t
      case None => sys.error(s"Internal Error: variable $variable is not defined.")
    }
  }

  /**
   * Defines a local variable in this environment for a given Viper variable, and returns the corresponding
   * B3 variable.
   */
  def define(variable: sil.LocalVar): IdExpr = {
    currentMapping.get(variable) match {
      case Some(t) =>
        sys.error(s"Internal Error: variable $variable is already defined.")
      case None =>
        val name = uniqueName(variable.name)
        val bvar = IdExpr(Identifier(name)(verifier.mainModule.silVarNamespace), verifier.typeModule.translateType(variable.typ))
        currentMapping.put(variable, bvar)
        allUsedNames.update(variable.name, name)
        bvar
    }
  }

  def allDefinedVariables() : Set[sil.LocalVar] = currentMapping.keys.toSet

  def allDefinedNames(p : sil.Program) : Set[String] =
    allDefinedVariables().map(_.name) union p.scopedDecls.map(_.name).toSet

  def isDefinedAt(variable: sil.LocalVar) : Boolean = currentMapping.isDefinedAt(variable)

  def makeUniquelyNamed(decl: sil.LocalVarDecl) : sil.LocalVarDecl =
    if (isDefinedAt(decl.localVar)) new sil.LocalVarDecl(this.uniqueName(decl.localVar.name),decl.typ)(decl.pos, decl.info) else decl

  def undefine(variable: sil.LocalVar): Unit = {
    require(currentMapping.contains(variable))
    currentMapping.remove(variable)
  }

  /**
   * Takes a string and tries to produce a similar string that is not already used.
   */
  def uniqueName(s: String): String = {
    names.createUniqueIdentifier(s)
  }
}
