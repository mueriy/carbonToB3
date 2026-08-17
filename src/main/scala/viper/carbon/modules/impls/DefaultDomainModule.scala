// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2021 ETH Zurich.

package viper.carbon.modules.impls

import viper.carbon.modules.{DomainModule, StatelessComponent}
import viper.silver.{ast => sil}
import viper.carbon.b3.B3Nodes._
import viper.carbon.b3.B3Naming._
import viper.carbon.verifier.{Environment, Verifier}
import viper.carbon.b3.B3Implicits._
import viper.silver.ast.NamedDomainAxiom

/**
 * The default implementation of [[viper.carbon.modules.DomainModule]].
 */
class DefaultDomainModule(val verifier: Verifier) extends DomainModule with StatelessComponent {

  import verifier._
  import typeModule._
  import expModule._
  import mainModule._

  def name = "Domain module"

  implicit val namespace = verifier.freshNamespace("domain")

  // name for output identifier (to try to avoid clashes - should be improved for robustness (see issue #19)
  def outputName(domain: sil.Domain) : String = domain.name + "DomainType"

// /*
  override def translateDomain(domain: sil.Domain): Seq[Decl] = {
    if (!domain.typVars.isEmpty) {
      sys.error("B3 LATER: Domains with typ vars are currently not supported!")
    }
    val prevState = stateModule.state
    stateModule.replaceState(stateModule.pureState)
    val fs = domain.functions.filter(f => f.interpretation.isEmpty) flatMap translateDomainFunction
    val as = domain.axioms flatMap translateDomainAxiom
    //s"The type for domain ${domain.name}"
    val ts = TypeDecl(NamedType(this.outputName(domain) , domain.typVars map (tv => TypeVar(tv.name))))
    stateModule.replaceState(prevState)
    //s"Translation of domain ${domain.name}"
    ts ++ fs ++ as
  }

  private def translateDomainFunction(f: sil.DomainFunc): Seq[Decl] = {
    env = Environment(verifier, f)
    val t = translateType(f.typ)
    val res = if (f.unique) {
      sys.error("B3 ADVANCED: *unique* domain functions currently not supported!")
      // B3 ADVANCED: this involves setting up a central place for declaring taggers
      // Also, we could rollback to using unique instead of tag in ConstDecl, as all tags for
      // unique should have the same name anyways. Although, there might be "constants" that
      // we want to be "unique", but not in comparison to general unique "constants"..., then
      // this doesnt work anymore. ("complexity >>> use" is the reason this is ADVANCED) 
/* 
      val func = ConstDecl(Identifier(f.name), t, unique = true)
      MaybeCommentedDecl(s"Translation of domain unique function ${f.name}", func, size = 1)
*/
    } else {
      val args = f.formalArgs map (x => FParameter(Identifier(if (x.isInstanceOf[sil.LocalVarDecl]) x.asInstanceOf[sil.LocalVarDecl].name else env.uniqueName(f.name + "_param")), translateType(x.typ)))
      val func = Function(Identifier(f.name), args, t)
      //s"Translation of domain function ${f.name}"
      func
    }
    env = null
    res
  }

  private def translateDomainAxiom(axiom: sil.DomainAxiom): Seq[Decl] = {
    env = Environment(verifier, axiom)
    //(AS): I believe this is not needed, as locals are introduced in the translation
    //mainModule.defineLocalVars(axiom)

    //if (axiom.isInstanceOf[NamedDomainAxiom])
    //  (s"Translation of domain axiom ${axiom.asInstanceOf[NamedDomainAxiom].name}")
    //else
    //  (s"Translation of anonymous domain axiom")
    val res = Axiom(translateExp(axiom.exp))
    //mainModule.undefineLocalVars(axiom)
    env = null
    res
  }

  override def translateDomainFuncApp(fa: sil.DomainFuncApp): Expr = {
    val funct = verifier.program.findDomainFunction(fa.funcname)
    if (funct.unique) {
      Const(Identifier(funct.name), translateType(fa.typ))
    } else {
      val res = FunctionCallExpr(Identifier(funct.name), fa.args map translateExp, translateType(fa.typ))
      // res.showReturnType = true //B3 NOTE: not needed for the B3 RawAst 
      res
    }
  }

  override def translateDomainTyp(typ: sil.DomainType): Type = {
    val domain = verifier.program.findDomain(typ.domainName)
    if (!domain.typVars.isEmpty) {
      sys.error("B3 LATER: Domains with typ vars are currently not supported!")
    }
    val typArgs = domain.typVars map (tv => typ.typVarsMap.getOrElse(tv, tv))
    NamedType(this.outputName(domain), typArgs map translateType)
  }

}
