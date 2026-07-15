// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2021 ETH Zurich.

package viper.carbon.modules.impls

import viper.carbon.modules._
import viper.silver.ast.utility.Expressions
import viper.silver.{ast => sil}
import viper.carbon.b3.B3Implicits._
import viper.carbon.b3.B3Nodes._
import viper.carbon.b3.B3Naming._
import viper.carbon.b3.B3Development._
import viper.carbon.b3.ErrorMemberMapping

import java.text.SimpleDateFormat
import java.util.Date
import viper.carbon.verifier.Environment
import viper.silver.verifier.{TypecheckerWarning, errors}
import viper.carbon.verifier.Verifier
import viper.silver.ast.Quasihavoc
import viper.silver.ast.utility.rewriter.Traverse
import viper.silver.reporter.{Reporter, WarningsDuringTypechecking, QuantifierChosenTriggersMessage}

import scala.collection.mutable

/**
 * The default implementation of a [[viper.carbon.modules.MainModule]].
 */
class DefaultMainModule(val verifier: Verifier) extends MainModule with StatelessComponent {

  import verifier._
  import typeModule._
  import stmtModule._
  import exhaleModule._
  import heapModule._
  import funcPredModule._
  import domainModule._
  import expModule._

  import stateModule._

  def name = "Main module"

  override val silVarNamespace = verifier.freshNamespace("main.silver")
  implicit val mainNamespace = verifier.freshNamespace("main")

  override def translateLocalVarSigToBinding(typ: sil.Type, v: sil.LocalVar): Binding = {
    Binding(env.get(v).name, translateType(typ))
  }
  override def translateLocalVarSigToPParameter(typ:sil.Type, v:sil.LocalVar, inoutMode: RawAst.ParameterMode = IN): PParameter = {
    PParameter(env.get(v).name, translateType(typ), inoutMode)
  }
  override def translateLocalVarSigToFParameter(typ:sil.Type, v:sil.LocalVar, isInjective: Boolean = false): FParameter = {
    FParameter(env.get(v).name, translateType(typ), isInjective)
  }
  override def translateLocalVarSigToVarDecl(typ:sil.Type, v:sil.LocalVar, isInjective: Boolean = false): VarDecl = {
    VarDecl(env.get(v).name, EmptyStmt, translateType(typ), isInjective)
  }
  // override def translateLocalVarSig(typ:sil.Type, v:sil.LocalVar): LocalVarDecl = {
  //   val t: Type = translateType(typ)
  //   val name: Identifier = env.get(v).name
  //   LocalVarDecl(name, t)
  // } B3 TODO: go over the above functions, this was the original version, but we need it for all LocalVarDecl types

  override def translate(p: sil.Program, reporter: Reporter): (Program, Map[String, Map[String, String]]) = {

    verifier.replaceProgram(
      p.transform(
        {
          case f: sil.Forall => {
            val res = f.autoTrigger
            if (res.triggers.isEmpty) {
              reporter.report(WarningsDuringTypechecking(Seq(TypecheckerWarning("No triggers provided or inferred for quantifier.", res.pos))))
            }
            reporter report QuantifierChosenTriggersMessage(res, res.triggers, f.triggers)
            res
          }
          case e: sil.Exists => {
            val res = e.autoTrigger
            if (res.triggers.isEmpty) {
              reporter.report(WarningsDuringTypechecking(Seq(TypecheckerWarning("No triggers provided or inferred for quantifier.", res.pos))))
            }
            reporter report QuantifierChosenTriggersMessage(res, res.triggers, e.triggers)
            res
          }
          case q: Quasihavoc => desugarQuasihavoc(q)
        },
        Traverse.TopDown)
    )

    val backendFuncs = new mutable.HashSet[sil.DomainFunc]()
    for (d <- p.domains) {
      backendFuncs.addAll(d.functions.filter(f => f.interpretation.isDefined))
    }

    // We record the B3 names of all Viper variables in this map.
    // The format is Viper member name -> (Viper variable name -> B3 variable name).
    var nameMaps : Map[String, mutable.HashMap[String, String]] = null


    val output = verifier.program match {
      case sil.Program(domains, fields, functions, predicates, methods, extensions) =>
        // translate all members

        // important to convert Seq to List to force the methods to be translated, otherwise it's possible that
        // evaluation happens lazily, which can lead to incorrect behaviour (evaluation order is important here)
/* B3 TODO2
        val translatedFields = (fields flatMap translateField).toList
*/
        nameMaps = (methods ++ functions ++ predicates).map(_.name -> new mutable.HashMap[String, String]()).toMap
        val members = // (domains flatMap translateDomainDecl) ++ //B3 TODO
/* B3 TODO2
          translatedFields ++
*/
          (functions flatMap (f => translateFunction(f, nameMaps.get(f.name)))) ++
/* B3 TODO2
          (predicates flatMap (p => translatePredicate(p, nameMaps.get(p.name)))) ++
*/
          (methods flatMap (m => translateMethodDecl(m, nameMaps.get(m.name)))) //++
/* B3 TODO: What is a backend function?
          (backendFuncs flatMap translateBackendFunc)
*/

        // get the preambles (only at the end, even if we add it at the beginning)
        val preambles: Seq[Decl] = verifier.allModules flatMap {
          m =>
            //"Preamble of ${m.name}."
            if (m.preamble.size > 0) m.preamble
            else Nil
        }
        
        // B3: Removed the header information for debugging, because we cannot add that to a B3-AST

        val declCollection = members ++ preambles

        // We need to split the "Decl"s into their own groups, because there is no overarching Decl type in the B3 AST 
        val b3signatureTypes: Seq[String] = Seq() // Not sure what this is. Is a new, unexplained feature.
        val b3domains: Seq[Domain] = declCollection collect {case m: Domain => m} 
        var b3types: Seq[TypeDecl] = declCollection collect {case m: TypeDecl => m} 
        val b3taggers: Seq[Tagger] = declCollection collect {case m: Tagger => m} 
        var b3functions: Seq[Function] = declCollection collect {case m: Function => m} 
        val b3axioms: Seq[Axiom] = declCollection collect {case m: Axiom => m} 
        val b3procedures: Seq[Procedure] = declCollection collect {case m: Procedure => m} 
        
        if (!(declCollection collect {case m: Seq[Any] => m}).isEmpty) {
          sys.error("bad declCollection is the bug")
        }


        // ADDING SOME DECLs MANUALLY DURING DEVELOPMENT
        if (!b3types.contains(TypeDecl(NamedType("HeapType")))) {
          b3types = b3types ++ TypeDecl(NamedType("HeapType"))
        }
        if (!b3types.contains(TypeDecl(NamedType("MaskType")))) {
          b3types = b3types ++ TypeDecl(NamedType("MaskType"))
        }
        if (!b3types.contains(TypeDecl(NamedType("FrameType")))) {
          b3types = b3types ++ TypeDecl(NamedType("FrameType"))
        }
        val stateFunc = Function(stateModule.staticGoodState.asInstanceOf[FunctionCallExpr].name, stateModule.staticStateContributions(), Bool)
        if (!b3functions.contains(stateFunc)) {
          b3functions = b3functions ++ stateFunc
        }
        val AssumeFunctionsAbove = Function(funcPredModule.TODO_REMOVE_assumeFunctionsAboveName, Seq(), Int)
        if (!b3functions.contains(AssumeFunctionsAbove)) {
          b3functions = b3functions ++ AssumeFunctionsAbove
        }
        val emptyFrame = Function(funcPredModule.TODO_REMOVE_emptyFrameName, Seq(), NamedType("FrameType"))
        if (!b3functions.contains(emptyFrame)) {
          b3functions = b3functions ++ emptyFrame
        }
        Program(b3signatureTypes, b3domains, b3types, b3taggers, b3functions, b3axioms, b3procedures)
    }

    (output.optimized.asInstanceOf[Program], nameMaps.map(e => e._1 -> e._2.toMap))
  }

  def translateMethodDecl(m: sil.Method, names: Option[mutable.Map[String, String]]): Seq[Decl] = {
    val mWithLoopInfo = loopModule.initializeMethod(m)

    env = Environment(verifier, mWithLoopInfo)
    ErrorMemberMapping.currentMember = mWithLoopInfo
    val res = mWithLoopInfo match {
      case method @ sil.Method(name, formalArgs, formalReturns, pres, posts, _) =>
        
        // Translate Parameters
        val ins: Seq[PParameter] = formalArgs map {translateLocalVarDeclToPParameter(_, IN)}
        val outs: Seq[PParameter] = formalReturns map {translateLocalVarDeclToPParameter(_, OUT)}
        //B3 INFO: Viper does not have inout parameters. However, for our heap and permission 
        // implementation we need variables that can be some value at the start and then can be modified.
        //(B3 TODO: since we don't actually call any procedures, we could also just use a vardecl? 
        //  What are the (dis)advantages of these two options?)
        val inouts: Seq[PParameter] = Seq() //B3 TODO: add global variables here!

        // Translate individual parts for the procedure body
        // "Initializing the state"
        val init = stateModule.initBoogieState ++ assumeAllFunctionDefinitions 
/* B3 TODO2 (perm): ++
          (if (verifier.respectFunctionPrecPermAmounts) Nil else permModule.assumePermUpperBounds(true)) ++ stmtModule.initStmt(method.bodyOrAssumeFalse)
*/
        //"Initializing the old state"
        val initOld = TODO_Stmt("translateMethodDecl", "initOld")//stateModule.initOldState
        //"Assumptions about method arguments"
        val paramAssumptions = TODO_Stmt("translateMethodDecl", "paramAssumptions")//mWithLoopInfo.formalArgs map (a => allAssumptionsAboutValue(a.typ, translateLocalVarDeclToPParameter(a), true))
        //""Checked inhaling of precondition"
        val inhalePre = TODO_Stmt("translateMethodDecl", "inhalePre")//translateMethodDeclPre(pres)
        //"Checked inhaling of postcondition to check definedness"
        val checkPost = TODO_Stmt("translateMethodDecl", "checkPost")  /* = if (posts.nonEmpty) {
          translateMethodDeclCheckPosts(posts)
        }
        else Nil */
        val postsWithErrors = posts map (p => (p, errors.PostconditionViolated(p, mWithLoopInfo)))
        //"Exhaling postcondition"
        val exhalePost = TODO_Stmt("translateMethodDecl", "exhalePost")//exhaleWithoutDefinedness(postsWithErrors)
        //Translate Method body -> Procedure body
        val mainBody = translateStmt(method.bodyOrAssumeFalse)
          /* TODO: Might be worth special-casing on methods with empty bodies */
        
        val body = Block(init ++ paramAssumptions ++ inhalePre ++ initOld ++ checkPost ++ mainBody ++ exhalePost)

        /* B3 INFO: In the case of boogie, the variable declarations where added by PrettyPrinter at the end, simply 
        printing all of them at the start of the procedure body. We will use the same approach for now. VarDecl nodes 
        in B3 have a body that is a Stmt (e.g. a Block-Stmt). The scope of the declared variable is ONLY in that body! 
        Even if another statement is in the same Body-Stmt, coming just after the VarDecl-Stmt, it CANNOT use that 
        variable (since it is out of scope). Therefore we pack the body inside multiple layers of VarDecl's, until
        we declared all undeclared variables in the procedure body.*/
        /* B3 QUEST: Theoretically we could make the scope of variables smaller by declaring them more locally. 
        This could decrease the nr of variables in the context, making the SMT solver a bit more efficient. However,
        we would have to be very carefull with the scopes. (The scope of a VarDecl inside another is necessarily
        a subset of the outer VarDecl's scope, so the outer VarDecl's scope must go at least as far as needed for
        the inner VarDecls.) Maybe this is possible; at least in some situations? */
        /* B3 INFO: (ADVANCED) In PrettyPrinter we also see handling of LocalVarWhereDecl's. B3 doesn't have "where", 
        so if we need tha twe would likely have to handle that somewhere else. But this is why it is missing here. */

        // collect all variables that are not declared in the body or by a parameter
        val undecl = body.undeclLocalVars filter (v1 => (ins ++ outs ++ inouts).forall(v2 => v2.name != v1.name))
        // pack procedure body in layers of VarDecls (one layer per undeclared variable)
        val bodyWithDecls = undecl.foldRight(body.asInstanceOf[Stmt])((l, r) => VarDecl(l.name, r, l.typ)) 
      
        //s"Translation of method $name"
        Procedure(name = Identifier(name), 
                  parameters = ins ++ outs ++ inouts,
                  pre = Seq(), post = Seq(),
                  body = Some(bodyWithDecls))
    }

    if (names.isDefined){
      val usedNames = env.currentNameMapping
      // add all local vars
      names.get ++= usedNames
    }

    env = null
    ErrorMemberMapping.currentMember = null
    res
  }

/*
  // B3 TODO
  private def translateMethodDeclCheckPosts(posts: Seq[sil.Exp]): Stmt = {
    val (freshStateStmtAux, state) = stateModule.freshTempState("Post", discardCurrent = true, initialise = true)
    val freshStateStmt = freshStateStmtAux ++ stateModule.assumeGoodState

    // note that the order here matters - onlyExhalePosts should be computed with respect to the reset state
    val onlyExhalePosts: Seq[Stmt] = inhaleModule.inhaleExhaleSpecWithDefinednessCheck(
    posts, {
      errors.ContractNotWellformed(_)
    })

    val stmts = (
      if (Expressions.contains[sil.InhaleExhaleExp](posts)) {
        // Postcondition contains InhaleExhale expression.
        // Need to check inhale and exhale parts separately.
        val onlyInhalePosts: Seq[Stmt] = inhaleModule.inhaleInhaleSpecWithDefinednessCheck(
        posts, {
          errors.ContractNotWellformed(_)
        })

        NondetIf(
          freshStateStmt ++
          MaybeComment("Checked inhaling of postcondition to check definedness",
            MaybeCommentBlock("Do welldefinedness check of the inhale part.",
              NondetIf(onlyInhalePosts ++ Assume(FalseLit()))) ++
              MaybeCommentBlock("Normally inhale the exhale part.",
                onlyExhalePosts)
          ) ++
          MaybeComment("Stop execution", Assume(FalseLit()))
        )
      }
      else {
        NondetIf(
          freshStateStmt ++
          MaybeComment("Checked inhaling of postcondition to check definedness", onlyExhalePosts) ++
            MaybeComment("Stop execution", Assume(FalseLit()))
        )
      })

    stateModule.replaceState(state)

    stmts
  }


  // B3 TODO
  private def translateMethodDeclPre(pres: Seq[sil.Exp]): Stmt = {
    val res = if (Expressions.contains[sil.InhaleExhaleExp](pres)) {
      // Precondition contains InhaleExhale expression.
      // Need to check inhale and exhale parts separately.
      val onlyExhalePres: Seq[Stmt] = inhaleModule.inhaleExhaleSpecWithDefinednessCheck(
      pres, {
        errors.ContractNotWellformed(_)
      })
      val onlyInhalePres: Seq[Stmt] = inhaleModule.inhaleInhaleSpecWithDefinednessCheck(
      pres, {
        errors.ContractNotWellformed(_)
      })
      MaybeCommentBlock("Checked inhaling of precondition",
        MaybeCommentBlock("Do welldefinedness check of the exhale part.",
          NondetIf(onlyExhalePres ++ Assume(FalseLit()))) ++
          MaybeCommentBlock("Normally inhale the inhale part.",
            onlyInhalePres)
      )
    }
    else {
      val inhalePres: Seq[Stmt] = inhaleModule.inhaleInhaleSpecWithDefinednessCheck(
      pres, {
        errors.ContractNotWellformed(_)
      })
      MaybeCommentBlock("Checked inhaling of precondition", inhalePres)
    }

    res
  }
*/

  override def allAssumptionsAboutValue(typ:sil.Type, arg: LocalVarDecl, isParameter:Boolean): Stmt = {
    val tmp = verifier.allModules map (_.validValue(typ, arg.l, isParameter))
    val assumptions = tmp.filter(_.isDefined).map(_.get)
    assumptions.allOption match {
      case None => Nil
      case Some(e) => Assume(e)
    }
  }

/*
  // B3 TODO
  def translateDomainDecl(d: sil.Domain): Seq[Decl] = {
    env = Environment(verifier, d)
    val res = translateDomain(d)
    env = null
    res
  }
*/

  /***
    * Desugar a quasihavoc into an exhale followed by an inhale statement
    * @param q should be a field or pedicate quasihavoc
    * @return
    */
  private def desugarQuasihavoc(q: sil.Quasihavoc) = {
    val curPermVarDecl = sil.LocalVarDecl("perm_temp_quasihavoc_", sil.Perm)()
    val curPermVar = curPermVarDecl.localVar
    val resourceCurPerm =
      q.exp match {
        case r : sil.FieldAccess =>
          sil.FieldAccessPredicate(r, Some(curPermVar))()
        case r: sil.PredicateAccess =>
          sil.PredicateAccessPredicate(r, Some(curPermVar))()
        case _ => sys.error("Not supported resource in quasihavoc")
      }

    val curPermInhExPermission =
      sil.Seqn(
        sil.LocalVarAssign(curPermVar, sil.CurrentPerm(q.exp)())() +:
          Seq(
            sil.Exhale(resourceCurPerm)(),
            sil.Inhale(resourceCurPerm)()
          )
        ,
        Seq(curPermVarDecl)
      )()

    q.lhs match {
      case Some(cond) =>
        sil.If(cond,
          curPermInhExPermission,
          sil.Seqn(Seq(), Seq())()
        )()
      case None =>
        sil.Seqn(curPermInhExPermission, Seq())()
    }
  }
}
