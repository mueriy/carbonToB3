// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2021 ETH Zurich.

package viper.carbon.modules.impls

import viper.carbon.modules.{ExpModule, StatelessComponent}
import viper.silver.{ast => sil}
import viper.carbon.b3.B3Nodes._
import viper.carbon.b3.B3Implicits._
import viper.carbon.b3.B3Development._
import viper.carbon.b3.B3Naming.Identifier
import viper.carbon.b3.Transformer
import viper.carbon.verifier.Verifier
import viper.silver.verifier.{PartialVerificationError, reasons}
import viper.carbon.modules.components.{DefinednessComponent, DefinednessState}
import viper.silver.ast.{LocationAccess, MagicWand, PredicateAccess, Ref}
import viper.silver.ast.utility.Expressions

/**
 * The default implementation of [[viper.carbon.modules.ExpModule]].
 */
class DefaultExpModule(val verifier: Verifier) extends ExpModule with DefinednessComponent with StatelessComponent {

  import verifier._
  import heapModule._
  import domainModule._
  import seqModule._
  import setModule._
  import mapModule._
  import permModule._
  import inhaleModule._
  import funcPredModule._
  import exhaleModule._
  import stateModule._
  import mainModule._

  override def start(): Unit = {
    register(this)
  }

  def name = "Expression module"

  // B3 ADVANCED: (wand)
  override def translateExpInWand(e: sil.Exp): Expr = {
    val duringPackageStmt = wandModule.nestingDepth > 0
    if(duringPackageStmt){
      ADVANCED_Expr_bool("DefaultExpModule", "translateExpInWand")
/*
      val oldCurState = stateModule.state
      stateModule.replaceState(wandModule.UNIONState.asInstanceOf[StateRep].state)  // state in which 'e' is evaluated

      val stmt = translateExp(e)

      stateModule.replaceState(oldCurState)

      stmt
*/
    }else{
      translateExp(e)
    }
  }

  override def translateExp(e: sil.Exp): Expr = {
    e match {
      case sil.IntLit(i) =>
        IntLit(i)
      case sil.BoolLit(b) =>
        BoolLit(b)
      case sil.NullLit() =>
        translateNull
      case l@sil.LocalVar(_, _) =>
        translateLocalVar(l)
      case r@sil.Result(typ) =>
        translateResult(r)
      case f@sil.FieldAccess(_, _) =>
        translateResourceAccess(f)
      case sil.InhaleExhaleExp(_, _) =>
        sys.error("should not occur here (either, we inhale or exhale this expression, in which case whenInhaling/whenExhaling should be used, or the expression is not allowed to occur.")
      case p@sil.PredicateAccess(_, _) =>
        LATER_Expr_bool("predicates", "translateExp->sil.PredicateAccess")//translateResourceAccess(p)
      case w: sil.MagicWand =>
        ADVANCED_Expr_bool("wand", "translateExp->sil.MagicWand")//translateResourceAccess(w)
      case sil.Unfolding(_, exp) =>
        translateExp(exp)
      case sil.Applying(_, exp) => translateExp(exp)
      case sil.Asserting(_, exp) => translateExp(exp)
      case sil.Old(exp) =>
        //B3 QUEST: why did Carbon not use boogie's old()-expression?
        val prevState = stateModule.state
        stateModule.replaceState(stateModule.oldState)
        val res = translateExp(exp)
        stateModule.replaceState(prevState)
        res
      case sil.LabelledOld(exp, oldLabel) =>
        LATER_Expr_bool("LabelledOld", "translateExp->sil.LabelledOld")
/*
        var findLabel = oldLabel
        if(findLabel.equals("lhs"))
          findLabel = findLabel+wandModule.getActiveLhs()
        val prevState = stateModule.state
        val labelState = LabelHelper.getLabelState[stateModule.StateSnapshot](
          findLabel,
          stateModule.freshTempStateKeepCurrent,
          stateModule.stateRepositoryGet, stateModule.stateRepositoryPut)
        stateModule.replaceState(labelState)
        val res = translateExp(exp)
        stateModule.replaceState(prevState)
        res
*/
      case sil.Let(lvardecl, exp, body) =>
        ADVANCED_Expr_bool("Let", "DExpM->translateExp->sil.Let")
/*
        val translatedExp = translateExp(exp) // expression to bind "v" to
      val v = env.makeUniquelyNamed(lvardecl) // choose a fresh "v" binder
        env.define(v.localVar)
        val translatedBody = translateExp(Expressions.instantiateVariables(body, Seq(lvardecl), Seq(v.localVar), env.allDefinedNames(program))) // translate body with "v" in place of bound variable
      val substitutedBody = translatedBody.replace(env.get(v.localVar), translatedExp) // now replace all "v"s with expression. Doing this after translation avoids constructs such as heap-dependant expressions getting reevaluated after substitution in the wrong heaps (e.g. if substituted into an "old" expression).
        env.undefine(v.localVar)
        substitutedBody
*/
      case sil.CondExp(cond, thn, els) =>
        CondExp(translateExp(cond), translateExp(thn), translateExp(els))
      case q@sil.Exists(vars, triggers, exp) => {
        // alpha renaming, to avoid clashes in context
        val renamedVars: Seq[sil.LocalVarDecl] = vars map (v => {
          val v1 = env.makeUniquelyNamed(v); env.define(v1.localVar); v1
        });
        val renaming = (e: sil.Exp) => Expressions.instantiateVariables(e, (vars map (_.localVar)), renamedVars map (_.localVar))
        val ts : Seq[Pattern] = (triggers map
          (t => (funcPredModule.toExpressionsUsedInTriggers(t.exps map (e => translateExp(renaming(e)))))
            map (Pattern(_)) // build a trigger for each sequence element returned (in general, one original trigger can yield multiple alternative new triggers)
            )).flatten
        val weight = q.info match {
          case sil.WeightedQuantifier(value) => Some(value)
          case _ => None
        }
        val res = Exists(renamedVars map translateLocalVarDeclToBinding, ts, translateExp(renaming(exp))) //B3 INFO: 'weight' was used here, but is currently not supported by B3
        renamedVars map (v => env.undefine(v.localVar))
        res
      }
      case q@sil.Forall(vars, triggers, exp) => {
        // alpha renaming, to avoid clashes in context
        val renamedVars: Seq[sil.LocalVarDecl] = vars map (v => {
          val v1 = env.makeUniquelyNamed(v); env.define(v1.localVar); v1
        });
        val renaming = (e: sil.Exp) => Expressions.instantiateVariables(e, (vars map (_.localVar)), renamedVars map (_.localVar))
        val ts : Seq[Pattern] = (triggers map
          (t => (funcPredModule.toExpressionsUsedInTriggers(t.exps map (e => translateExp(renaming(e)))))
            map (Pattern(_)) // build a trigger for each sequence element returned (in general, one original trigger can yield multiple alternative new triggers)
            )).flatten
        val weight = q.info match {
          case sil.WeightedQuantifier(value) => Some(value)
          case _ => None
        }
        val res = Forall(renamedVars map translateLocalVarDeclToBinding, ts, translateExp(renaming(exp))) //B3 INFO: 'weight' was used here, but is currently not supported by B3
        renamedVars map (v => env.undefine(v.localVar))
        res
      }
      case sil.ForPerm(variables, accessRes, body) => {
        LATER_Expr_bool("translateExp", "sil.ForPerm")
/*
        val locations = Seq(accessRes)

        // alpha renaming, to avoid clashes in context
        val renamedVars: Seq[sil.LocalVarDecl] = variables.map(variable => {
          val v1 = env.makeUniquelyNamed(variable); env.define(v1.localVar); v1
        })
        val renaming = (e: sil.Exp) => Expressions.instantiateVariables(e, variables.map(_.localVar), renamedVars.map(_.localVar))
        val perResFilter: sil.ResourceAccess => (Expr, Seq[Trigger]) = resAcc => {
          val zipped = variables.map(_.localVar) zip renamedVars.map(_.localVar)
          val replacements = zipped.toMap

          val substitutedResAccess: sil.ResourceAccess = resAcc.replace(replacements)
          val maskRead = currentPermission(substitutedResAccess)
          val heapRead = translateResourceAccess(substitutedResAccess)
          (hasDirectPerm(substitutedResAccess), Seq(Trigger(maskRead), Trigger(heapRead)))
        }
        val filter = locations.foldLeft[(Expr, Seq[Trigger])](BoolLit(false), Seq())((soFar, loc) => soFar match {
          case (exp, triggers) =>
            perResFilter(loc) match {
              case (newExp, newTriggers) => (BinExp(exp, Or, newExp), triggers ++ newTriggers)
            }
        })

        val res = Forall(renamedVars.map(renamedVar => translateLocalVarDecl(renamedVar)), filter._2, // no triggers yet :(
          BinExp(filter._1, Implies, translateExp(renaming(body))))
        renamedVars.foreach(renamedVar => env.undefine(renamedVar.localVar))
        res
*/
      }
      case sil.WildcardPerm() =>
        LATER_Expr_int("translateExp(sil.WildcardPerm)")//translatePerm(e)
      case sil.FullPerm() =>
        LATER_Expr_int("translateExp(sil.FullPerm)")//translatePerm(e)
      case sil.NoPerm() =>
        LATER_Expr_int("translateExp(sil.NoPerm)")//translatePerm(e)
      case sil.EpsilonPerm() =>
        LATER_Expr_int("translateExp(sil.EpsilonPerm)")//translatePerm(e)
      case sil.PermMinus(_) =>
        LATER_Expr_int("translateExp(sil.PermMinus)")//translatePerm(e)
      case sil.CurrentPerm(_) =>
        LATER_Expr_int("translateExp(sil.CurrentPerm)")//translatePerm(e)
      case sil.FractionalPerm(_, _) =>
        LATER_Expr_int("translateExp(sil.FractionalPerm)")//translatePerm(e)
      case sil.AccessPredicate(_, _) =>
        sys.error("not handled by expression module")
      case sil.EqCmp(left, right) =>
        left.typ match {
          case _: sil.SeqType =>
            translateSeqExp(e)
          case _: sil.SetType =>
            translateSetExp(e)
          case _: sil.MultisetType =>
            translateSetExp(e)
          case _: sil.MapType =>
            translateMapExp(e)
          case x if x == sil.Perm =>
            LATER_Expr_bool("translateExp", "EqCmp sil.Perm")//translatePermComparison(e)
          case _ =>
            translateExp(left) === translateExp(right)
        }
      case sil.NeCmp(left, right) =>
        left.typ match {
          case _: sil.SeqType =>
            translateSeqExp(e)
          case _: sil.SetType =>
            translateSetExp(e)
          case _: sil.MultisetType =>
            translateSetExp(e)
          case _: sil.MapType =>
            translateMapExp(e)
          case x if x == sil.Perm =>
            LATER_Expr_bool("translateExp", "NeCmp sil.Perm")//translatePermComparison(e)
          case _ =>
            translateExp(left) !== translateExp(right)
        }
      case sil.DomainBinExp(_, sil.PermGeOp, _) |
           sil.DomainBinExp(_, sil.PermGtOp, _) |
           sil.DomainBinExp(_, sil.PermLeOp, _) |
           sil.DomainBinExp(_, sil.PermLtOp, _) =>
        LATER_Expr_bool("translateExp(sil.DomainBinExLogic)")//translatePermComparison(e)
      case sil.DomainBinExp(_, sil.PermAddOp, _) |
           sil.DomainBinExp(_, sil.PermMulOp, _) |
           sil.DomainBinExp(_, sil.PermSubOp, _) |
           sil.DomainBinExp(_, sil.IntPermMulOp, _) |
           sil.DomainBinExp(_, sil.FracOp, _) |
           sil.DomainBinExp(_, sil.PermDivOp, _) =>
        LATER_Expr_int("translateExp(sil.DomainBinExCalc)")//translatePerm(e)
      case sil.DomainBinExp(left, op, right) =>
        var reverse = false
        val bop = op match {
          case sil.OrOp => Or
          case sil.LeOp => LeCmp
          case sil.LtOp => LtCmp
          case sil.GeOp => reverse = true; LeCmp // B3 does not support GeCmp, but we can just use LeCmp and reverse lhs <-> rhs,
          case sil.GtOp => reverse = true; LtCmp // since a >= b <==> b <= a (and same for GtCmp) 
          case sil.AddOp => Add
          case sil.SubOp => Sub
          case sil.DivOp => IntDiv
          case sil.ModOp => Mod
          case sil.MulOp => Mul
          case sil.AndOp => And
          case sil.ImpliesOp => Implies
          case _ =>
            sys.error("Expression translation did not match any cases (should be handled before reaching translateExp code)" + e.getClass())
        }
        if (reverse) {
          OpExpr(bop, Seq(translateExp(right), translateExp(left)))
        } else {
          OpExpr(bop, Seq(translateExp(left), translateExp(right)))
        }
      case sil.Minus(exp) =>
        translateExp(exp).neg
      case sil.Not(exp) =>
        translateExp(exp).not
      case fa@sil.FuncApp(_, _) =>
        translateFuncApp(fa)
      case fa@sil.DomainFuncApp(_, _, _) =>
        translateDomainFuncApp(fa)
      case fa@sil.BackendFuncApp(_, _) =>
        ADVANCED_Expr_bool("Backend", "translateExp->sil.BackendFuncApp")
/*         
        translateBackendFuncApp(fa)
 */
      case seqExp@sil.EmptySeq(_) =>
        translateSeqExp(seqExp)
      case seqExp@sil.ExplicitSeq(_) =>
        translateSeqExp(seqExp)
      case seqExp@sil.RangeSeq(_, _) =>
        translateSeqExp(seqExp)
      case seqExp@sil.SeqAppend(_, _) =>
        translateSeqExp(seqExp)
      case seqExp@sil.SeqIndex(_, _) =>
        translateSeqExp(seqExp)
      case seqExp@sil.SeqTake(_, _) =>
        translateSeqExp(seqExp)
      case seqExp@sil.SeqDrop(_, _) =>
        translateSeqExp(seqExp)
      case seqExp@sil.SeqContains(_, _) =>
        translateSeqExp(seqExp)
      case seqExp@sil.SeqUpdate(_, _, _) =>
        translateSeqExp(seqExp)
      case seqExp@sil.SeqLength(_) =>
        translateSeqExp(seqExp)

      case setExp@sil.EmptySet(_) => translateSetExp(setExp)
      case setExp@sil.ExplicitSet(_) => translateSetExp(setExp)
      case setExp@sil.EmptyMultiset(_) => translateSetExp(setExp)
      case setExp@sil.ExplicitMultiset(_) => translateSetExp(setExp)
      case setExp@sil.AnySetUnion(_, _) => translateSetExp(setExp)
      case setExp@sil.AnySetIntersection(_, _) => translateSetExp(setExp)
      case setExp@sil.AnySetSubset(_, _) => translateSetExp(setExp)
      case setExp@sil.AnySetMinus(_, _) => translateSetExp(setExp)
      case setExp@sil.AnySetContains(_, _) => translateSetExp(setExp)
      case setExp@sil.AnySetCardinality(_) => translateSetExp(setExp)

      case mapExp: sil.EmptyMap => translateMapExp(mapExp)
      case mapExp: sil.ExplicitMap => translateMapExp(mapExp)
      case mapExp: sil.Maplet => translateMapExp(mapExp)
      case mapExp: sil.MapCardinality => translateMapExp(mapExp)
      case mapExp: sil.MapContains => translateMapExp(mapExp)
      case mapExp: sil.MapDomain => translateMapExp(mapExp)
      case mapExp: sil.MapRange => translateMapExp(mapExp)
      case mapExp: sil.MapLookup => translateMapExp(mapExp)
      case mapExp: sil.MapUpdate => translateMapExp(mapExp)

      case _ => sys.error("Viper expression didn't match any existing case.")
    }
  }

  override def translateLocalVar(l: sil.LocalVar): IdExpr = {
    env.get(l)
  }

  override def simplePartialCheckDefinednessAfter(e: sil.Exp, error: PartialVerificationError, makeChecks: Boolean,
                                                  definednessStateOpt: Option[DefinednessState]): Stmt = {

    val stmt: Stmt = (if (makeChecks)
      e match {
        case sil.Div(_, b) =>
            Assert(translateExp(b) !== IntLit(0), error.dueTo(reasons.DivisionByZero(b)), 4)
        case sil.Mod(_, b) =>
            Assert(translateExp(b) !== IntLit(0), error.dueTo(reasons.DivisionByZero(b)), 4)
        case sil.FractionalPerm(_, b) =>
            Assert(translateExp(b) !== IntLit(0), error.dueTo(reasons.DivisionByZero(b)), 4)
        case _ => Nil
      }
    else Nil)

    stmt
  }

  override def checkDefinedness(e: sil.Exp, error: PartialVerificationError, makeChecks: Boolean,
                                definednessStateOpt: Option[DefinednessState] = None,
                                duringPackageStmt: Boolean = false, ignoreIfInWand: Boolean = false): Stmt = {

    if(duringPackageStmt && ignoreIfInWand)  // ignore the check
      return EmptyStmt

    val oldCurState = stateModule.state
    if(duringPackageStmt) {
      sys.error("B3 ADVANCED (wand): DefaultExpModule -> checkDefinedness -> duringPackageStmt is true?!")
/*
      stateModule.replaceState(wandModule.UNIONState.asInstanceOf[StateRep].state)
*/
    }

    val definednessDescription =
      if(makeChecks) {
        s"Check definedness of $e"
      } else {
        s"Execute definedness check of $e without enforcing the checks (e.g., to gain more information)"
      }

    //definednessDescription
    val stmt = checkDefinednessImpl(e, error, makeChecks = makeChecks, definednessStateOpt)

    if(duringPackageStmt) {
      ADVANCED_Stmt("wand", "DExpM->checkDefinedness->wand-part")
/*
      stateModule.replaceState(oldCurState)
      If(wandModule.getCurOpsBoolvar(), stmt, Statements.EmptyStmt)
*/
    }else stmt
  }

  private def checkDefinednessImpl(e: sil.Exp, error: PartialVerificationError, makeChecks: Boolean,
                                   definednessStateOpt: Option[DefinednessState]): Stmt = {
    e match {
      case sil.And(e1, e2) =>
        checkDefinednessImpl(e1, error, makeChecks = makeChecks, definednessStateOpt) ::
          If(translateExp(Expressions.asBooleanExp(e1)), checkDefinednessImpl(e2, error, makeChecks = makeChecks, definednessStateOpt), EmptyStmt) ::
          Nil
      case sil.Implies(e1, e2) =>
        checkDefinednessImpl(e1, error, makeChecks = makeChecks, definednessStateOpt) ::
          If(translateExp(e1), checkDefinednessImpl(e2, error, makeChecks = makeChecks, definednessStateOpt), EmptyStmt) ::
          Nil
      case sil.CondExp(c, e1, e2) =>
        checkDefinednessImpl(c, error, makeChecks = makeChecks, definednessStateOpt) ::
          If(translateExp(c),
            checkDefinednessImpl(e1, error, makeChecks = makeChecks, definednessStateOpt),
            checkDefinednessImpl(e2, error, makeChecks = makeChecks, definednessStateOpt)
          ) :: Nil
      case sil.Or(e1, e2) =>
        checkDefinednessImpl(e1, error, makeChecks = makeChecks, definednessStateOpt) :: // short-circuiting evaluation:
          If(OpExpr(Not, translateExp(e1)), checkDefinednessImpl(e2, error, makeChecks = makeChecks, definednessStateOpt), EmptyStmt) ::
          Nil
      case sil.Asserting(assertion, e) =>
        val checkAssDefined = checkDefinedness(assertion, error, makeChecks = makeChecks)
        val (stateStmt, state) = stateModule.freshTempState("asserting")
        //"Exhale assertion of asserting"
        val checkAssHolds = exhale(Seq((assertion, error, Some(error))), B3Code = +0)
        stateModule.replaceState(state)
        val checkEDefined = checkDefinedness(e, error, makeChecks = makeChecks)
        checkAssDefined :: stateStmt :: checkAssHolds :: checkEDefined :: Nil
      case w@sil.MagicWand(_, _) =>
        ADVANCED_Stmt("wand", "DExpM->checkDefinednessImpl->sil.MagicWand")
        // checkDefinednessWand(w, error, makeChecks = makeChecks)
      case sil.Let(v, e, body) =>
        checkDefinednessImpl(e, error, makeChecks = makeChecks, definednessStateOpt) ::
        {
          val u = env.makeUniquelyNamed(v) // choose a fresh "v" binder
          env.define(u.localVar)
          Assign(translateLocalVar(u.localVar), translateExp(e)) ::
          checkDefinednessImpl(body.replace(v.localVar, u.localVar), error, makeChecks = makeChecks, definednessStateOpt) ::
            {
              env.undefine(u.localVar)
              Nil
            }
        }
      case _ =>
        def translate(e: sil.Exp, definednessStateOptInTranslate: Option[DefinednessState]): Block = {
          val checks = components map (_.partialCheckDefinedness(e, error, makeChecks = makeChecks, definednessStateOptInTranslate))
          val stmt = checks map (_._1())

          // AS: note that some implementations of the definedness checks rely on the order of these calls (i.e. parent nodes are checked before children, and children *are* always checked after parents.
          val stmt2 = for (sub <- subexpressionsForDefinedness(e)) yield {
            checkDefinednessImpl(sub, error, makeChecks = makeChecks, definednessStateOptInTranslate)
          }
          val stmt3 = checks map (_._2())

          e match {
            case sil.MagicWand(_, _) =>
              sys.error("wand subnodes:" + e.subnodes.toString() +
                "stmt:" + stmt.toString() +
                "stmt2:" + stmt2.toString() +
                "stmt3:" + stmt3.toString())
            case _ => Nil
          }

          stmt ++ stmt2 ++ stmt3 ++
          //"Free assumptions (exp module)"
            allFreeAssumptions(e)
        }

        if (e.isInstanceOf[sil.QuantifiedExp]) {
          val orig_vars = e.asInstanceOf[sil.QuantifiedExp].variables
          val bound_vars = orig_vars.map(v => env.makeUniquelyNamed(v))
          bound_vars map (v => env.define(v.localVar))
          val res = if (e.isInstanceOf[sil.ForPerm]) {
            val eAsForallRef = Expressions.renameVariables(e, orig_vars.map(_.localVar), bound_vars.map(_.localVar)).asInstanceOf[sil.ForPerm]

            val filter: Expr = LATER_Expr_bool("checkDefinednessImpl", "val filter needs hasDirectPerm(...)")//hasDirectPerm(eAsForallRef.resource)

            handleQuantifiedLocals(bound_vars, If(filter, translate(eAsForallRef, definednessStateOpt), Nil))
          } else {
            handleQuantifiedLocals(bound_vars, translate(Expressions.renameVariables(e, orig_vars.map(_.localVar), bound_vars.map(_.localVar)), definednessStateOpt))
          }
          bound_vars map (v => env.undefine(v.localVar))
          res
        } else e match {
          case sil.Old(_) =>
            val prevState = stateModule.state
            stateModule.replaceState(stateModule.oldState)
            val res = translate(e, None) //definedness state is the old state (i.e., same as currently set state)
            stateModule.replaceState(prevState)
            res
          case sil.LabelledOld(_, oldLabel) =>
            ADVANCED_Stmt("LabelledOld", "DExpM->checkDefinednessimpl->sil.LabelledOld")
/*
            var findLabel = oldLabel
            if(findLabel.equals("lhs"))
              findLabel = "lhs"+wandModule.getActiveLhs()
            val prevState = stateModule.state
            val labelState = LabelHelper.getLabelState[stateModule.StateSnapshot](
              findLabel,
              stateModule.freshTempStateKeepCurrent,
              stateModule.stateRepositoryGet, stateModule.stateRepositoryPut)
            stateModule.replaceState(labelState)
            val res = translate(e, None) //definedness state is the labelled old state (i.e., same as currently set state)
            stateModule.replaceState(prevState)
            res
*/
          case _ =>
            translate(e, definednessStateOpt)
        }
    }
  }

  /***
    * Returns subexpressions that are relevant for definedness checks
    */
  private def subexpressionsForDefinedness(e: sil.Exp) : Seq[sil.Exp] = {
    e match {
      case sil.AccessPredicate(res : sil.LocationAccess, perm) => res.subExps ++ Seq(perm)
      case sil.CurrentPerm(res: sil.LocationAccess) => res.subExps
      case _ => e.subExps
    }
  }

/* B3 ADVANCED
  /**
    * checks self-framedness of both sides of wand
    * GP: maybe should "MagicWandNotWellFormed" error
    */
  private def checkDefinednessWand(e: sil.MagicWand, error: PartialVerificationError, makeChecks: Boolean): Stmt = {
    val (initStmtLHS, curState): (Stmt, stateModule.StateSnapshot) = stateModule.freshEmptyState("WandDefLHS", true)
    val defStateLHS = stateModule.state
    val (initStmtRHS, _): (Stmt, stateModule.StateSnapshot) = stateModule.freshEmptyState("WandDefRHS", true)
    val defStateRHS = stateModule.state

    stateModule.replaceState(defStateLHS)
    val lhs = initStmtLHS ++ inhaleWithDefinednessCheck(e.left, error)
    val lhsID = wandModule.getNewLhsID() // identifier for the lhs of the wand to be referred to later when 'old(lhs)' is used
    val defineLHS = stmtModule.translateStmt(sil.Label("lhs"+lhsID, Nil)(e.pos, e.info))
    wandModule.pushToActiveWandsStack(lhsID)
    stateModule.replaceState(defStateRHS)
    val rhs = initStmtRHS ++ inhaleWithDefinednessCheck(e.right, error)
    wandModule.popFromActiveWandsStack()
    stateModule.replaceState(curState)
    NondetIf(lhs ++ defineLHS ++ rhs ++ Assume(FalseLit()))
  }
*/ 

  def handleQuantifiedLocals(vars: Seq[sil.LocalVarDecl], res: Stmt): Stmt = {
    // introduce local variables for the variables in quantifications. we do this by first check
    // definedness without worrying about missing variable declarations, and then replace all of them
    // with fresh variables.
    val namespace = verifier.freshNamespace("exp.quantifier")
    val newVars = vars map (x => (translateLocalVar(x.localVar),
      // we use a fresh namespace to make sure we get fresh variables
      Identifier(x.name)(namespace)
      ))
    Choose(Block(Seq(Transformer.transform(res, {
      case v@IdExpr(name, _, isOld) =>
        newVars.find(x => (name == x._1.name)) match {
          case None => v // no change
          case Some((x, xb)) =>
            // use the new variable
            IdExpr(xb, x.typ, isOld)
        }
    })(),Assume(FalseLit()))))
  }

  override def allFreeAssumptions(e: sil.Exp): Stmt = {
    def translate: Block = {
      val stmt = components map (_.freeAssumptions(e))
      /**
       * Generally if e' is a subexpression of e then whenever e is inhaled/exhaled then any assumption that  can be
       * made for free if e' is inhaled/exhaled is also free.
       * If e is a magic wand then this is not true since what is inhaled is just the wand as a complete entity,
       * no assumptions can be made on the left and right hand side or their subexpressions as they contain assertions
       * which are only exhaled/inhaled when the wand is applied.
       */
      val stmt2 =
        e match {
          case sil.MagicWand(_,_) => Nil
          case sil.Forall(_,_,_) => Nil
          case sil.Exists(_,_,_) => Nil
          case sil.Let(v, e, body) => {
            val u = env.makeUniquelyNamed(v) // choose a fresh "v" binder
            env.define(u.localVar)
            val stmts = Assign(translateLocalVar(u.localVar), translateExp(e)) ++ allFreeAssumptions(body.replace(v.localVar,u.localVar))
            env.undefine(u.localVar)
            stmts
          }
          case _ =>
            for (sub <- e.subnodes if sub.isInstanceOf[sil.Exp]) yield {
              allFreeAssumptions(sub.asInstanceOf[sil.Exp])
            }
        }
      stmt ++ stmt2
    }
    if (e.isInstanceOf[sil.Old]) {
      val prevState = stateModule.state
      stateModule.replaceState(stateModule.oldState)
      val res = translate
      stateModule.replaceState(prevState)
      res
    } else {
      translate
    }
  }
}
