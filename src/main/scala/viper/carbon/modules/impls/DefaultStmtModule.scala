// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2021 ETH Zurich.

package viper.carbon.modules.impls

import viper.carbon.modules.{StatelessComponent, StmtModule}
import viper.carbon.modules.components.{DefinednessComponent, DefinednessState, SimpleStmtComponent}
import viper.silver.ast.utility.Expressions.{whenExhaling, whenInhaling}
import viper.silver.{ast => sil}
import viper.carbon.b3.B3Implicits._
import viper.carbon.b3.B3Naming._
import viper.carbon.b3.B3Nodes._
import viper.carbon.verifier.Verifier
import viper.silver.verifier.{PartialVerificationError, errors, reasons}
import viper.silver.ast.utility.Expressions

/**
 * The default implementation of a [[viper.carbon.modules.StmtModule]].
 */
class DefaultStmtModule(val verifier: Verifier) extends StmtModule with SimpleStmtComponent with StatelessComponent with DefinednessComponent {

  import verifier._
  import expModule._
  import stateModule._
  import exhaleModule._
  import inhaleModule._
  import funcPredModule._
  import wandModule._
  import typeModule._

  override def start(): Unit = {
    // this is the main translation, so it should come at the "beginning"; it defines the innermost code used in the translation; other modules can wrap this with their own code
    register(this, before = Seq(verifier.heapModule,verifier.permModule))
    // NOTE: this builds up the translation inside-out, so the *first* component defines the innermost code.
    // This works as follows, for statement translation: StmtModule, then PermModule, then HeapModule
    // For Fold statements: Heap module adds version/secondary mask code as a postfix to the main code from the StmtModule
    // For Field assignments: Heap module (which goes last) adds the translation of the actual operation as a postfix the other code (which checks well-definedness)
    // For MethodCall: assumptions about return values are added by the HeapModule as a postfix to the main translation in StmtModule
    // For New: the operation translation (HeapModule) is added as a prefix to the code adding permissions (PermModule)
    expModule.register(this)
  }

  private val lblNamespace = verifier.freshNamespace("stmt.lbl")
  var lblVarsNamespace = verifier.freshNamespace("var.lbl")
  private val tmpVarsNamespace = verifier.freshNamespace("stmt.tmpvar")
  override def labelNamespace = lblNamespace

  def name = "Statement module"

  /**
    * For each label we track a boolean that indicates whether the label has been defined in the trace
    */
  var labelBooleanGuards : collection.mutable.Map[String, LocalVarDecl] = new collection.mutable.HashMap[String, LocalVarDecl]()
  override def initStmt(methodBody: sil.Stmt): Stmt = {
    ADVANCED_Stmt("initStmt (DefaultStmtModule)", "this is required for labels")
/*
    labelBooleanGuards = new collection.mutable.HashMap[String, LocalVarDecl]()

    //create a boolean variable declaration for each label
    methodBody.visit(
      n => n match {
        case sil.Label(name,_) =>
          labelBooleanGuards.put(name, LocalVarDecl(Identifier(name+"_lblGuard")(lblVarsNamespace), Bool))
      }
    )

    for (boolDecls <- labelBooleanGuards.values.toList) yield {
      boolDecls.l := FalseLit()
    }
*/
  }

  /**
    * Takes a list of assertions, and executes all of the unfolding expressions inside. In particular, this means that the correct
   * predicate definitions get assumed, under the correct conditionals. Most checking of definedness is disabled (by the "false"
   * parameter to checkDefinedness), however, note that checking that the predicates themselves are held when unfolding is still
   * performed, because the code for that is an exhale. Because of this, it may be necessary to make sure that this operation is
   * called before/after the corresponding exhale/inhale of the assertions.
   */
/*
  def executeUnfoldings(exps: Seq[sil.Exp], exp_error: (sil.Exp => PartialVerificationError)): Stmt = {
    (exps map (exp => (if (exp.existsDefined[Unit]({case sil.Unfolding(_,_) => })) checkDefinedness(exp, exp_error(exp), false) else Nil:Stmt)))
  }
*/

  /**
    * Returns a function that takes a Stmt and then adds specific stmts in front of them. If the original stmt was a
    * Fold stmt, then the function will also add stmts to the back.
    *  
    * @param statesStackForPackageStmt stack of states used in translating package statements
    * @param insidePackageStmt Boolean that represents whether 'stmt' is being translated inside package statement or not
    * @param allStateAssms represents all assumptions about states on the statesStack
    *
    * These wand-related parameters are used when the method is called during packaging a wand.
    * For more details see the general node in 'wandModule'
    */
  override def handleStmt(s: sil.Stmt, statesStackForPackageStmt: List[Any] = null, allStateAssms: Expr = TrueLit(), insidePackageStmt: Boolean = false) : (Block => Block) = {
    s match {
      case s: sil.Fold => 
        val (bef, aft) = (TODO_Stmt("handleStmt: sil.Fold"), TODO_Stmt())//translateFold(s, statesStackForPackageStmt, insidePackageStmt)
        stmts => bef +++ stmts +++ aft // put new stmts in front and in back
      case _ =>  stmts => simpleHandleStmt(s, statesStackForPackageStmt, allStateAssms, insidePackageStmt) +++ stmts // put new stmts in front
    }
  } 

  /**
    * Defines what stmt to add in front of the stmt to be handled.
    * 
    * @param statesStack stack of states used in translating statements during packaging a wand (carries currentState and LHS of wands)
    * @param insidePackageStmt Boolean that represents whether 'stmt' is being translated inside package statement or not
    * @param allStateAssms represents all assumptions about states on the statesStack
    *
    * These wand-related parameters are used when the method is called during packaging a wand.
    * For more details see the general node in 'wandModule'
    */
  override def simpleHandleStmt(stmt: sil.Stmt, statesStack: List[Any] = null, allStateAssms: Expr = TrueLit(), insidePackageStmt: Boolean = false): Stmt = {
    if(loopModule.isLoopDummyStmt(stmt)) {
      //statement was just added for loop information purposes (only loopModule cares about it)
      return Nil
    }

    //In certain cases, definedness checks should not be included inside a package statement
    def maybeDefError(error: PartialVerificationError) : Option[PartialVerificationError] = {
      if(insidePackageStmt) { None } else { Some(error) }
    }

    stmt match {
      case assign@sil.LocalVarAssign(lhs, rhs) =>
        checkDefinedness(lhs, errors.AssignmentFailed(assign), insidePackageStmt = insidePackageStmt) ++
          checkDefinedness(rhs, errors.AssignmentFailed(assign), insidePackageStmt = insidePackageStmt) ++
        {if(insidePackageStmt)
          ADVANCED_Stmt("simpleHandleStmt", "sil.LocalVarAssign")// Assign(translateExpInWand(lhs), translateExpInWand(rhs))
        else
          Assign(translateExp(lhs).asInstanceOf[IdExpr], translateExp(rhs))}
      case assign@sil.FieldAssign(lhs, rhs) =>
        checkDefinedness(lhs.rcv, errors.AssignmentFailed(assign)) ++
          checkDefinedness(rhs, errors.AssignmentFailed(assign))
      case fold@sil.Fold(e) => sys.error("Internal error: translation of fold statement cannot be handled by simpleHandleStmt code; found:" + fold.toString())
      case unfold@sil.Unfold(e) =>
        LATER_Stmt("simpleHandleStmt", "sil.Unfold") //translateUnfold(unfold, statesStack, insidePackageStmt)
      case inh@sil.Inhale(e) =>
        LATER_Stmt("simpleHandleStmt", "sil.Inhale") //inhaleWithDefinednessCheck(whenInhaling(e), errors.InhaleFailed(inh), statesStack, insidePackageStmt)
      case exh@sil.Exhale(e) =>
        LATER_Stmt("simpleHandleStmt", "sil.Exhale")
/*
        val transformedExp = whenExhaling(e)
        val defErrorOpt = maybeDefError(errors.ExhaleFailed(exh))
        exhale(Seq((transformedExp, errors.ExhaleFailed(exh), defErrorOpt)), statesStackForPackageStmt = statesStack, insidePackageStmt = insidePackageStmt)
*/
      case a@sil.Assert(e) =>
        val transformedExp = whenExhaling(e)
        val defErrorOpt = maybeDefError(errors.AssertFailed(a))

        if (transformedExp.isPure) {
          // if e is pure, then assert and exhale are the same
          exhale(Seq((transformedExp, errors.AssertFailed(a), defErrorOpt)), statesStackForPackageStmt = statesStack, insidePackageStmt = insidePackageStmt)
        } else {
          TODO_Stmt("simpleHandleStmt", "sil.Assert")
/*
          // we create a temporary state to ignore the side-effects
          val (backup, snapshot) = freshTempState("Assert")
          val exhaleStmt = exhale(Seq((transformedExp, errors.AssertFailed(a), defErrorOpt)), isAssert =  true, statesStackForPackageStmt = statesStack, insidePackageStmt = insidePackageStmt, havocHeap = false)
          replaceState(snapshot)
          // B3 TODO: freshTempState must return a B3 backup Stmt! 
          exhaleStmt //backup :: exhaleStmt :: Nil
*/
        }
      case mc@sil.MethodCall(methodName, args, targets) =>
        val method = verifier.program.findMethod(methodName)
        // save pre-call state
        val (preCallStateStmt, state) = stateModule.freshTempState("PreCall")
        val preCallState = stateModule.state
        val oldState = stateModule.oldState
        stateModule.replaceState(state)
        val toUndefine = collection.mutable.ListBuffer[sil.LocalVar]()
        val actualArgs = args.zipWithIndex map (a => {
          val (actual, i) = a
          // use the concrete argument if it is just a variable or constant (to avoid code bloat)
          val useConcrete = actual match {
            case v: sil.LocalVar if !targets.contains(v) => true
            case _: sil.Literal => true
            case _ => false
          }
          if (!useConcrete) {
            val silFormal = method.formalArgs(i)
            val tempArg = sil.LocalVar("arg_" + silFormal.name, silFormal.typ)()
            mainModule.env.define(tempArg)
            toUndefine.append(tempArg)
            val translatedTempArg = translateExp(tempArg)
            val translatedActual = translateExp(actual)
            val stmt = translatedTempArg := translatedActual
            (tempArg, stmt, Some(actual))
          } else {
            (args(i), Nil: Stmt, None)
          }
        })
        val neededRenamings : Seq[(sil.AbstractLocalVar, sil.Exp)] = actualArgs.filter((_._3.isDefined)).map(element => (element._1.asInstanceOf[sil.LocalVar],element._3.get))
        val removingTriggers: (errors.ErrorNode => errors.ErrorNode) =
          ((n: errors.ErrorNode) => n.transform{case q: sil.Forall => q.copy(triggers = Nil)(q.pos, q.info, q.errT)})
        val renamingArguments : (errors.ErrorNode => errors.ErrorNode) = ((n:errors.ErrorNode) => removingTriggers(n).transform({
          case e:sil.Exp => Expressions.instantiateVariables[sil.Exp](e,neededRenamings map (_._1), neededRenamings map (_._2))
        }))

        val pres = method.pres map (e => Expressions.instantiateVariables(e, method.formalArgs ++ method.formalReturns, (actualArgs map (_._1)) ++ targets, mainModule.env.allDefinedNames(program)))
        val posts = method.posts map (e => Expressions.instantiateVariables(e, method.formalArgs ++ method.formalReturns, (actualArgs map (_._1)) ++ targets, mainModule.env.allDefinedNames(program)))
        val res = preCallStateStmt ++
          (targets map (e => checkDefinedness(e, errors.CallFailed(mc), insidePackageStmt = insidePackageStmt))) ++
          (args map (e => checkDefinedness(e, errors.CallFailed(mc), insidePackageStmt = insidePackageStmt))) ++
          (actualArgs map (_._2)) ++
          //"Exhaling precondition"
          //B3 LATER: unfolding: executeUnfoldings(pres, (pre => errors.PreconditionInCallFalse(mc).withReasonNodeTransformed(renamingArguments))) ++
            exhaleWithoutDefinedness(pres map (e => (e, errors.PreconditionInCallFalse(mc).withReasonNodeTransformed(renamingArguments))), statesStackForPackageStmt = statesStack, insidePackageStmt = insidePackageStmt) ++
          //"Havocing target variables"
          Reinit(Seq()) ++ //B3 TODO: correct var name list; was previously: "Havoc((targets map translateExp).asInstanceOf[Seq[Var]]) ++"
          {
            stateModule.replaceOldState(preCallState)
            //"Inhaling postcondition"
            val res = inhale(posts map (e => (e, errors.CallFailed(mc).withReasonNodeTransformed(renamingArguments))), addDefinednessChecks = false, statesStack, insidePackageStmt) //++
              //B3 LATER: unfold: // executeUnfoldings(posts, (post => errors.Internal(post).withReasonNodeTransformed(renamingArguments)))
            stateModule.replaceOldState(oldState)
            toUndefine map mainModule.env.undefine
            res
          }
        res
      case sil.While(_, _, _) =>
        //handled by LoopModule
        Nil
      case i@sil.If(cond, thn, els) =>
        val condTr = if(allStateAssms == TrueLit()) { translateExpInWand(cond) } else { allStateAssms ==> translateExpInWand(cond) }
        val condTempVar = IdExpr(Identifier("condition")(tmpVarsNamespace), Bool)
        checkDefinedness(cond, errors.IfFailed(cond), insidePackageStmt = insidePackageStmt) ++
        // Assign the condition to a temp var s.t. it's safe to optimize away the following if it's empty without
        // losing triggering expressions in the if-condition (see Carbon issue #420).
        Assign(condTempVar, condTr) ++ // B3 TODO?: Maybe we will need to make sure to declare this variable somewhere
        If(condTempVar,
          translateStmt(thn, statesStack, allStateAssms, insidePackageStmt),
          translateStmt(els, statesStack, allStateAssms, insidePackageStmt))
      case sil.Label(name, _) => {
        ADVANCED_Stmt("simpleHandleStmt", "sil.Label")
/*
        val labelState = LabelHelper.getLabelState[stateModule.StateSnapshot](
          name,
          stateModule.freshTempStateKeepCurrent,
          stateModule.stateRepositoryGet, stateModule.stateRepositoryPut)
        //first label, then init statement: otherwise gotos to this label will skip the initialization
        Label(Lbl(Identifier(name)(lblNamespace))) ++
          stateModule.initToCurrentStmt(labelState) ++
          labelBooleanGuards.get(name).fold[Stmt](Nil)(labelGuardDecl => Seq(labelGuardDecl.l := TrueLit()))  //label is defined
*/
      }
      case sil.Goto(_) =>
        /* Handled by loop module, since the loop module decides whether the goto should be translated as a goto. */
        Nil
      case pa@sil.Package(wand, proof) => {
        ADVANCED_Stmt("simpleHandleStmt", "sil.Package")
/*
        checkDefinedness(wand, errors.MagicWandNotWellformed(wand), insidePackageStmt = insidePackageStmt)
        translatePackage(pa, errors.PackageFailed(pa), statesStack, allStateAssms, insidePackageStmt)
*/
      }
      case a@sil.Apply(wand) =>
        ADVANCED_Stmt("simpleHandleStmt", "sil.Apply")
/*
        translateApply(a, errors.ApplyFailed(a), statesStack, allStateAssms, insidePackageStmt)
*/
      case _ =>
        Nil
    }
  }

  /**
    * @param statesStack   stack of states used in translating package statements (carries currentState and LHS of wands)
    * @param duringPackage Boolean that represents whether this exhale is inside package statement or not
    * @param allStateAssms represents all assumptions about states on the statesStack
    *
    * These wand-related parameters are used when the method is called during packaging a wand.
    * For more details see the general node in 'wandModule'
    */
  override def translateStmt(stmt: sil.Stmt, statesStack: List[Any] = null, allStateAssms: Expr = TrueLit(), duringPackage: Boolean = false): Stmt = {
/* B3 ADVANCED (wand)
    if(duringPackage) {
        wandModule.translatingStmtsInWandInit()
    }
*/

    // Seqn (sequence of stmts) => handle each Stmt individually, then return. Declare all local variables first
    stmt match {
      case sil.Seqn(ss, scopedDecls) =>
        val locals = scopedDecls.collect {case l: sil.LocalVarDecl => l}
        /* B3 INFO: In Boogie, VarDecl's were added by PrettyPrinter when printing the Procedure. In B3, VarDecl's have
        a Body, which defines its scope. Outside of this they are NOT declared. Because e.g. the while statement is replaced
        with a special non-while construction, getting the scope of LocalVarDecl's in a sil.Seqn correctly is not that 
        straightforward. (Since the variables could be also used in the parts added before and after the (translated) while body.)
        This means that it would be safer for now to rename the variables in e.g. while stmts as before and declaring all 
        variables "at the start" of the procedure. We should try to keep track of all "special locations" (like While), to
        then find a way to decrease the VarDecl scope wherever possible. I believe B3 does it like that because they think it
        is more efficient that way, although that would only be true if B3 parses written B3 code in a way that keeps the
        scopes actually small (<-- TODO: check this) 
        For now we do not declare the vars here and leave that to the Method->Procedure transformer.
        Special Stmts:
          - While
          - (probably) Wand (ADVANCED)
          - TODO: collect and add all here
        */

        // B3 TODO: check again if this really works. Saver option would be to just "VarDecl" the whole Procedure body)
        val localVars = locals map (v => mainModule.env.define(v.localVar)) // add local variables to environment
        //"Assumptions about local variables"
        val localVarsAssumptions = locals map (a => mainModule.allAssumptionsAboutValue(a.typ, mainModule.translateLocalVarDeclToVarDecl(a), true)) // B3 TODO: we need to find out what local variables are included here; theoretically this should only include VarDecl's, but what if Bindings are also included?
        val translatedStmts = (ss map (st => translateStmt(st, statesStack, allStateAssms, duringPackage)))
        val seqOfAllStmts = localVarsAssumptions ++ translatedStmts
        
        // In B3, VarDecl have a body and the scope of the Var is only in that body => need to nest all vardecls and place 
        //  the actual statement in the innermost place (body).
        val translatedStmt = seqOfAllStmts match {
          case Seq(oneStmt) => oneStmt
          case moreStmts => Block(moreStmts)
        }
        val translatedStmtWithVarDecls = localVars.foldRight(translatedStmt)((l, r) => VarDecl(l.name, r, l.typ)) 
        
        locals map (v => mainModule.env.undefine(v.localVar)) // remove local variables from environment
        // return to avoid the extra 'assumeGoodState'
        return seqOfAllStmts // (use "return translatedStmtWithVarDecls" instead to do the VarDecl here.)
      case _ =>
    }

    var stmts = Block(Nil)
    for (c <- components) { // NOTE: this builds up the translation inside-out, so the *first* component defines the innermost code.
      //      val (before, after) = c.handleStmt(stmt, statesStack, allStateAssms, inWand)
      //      stmts = before ++ stmts ++ after
      val f = c.handleStmt(stmt, statesStack, allStateAssms, duringPackage)
      stmts = f(stmts)
    }
    if (stmts.children.size == 0 && !loopModule.isLoopDummyStmt(stmt)) {
      assert(assertion = false, "Translation of " + stmt + " is not defined")
    }
    val translation = stmts ::
/*
      (if(duringPackage){  //[[B3 temp: remove heap-state assumptions after statements]]
        exchangeAssumesWithBoolean(assumeGoodState, statesStack.head.asInstanceOf[StateRep].boolVar)
      }else{
        assumeGoodState
      }) ::
*/
      Nil

    translation
  }


  override def simplePartialCheckDefinednessBefore(e: sil.Exp, error: PartialVerificationError, makeChecks: Boolean, definednessStateOpt: Option[DefinednessState]): Stmt = {
    if(makeChecks) {
      e match {
        case labelOld@sil.LabelledOld(_, labelName) =>
          ADVANCED_Stmt("simplePartialCheckDefinednessBefore", "sil.LabelledOld") //(sil.LabelledOld => old state of a var at a certain (specifically designed/labeled) position)
/*
          labelBooleanGuards.get(labelName) match {
            case Some(labelGuardDecl) =>
              Assert(labelGuardDecl.l, error.dueTo(reasons.LabelledStateNotReached(labelOld)))
            case None => Nil
          }
*/
        case _ => Nil
      }
    } else Nil
  }
}
