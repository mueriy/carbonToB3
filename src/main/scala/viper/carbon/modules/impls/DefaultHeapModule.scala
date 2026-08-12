// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2021 ETH Zurich.

package viper.carbon.modules.impls

import viper.carbon.modules._
import viper.carbon.modules.components.{DefinednessComponent, InhaleComponent, SimpleStmtComponent}
import viper.silver.ast.utility.Expressions
import viper.silver.{ast => sil}
import viper.carbon.b3.B3Nodes._
import viper.carbon.b3.B3Naming._
import viper.carbon.b3.B3Implicits._
import viper.carbon.b3.B3Development._
import viper.carbon.verifier.Verifier
import viper.carbon.utility.{PolyMapDesugarHelper, PolyMapRep}
import viper.silver.ast.utility.QuantifiedPermissions.QuantifiedPermissionAssertion
import viper.silver.verifier.PartialVerificationError
import viper.carbon.CarbonConfig
import viper.silver.ast.PredicateAccess

/**
 * The default implementation of a [[viper.carbon.modules.HeapModule]].
 */
class DefaultHeapModule(val verifier: Verifier)
    extends HeapModule
    with SimpleStmtComponent
    with DefinednessComponent {

  import verifier._
  import typeModule._
  import expModule._
  import stateModule._
  import permModule._
  import mainModule._

  def name = "Heap module"
  implicit val heapNamespace = verifier.freshNamespace("heap")
  val fieldNamespace = verifier.freshNamespace("heap.fields")
  // a fresh namespace for every axiom
  def axiomNamespace = verifier.freshNamespace("heap.axiom")

  override def start(): Unit = {
    stateModule.register(this)
    stmtModule.register(this)
    expModule.register(this)
  }

  var enableAllocationEncoding : Boolean = true // note: this may be modified on configuration, so should only be used e.g. in method defs which will be called later (e.g. during verification)

  private val fieldTypeName = "Field"
  private val normalFieldTypeName = "NormalField"
  private val normalFieldType = NamedType(normalFieldTypeName)
  override def addFieldMark(baseName: String, ftVars: Seq[Type]): String = s"${baseName}%%${fieldIdx(ftVars)}"
  private def fieldTagName(ftVars: Seq[Type]) = addFieldMark("%Tag_Field", ftVars)
  override def fieldType(ftVars: Seq[Type]) = NamedType(fieldTypeName, ftVars)
  override def fieldTypeOf(t: Type) = NamedType(fieldTypeName, Seq(normalFieldType, t))
  override def refType = NamedType("Ref")

  private val noFieldReplacement: Seq[Seq[Type]] = Seq(Seq(NamedType(""), NamedType("")))
  private var allFieldsTypVarsSeq: Seq[Seq[Type]] = noFieldReplacement

  override def allFieldsTypVars: Seq[Seq[Type]] = allFieldsTypVarsSeq

  private var fieldIdxMap = allFieldsTypVars.zipWithIndex.toMap
  override def fieldIdx(ftvars: Seq[Type]) = fieldIdxMap.getOrElse(ftvars, sys.error("non-field types versions cannot be indexed: "+ftvars))
  override def forallFields(formula: Seq[Type] => Any) = allFieldsTypVars map {formula(_)}


  override def fieldTypes = allFieldsTypVars map {NamedType(fieldTypeName, _)}
/*
  override def predicateVersionFieldTypeOf(p: sil.Predicate) =
    NamedType(fieldTypeName, Seq(predicateMetaTypeOf(p), funcPredModule.predicateVersionType))
  private def predicateMetaTypeOf(p: sil.Predicate) = NamedType("PredicateType_" + p.name)
  override def predicateVersionFieldType(genericT: String = "A") =
    NamedType(fieldTypeName, Seq(TypeVar(genericT), funcPredModule.predicateVersionType))
  override def predicateMaskFieldType: Type =
    NamedType(fieldTypeName, Seq(TypeVar("A"), pmaskType))
  override def predicateMaskFieldTypeOf(p: sil.Predicate): Type =
    NamedType(fieldTypeName, Seq(predicateMetaTypeOf(p), pmaskType))


  override def predicateMaskFieldTypeOfWand(wand: String): Type =
    NamedType(fieldTypeName, Seq(wandBasicType(wand), pmaskType))
  override def predicateVersionFieldTypeOfWand(wand: String) =
    NamedType(fieldTypeName, Seq(wandBasicType(wand), funcPredModule.predicateVersionType))


  override def wandBasicType(wand: String): Type = NamedType("WandType_" + wand)
  override def wandFieldType(wand: String) : Type = NamedType(fieldTypeName, Seq(wandBasicType(wand),Int))
*/
  private val heapTypeName = "%HeapType"
  private def heapTyp(ftvars: Seq[Type]) = NamedType(heapTypeName, ftvars)
  private def heapName(ftvars: Seq[Type]) = Identifier(addFieldMark("Heap", ftvars))
  private def exhaleHeapName(ftvars: Seq[Type]) = Identifier(addFieldMark("ExhaleHeap", ftvars))
  private def exhaleHeap(ftvars: Seq[Type]) = IdExpr(exhaleHeapName(ftvars), heapTyp(ftvars))
  private def constructOriginalHeap = allFieldsTypVars map {ftvars => IdExpr(heapName(ftvars), heapTyp(ftvars))}
  private var originalHeaps: Seq[IdExpr] = constructOriginalHeap
/*
  private val qpHeapName = Identifier("QPHeap")
  private val qpHeap = LocalVar(qpHeapName, heapTyp)
*/
  private var heaps: Seq[IdExpr] = originalHeaps
  private def heap(ftvars: Seq[Type]): IdExpr = heaps(fieldIdx(ftvars))
  private def heapVar(ftvars: Seq[Type]): IdExpr = {assert (!usingOldState); heap(ftvars)}
  private def heapExp(ftvars: Seq[Type]): Expr = if (usingPureState) dummyHeap(ftvars) else heap(ftvars)
  private val nullName = Identifier("null")
  private val nullLit = Const(nullName, refType)
  private val freshObjectName = Identifier("freshObj")
  private val freshObjectVar = IdExpr(freshObjectName, refType)

  // B3 NOTE: the old allocated, which was located on the Field NormalField Bool part of the heap
  // has the problem of not beeing on the same split as the references. It is hard to make an axiom
  // that uses different heap-splits to only use heap-splits from the same heap. Therefore, 
  // 'allocated' was relocated to the same split as the Refs. Instead of the allocated field storing
  // 'true' or 'false' it either stores 'isAlloc' or not, repectively. 'isAlloc' is just a randomly
  // defined reference. [keep this note above 'allocName']
  private lazy val allocName = if(enableAllocationEncoding) Identifier("$allocated")(fieldNamespace) else null
  private lazy val allocType = if(enableAllocationEncoding) fieldTypeOf(refType) else null
  private lazy val isAllocName = if(enableAllocationEncoding) Identifier("$isAllocated")(fieldNamespace) else null
  private lazy val isAllocType = if(enableAllocationEncoding) refType else null
  private lazy val isAlloc = if(enableAllocationEncoding) Const(isAllocName, isAllocType) else null

/*
  private val succHeapName = Identifier("succHeap")
  private val succHeapTransName = Identifier("succHeapTrans")
  private val identicalOnKnownLocsName = Identifier("IdenticalOnKnownLocations")
  private val identicalOnKnownLocsLiberalName = Identifier("IdenticalOnKnownLocationsLiberal")
*/
  private val isPredicateFieldName = Identifier("IsPredicateField")
/*
  private var PredIdMap:Map[String, BigInt] = Map()
  private var NextPredicateId:BigInt = 0
*/
  private val isWandFieldName = Identifier("IsWandField")
/*
  private val getPredicateOrWandIdName = Identifier("getPredWandId")
  private val sumHeapName = Identifier("SumHeap")
*/
  private val readHeapName = Identifier("readHeap")
  private val updateHeapName = Identifier("updHeap")
  private def dummyHeapName(ftvars: Seq[Type]) = Identifier(addFieldMark("dummyHeap", ftvars))
  private def dummyHeap(ftvars: Seq[Type]) = Const(dummyHeapName(ftvars), heapTyp(ftvars))



  override def fieldTypeConstructor = (ts: Seq[Type]) => fieldType(ts)
  override def heapMapRangeTypeFromField = (namedTyp: Type) => namedTyp.asInstanceOf[NamedType].typVars(1)

  override def preamble = {

    val obj = Binding(Identifier("o")(axiomNamespace), refType)
/* B3: currently unused
    val obj2 = Binding(Identifier("o2")(axiomNamespace), refType)
    def field(nr: Int, ftVars: Seq[Type]) = Binding(Identifier("f"+nr)(axiomNamespace), fieldType(ftVars))
*/
    def normalHeap(ft: NamedType) = Binding(heapName(ft.typVars), heapTyp(ft.typVars))

    val refField = Binding(Identifier("f")(axiomNamespace), fieldTypeOf(refType))
    val obj_refField = lookup(normalHeap(fieldTypeOf(refType)).l, obj.l, refField.l)
/* B3: currently unused
    val field = LocalVarDecl(Identifier("f")(axiomNamespace), fieldType)
    val field2 = LocalVarDecl(Identifier("f2")(axiomNamespace), NamedType(fieldTypeName, Seq(TypeVar("A2"), TypeVar("B2"))))
    val predField = LocalVarDecl(Identifier("pm_f")(axiomNamespace),
      predicateVersionFieldType("C"))
    val useSumOfStatesAxioms = loopModule.sumOfStatesAxiomRequired
*/

    // Register functions which we want to declare (only) concretely 
    registerFunction(readHeapName, Seq(0))
    registerFunction(updateHeapName, Seq(0))
    registerFunction(isPredicateFieldName, Seq(0))

    TypeDecl(refType) ++
      ConstDecl(nullName, refType) ++
      (fieldTypes map {TypeDecl(_)}) ++
      TypeDecl(normalFieldType) ++
      // Now we add all "one per Field-Type" Decls:
      (allFieldsTypVars flatMap {ftVars => {
        // Taggers for unique field values (used by 'translateField')
        Tagger(fieldTagName(ftVars), fieldType(ftVars)) ++
        // dummy heap also split!
        ConstDecl(dummyHeapName(ftVars), heapTyp(ftVars)) ++
        TypeDecl(heapTyp(ftVars))
      }}) ++ 
      (if(!enableAllocationEncoding) Nil else {
        // B3 NOTE: see note above val allocName
        ConstDecl(isAllocName, isAllocType) ++
        ConstDecl(allocName, allocType, Some(fieldTagName(allocType.typVars))) ++
        // all heap-lookups yield allocated objects or null
        Axiom(Forall(
          Seq(obj,
              refField,
              normalHeap(fieldTypeOf(refType))),
          Pattern(obj_refField),
          validReference(obj.l) ==> validReference(obj_refField)))
      }) ++
/* B3 ADVANCED (QPerm)
      Func(succHeapName,
        Seq(LocalVarDecl(heap0Name, heapTyp), LocalVarDecl(heap1Name, heapTyp)),
        Bool) ++
      Func(succHeapTransName,
        Seq(LocalVarDecl(heap0Name, heapTyp), LocalVarDecl(heap1Name, heapTyp)),
        Bool) ++
      Func(identicalOnKnownLocsName,
        Seq(LocalVarDecl(heapName, heapTyp), LocalVarDecl(exhaleHeapName, heapTyp)) ++ staticMask,
        Bool) ++
*/
/* B3 ADVANCED (goto)
      {
        if(useSumOfStatesAxioms)
          Func(identicalOnKnownLocsLiberalName,
            Seq(LocalVarDecl(heapName, heapTyp), LocalVarDecl(exhaleHeapName, heapTyp)) ++ staticMask,
            Bool)
        else Nil
      } ++
*/
// /* B3 NOTE: Alternative to current map system (we implement this here directly instead, which works better because of easier "concretization")
      {
        if(!verifier.usePolyMapsInEncoding) {
          val heapMapDesugarHelper = PolyMapDesugarHelper(allFieldsTypVars, refType, fieldTypeConstructor, heapNamespace)
          val heapDesugaringRep : PolyMapRep = heapMapDesugarHelper.desugarPolyMap(addFieldMark("H", _), heapTyp, (readHeapName, updateHeapName), heapMapRangeTypeFromField)
          heapDesugaringRep.select ++
          heapDesugaringRep.store ++
          //"Read and update axioms for the heap"
          heapDesugaringRep.axioms
        } else {
          Nil
        }
      } ++
// */
/* B3 LATER (predicates)
      Func(isPredicateFieldName,
        Seq(LocalVarDecl(Identifier("f"), fieldType)),
        Bool) ++
*/
/* B3 ADVANCED (wand)
      Func(isWandFieldName,
        Seq(LocalVarDecl(Identifier("f"), fieldType)),
        Bool) ++
      Func(getPredicateOrWandIdName,
        Seq(LocalVarDecl(Identifier("f"), fieldType)),
        Int) ++
*/
/* B3 ADVANCED (goto)
      {
        if(useSumOfStatesAxioms)
          Func(sumHeapName,
            Seq(LocalVarDecl(heapName, heapTyp), LocalVarDecl(heap1Name, heapTyp), LocalVarDecl(Identifier("mask1"), maskType),
              LocalVarDecl(heap2Name, heapTyp), LocalVarDecl(Identifier("mask2"), maskType)),
            Bool)
        else Nil
      } ++ 
*/         {
/* B3 LATER (predicates/wand/...): {
      val h = LocalVarDecl(heapName, heapTyp)
      val eh = LocalVarDecl(exhaleHeapName, heapTyp)
      val h0 = LocalVarDecl(heap0Name, heapTyp)
      val h1 = LocalVarDecl(heap1Name, heapTyp)
      val h2 = LocalVarDecl(heap2Name, heapTyp)
      val vars = Seq(h, eh) ++ staticMask
      val identicalFuncApp = FuncApp(identicalOnKnownLocsName, vars map (_.l), Bool)
      val identicalLiberalFuncApp = FuncApp(identicalOnKnownLocsLiberalName, vars map (_.l), Bool)

      identicalOnKnownLocsAxioms(false) ++
*/
/* B3 LATER (predicates)
        MaybeCommentedDecl("Updated Heaps are Successor Heaps", {
          val value = LocalVarDecl(Identifier("v"), TypeVar("B"));
          val upd = heapUpdate(h.l, obj.l, field.l, value.l)
          Axiom(Forall(
            Seq(h, obj, field, value),
            Trigger(Seq(upd))
            ,
            FuncApp(succHeapName, Seq(h.l, upd), Bool)
          ))
        }, size = 1) ++
        MaybeCommentedDecl("IdenticalOnKnownLocations Heaps are Successor Heaps",
          Axiom(Forall(
            vars,
            Trigger(Seq(identicalFuncApp))
            ,
            identicalFuncApp ==> FuncApp(succHeapName, Seq(h.l, eh.l), Bool)
          )), size = 1) ++
*/
/* B3 ADVANCED (Goto)
        {
          if (useSumOfStatesAxioms) {
            MaybeCommentedDecl("IdenticalOnKnownLiberalLocations Heaps are Successor Heaps",
              Axiom(Forall(
                vars,
                Trigger(Seq(identicalLiberalFuncApp))
                ,
                identicalLiberalFuncApp ==> FuncApp(succHeapName, Seq(h.l, eh.l), Bool)
              )), size = 1)
          } else {
            Nil
          }
        } ++
*/
/* B3 LATER (predicates)
      MaybeCommentedDecl("Successor Heaps are Transitive Successor Heaps", {
              val succHeapApp = FuncApp(succHeapName, Seq(h0.l, h1.l), Bool)
              Axiom(Forall(
                Seq(h0, h1),
                Trigger(Seq(succHeapApp))
                ,
                succHeapApp ==> FuncApp(succHeapTransName, Seq(h0.l, h1.l), Bool)
              ))
            }, size = 1) ++
        MaybeCommentedDecl("Transitivity of Transitive Successor Heaps", {
          val succHeapTransApp = FuncApp(succHeapTransName, Seq(h0.l, h1.l), Bool)
          val succHeapApp = FuncApp(succHeapName, Seq(h1.l, h2.l), Bool)
          Axiom(Forall(
            Seq(h0, h1, h2),
            Trigger(Seq(succHeapTransApp,succHeapApp))
            ,
            (succHeapTransApp && succHeapApp) ==> FuncApp(succHeapTransName, Seq(h0.l, h2.l), Bool) // NOTE: ignore IDE warning - these parentheses are NOT spurious, due to how the overloaded && and ==> get desugared
          ))
        }, size = 1) ++
*/
/* B3 ADVANCED (Goto)
        {
          if (useSumOfStatesAxioms) {
            identicalOnKnownLocsAxioms(true) ++
              MaybeCommentedDecl("Sum of heaps", {
                val mask1 = LocalVarDecl(Identifier("Mask1"),maskType)
                val mask2 = LocalVarDecl(Identifier("Mask2"),maskType)

                val sumStateApp = sumHeap(h.l, h1.l, mask1.l, h2.l, mask2.l)

                Axiom(Forall(
                  Seq(h, h1, mask1, h2, mask2),
                  Trigger(sumStateApp),
                  sumStateApp <==> (
                    FuncApp(identicalOnKnownLocsLiberalName, Seq(h1.l, h.l, mask1.l), Bool) &&
                      FuncApp(identicalOnKnownLocsLiberalName, Seq(h2.l, h.l, mask2.l), Bool)
                    )))
              })
          } else {
            Nil
          }
        }
*/
      Seq()
      }
    }

/*
  /* The liberal version does not equate the known-folded permission masks, but instead just propagates the information
   * that known-folded locations remain known-folded (while locations that are not known-folded are underspecified).
   * This permits taking the sum of two heaps that record different known-folded permission masks.
   */
  private def identicalOnKnownLocsAxioms(liberal: Boolean):Seq[Decl] = {
    val obj = LocalVarDecl(Identifier("o")(axiomNamespace), refType)
    val obj2 = LocalVarDecl(Identifier("o2")(axiomNamespace), refType)
    val field = LocalVarDecl(Identifier("f")(axiomNamespace), fieldType)
    val predField = LocalVarDecl(Identifier("pm_f")(axiomNamespace),
      predicateVersionFieldType("C"))
    val h = LocalVarDecl(heapName, heapTyp)
    val eh = LocalVarDecl(exhaleHeapName, heapTyp)
    val vars = Seq(h, eh) ++ staticMask

    val funcName = if (liberal) {
      identicalOnKnownLocsLiberalName
    } else {
      identicalOnKnownLocsName
    }

    val identicalFuncApp = FuncApp(funcName, vars map (_.l), Bool)
    // frame all locations with direct permission
    MaybeCommentedDecl("Frame all locations with direct permissions", Axiom(Forall(
      vars ++ Seq(obj, field),
      //        Trigger(Seq(identicalFuncApp, lookup(h.l, obj.l, field.l))) ++
      Trigger(Seq(identicalFuncApp, lookup(eh.l, obj.l, field.l))),
      identicalFuncApp ==>
        (staticPermissionPositive(obj.l, field.l) ==>
          (lookup(h.l, obj.l, field.l) === lookup(eh.l, obj.l, field.l)))
    )), size = 1) ++
    {
      // frame all predicate masks
      if(!liberal) {
        //equate permission mask maps
        MaybeCommentedDecl("Frame all predicate mask locations of predicates with direct permission", Axiom(Forall(
          vars ++ Seq(predField),
          Trigger(Seq(identicalFuncApp, isPredicateField(predField.l), lookup(eh.l, nullLit, predicateMaskField(predField.l)))),
          identicalFuncApp ==>
            ((staticPermissionPositive(nullLit, predField.l) && isPredicateField(predField.l)) ==>
              (lookup(h.l, nullLit, predicateMaskField(predField.l)) === lookup(eh.l, nullLit, predicateMaskField(predField.l))))
        )), size = 1)
      } else {
        //just propagate information that heap location is known-folded, but not that it is not known-folded
        MaybeCommentedDecl("Frame all predicate mask locations of predicates with direct permission. But don't propagate information " +
          " of locations that are not known-folded to allow for equating with multiple different (but compatible) heaps",
          Axiom(Forall( vars ++ Seq(predField),
          Trigger(Seq(identicalFuncApp, isPredicateField(predField.l), lookup(eh.l, nullLit, predicateMaskField(predField.l)))),
          identicalFuncApp ==>
            ((staticPermissionPositive(nullLit, predField.l) && isPredicateField(predField.l)) ==>
              Forall(Seq(obj2, field),
                Trigger(Seq(lookup(lookup(eh.l, nullLit, predicateMaskField(predField.l)), obj2.l, field.l, true))),
                (lookup(lookup(h.l, nullLit, predicateMaskField(predField.l)), obj2.l, field.l, true) ==>
                  lookup(lookup(eh.l, nullLit, predicateMaskField(predField.l)), obj2.l, field.l, true),
                ),
                field.typ.freeTypeVars
              )
        ))), size = 1)
      }
     }  ++
      // frame all locations with known folded permission
      MaybeCommentedDecl("Frame all locations with known folded permissions", Axiom(Forall(
        vars ++ Seq(predField),
        //Trigger(Seq(identicalFuncApp, lookup(h.l, nullLit, predicateMaskField(predField.l)), isPredicateField(predField.l))) ++
        // Trigger(Seq(identicalFuncApp, lookup(eh.l, nullLit, predField.l), isPredicateField(predField.l))) /*++
        Trigger(Seq(identicalFuncApp, isPredicateField(predField.l))) /*++
          Trigger(Seq(identicalFuncApp, lookup(eh.l, nullLit, predicateMaskField(predField.l)), isPredicateField(predField.l))) ++
          (verifier.program.predicates map (pred =>
            Trigger(Seq(identicalFuncApp, predicateTriggerAnyState(pred, predField.l), isPredicateField(predField.l))))
            )*/*/,
        identicalFuncApp ==>
          (
            (staticPermissionPositive(nullLit, predField.l) && isPredicateField(predField.l)) ==>
              Forall(Seq(obj2, field),
                //Trigger(Seq(lookup(h.l, obj2.l, field.l))) ++
                Trigger(Seq(lookup(eh.l, obj2.l, field.l))),
                (lookup(lookup(h.l, nullLit, predicateMaskField(predField.l)), obj2.l, field.l, true) ==>
                  (lookup(h.l, obj2.l, field.l) === lookup(eh.l, obj2.l, field.l))),
                field.typ.freeTypeVars
              )
            )
      )), size = 1)  ++ {
      // frame all wand masks
      if(!liberal) {
        MaybeCommentedDecl("Frame all wand mask locations of wands with direct permission", Axiom(Forall(
          vars ++ Seq(predField),
          Trigger(Seq(identicalFuncApp, isWandField(predField.l), lookup(eh.l, nullLit, wandMaskField(predField.l)))),
          identicalFuncApp ==>
            ((staticPermissionPositive(nullLit, predField.l) && isWandField(predField.l)) ==>
              (lookup(h.l, nullLit, wandMaskField(predField.l)) === lookup(eh.l, nullLit, wandMaskField(predField.l))))
        )), size = 1)
      } else {
        MaybeCommentedDecl("Frame all wand mask locations of wands with direct permission (but don't propagate information" +
          " about locations that are not known-folded)",
          Axiom(Forall( vars ++ Seq(predField),
          Trigger(Seq(identicalFuncApp, isWandField(predField.l), lookup(eh.l, nullLit, wandMaskField(predField.l)))),
          identicalFuncApp ==>
            ((staticPermissionPositive(nullLit, predField.l) && isWandField(predField.l)) ==>
              Forall(Seq(obj2, field),
                Trigger(Seq(lookup(lookup(eh.l, nullLit, wandMaskField(predField.l)), obj2.l, field.l, true))),
                (lookup(lookup(h.l, nullLit, wandMaskField(predField.l)), obj2.l, field.l, true) ==>
                  lookup(lookup(eh.l, nullLit, wandMaskField(predField.l)), obj2.l, field.l, true)
                  ),
                field.typ.freeTypeVars
              )
              )
        )), size = 1)
      }
    } ++
      MaybeCommentedDecl("Frame all locations in the footprint of magic wands", Axiom(Forall(
        vars ++ Seq(predField),
        Trigger(Seq(identicalFuncApp, isWandField(predField.l)))
        ,
        identicalFuncApp ==>
          (
            (staticPermissionPositive(nullLit, predField.l) && isWandField(predField.l)) ==>
              Forall(Seq(obj2, field),
                Trigger(Seq(lookup(eh.l, obj2.l, field.l))),
                (lookup(lookup(h.l, nullLit, wandMaskField(predField.l)), obj2.l, field.l, true) ==>
                  (lookup(h.l, obj2.l, field.l) === lookup(eh.l, obj2.l, field.l))),
                field.typ.freeTypeVars
              )
            )
      )), size = 1) ++
      (if(enableAllocationEncoding) // preserve "allocated" knowledge, where already true
        MaybeCommentedDecl("All previously-allocated references are still allocated", Axiom(Forall(
          vars ++ Seq(obj),
          /*Trigger(Seq(identicalFuncApp, lookup(h.l, obj.l, Const(allocName)))) ++*/
          Trigger(Seq(identicalFuncApp, lookup(eh.l, obj.l, Const(allocName)))),
          identicalFuncApp ==>
            (lookup(h.l, obj.l, Const(allocName)) ==> lookup(eh.l, obj.l, Const(allocName)))
        )), size = 1) else Nil)
  }

  override def sumHeap(resultHeap: Exp, heap1: Exp, mask1: Exp, heap2: Exp, mask2: Exp): Exp = {
    FuncApp(sumHeapName, Seq(resultHeap, heap1, mask1, heap2, mask2), Bool)
  }
*/

  override def heapTypes: Seq[NamedType] = allFieldsTypVars map {case ftVars => heapTyp(ftVars)}

/*
  override def successorHeapState(first: Seq[LocalVarDecl], second: Seq[LocalVarDecl]): Exp = {
    FuncApp(succHeapTransName, (first ++ second) map (_.l), Bool)
  }
*/

/*
  override def isPredicateField(f: Expr): Expr = {
    FunctionCallExpr(isPredicateFieldName, Seq(f), Bool)
  }

  override def isWandField(f: Expr): Expr = {
    FunctionCallExpr(isWandFieldName, Seq(f), Bool)
  }

  // returns predicate Id
  override def getPredicateOrWandId(f:Exp): Exp = {
    FuncApp(getPredicateOrWandIdName,Seq(f), Int)
  }

  override def getPredicateOrWandId(s:String): BigInt = {
    if (!PredIdMap.contains(s)) {
      val predId:BigInt = getNewPredId;
      PredIdMap += (s -> predId)
    }
    PredIdMap(s)
  }

  def getNewPredId : BigInt = {
    val id = NextPredicateId
    NextPredicateId = NextPredicateId + 1
    id
  }
*/

  override def translateField(f: sil.Field) = {
    val field = locationIdentifier(f)
    val typ = translateType(f.typ)
    val funcTyp = NamedType(fieldTypeName, Seq(normalFieldType, typ))
    ConstDecl(field, funcTyp, Some(fieldTagName(funcTyp.typVars))) 
/* B3 LATER (predicates) ++
      Axiom(isPredicateField(FunctionCallExpr(field, Seq(), funcTyp)).not) 
*/
/* B3 ADVANCED (wand) ++
      Axiom(isWandField(FunctionCallExpr(field, Seq(), funcTyp)).not)
*/
  }

  override def resetFields(program: sil.Program, config: CarbonConfig): Unit = {
    // Initialize temp collection
    val allFieldTypeVarsSet = if (config == null || !config.disableAllocEncoding.isSupplied) {
      collection.mutable.Set[Seq[Type]](allocType.typVars)
    } else { collection.mutable.Set.empty[Seq[Type]] }

    // Helpers
    def registerFieldType(fieldTypeVars: Seq[Type]) = {
      if (fieldTypeVars.size != 2) sys.error("Field type should have 2 typVars!")
      allFieldTypeVarsSet += fieldTypeVars
    }
    def fieldFromField(f: sil.Field) = {
      registerFieldType(Seq(normalFieldType, translateType(f.typ)))
    }


    // collect field types
    program match {
      case sil.Program(domains, fields, functions, predicates, methods, extensions) =>
        fields map {fieldFromField(_)}
        // B3 LATER/ADVANCED: collect other field types! (predicates) (wand)
    }

    // Update field-type collection for current Program
    allFieldsTypVarsSeq = if (allFieldTypeVarsSet.size == 0) noFieldReplacement
      else allFieldTypeVarsSet.toSeq 

    // Update field -> field-collection-index map
    fieldIdxMap = allFieldsTypVars.zipWithIndex.toMap

    // Update other variables dependent on the field-type collection and fieldIdxMap
    originalHeaps = constructOriginalHeap
    heaps = originalHeaps
    permModule.reset()

    // B3 TODO: find less ugly way to do this whole "nicer-name thing".
    heapTypVarsToIdx = fieldIdxMap
    specialFunctionReadHeapName = readHeapName
    specialFunctionUpdateHeapName = updateHeapName
  }

/*
// AS: Seems that many concerns here would be better addressed in / delegated to the FuncPredModule
  override def predicateGhostFieldDecl(p: sil.Predicate): Seq[Decl] = {
    val predicate = locationIdentifier(p)
    val pmField = predicateMaskIdentifer(p)
    val t = predicateVersionFieldTypeOf(p)
    val pmT = predicateMaskFieldTypeOf(p)
    val varDecls = p.formalArgs map mainModule.translateLocalVarDecl
    val vars = varDecls map (_.l)
    val predId:BigInt = getPredicateOrWandId(p.name)
    val f0 = FuncApp(predicate, vars, t)
    val f1 = predicateMaskField(f0)
    val f2 = FuncApp(pmField, vars, pmT)
    TypeDecl(predicateMetaTypeOf(p)) ++
      Func(predicate, varDecls, t) ++
      Func(pmField, varDecls, pmT) ++
      Axiom(MaybeForall(varDecls, Trigger(f1), f1 === f2)) ++
      Axiom(MaybeForall(varDecls, Trigger(f0), isPredicateField(f0))) ++
      Axiom(MaybeForall(varDecls, Trigger(f0), getPredicateOrWandId(f0) === IntLit(predId))) ++
      Func(predicateTriggerIdentifier(p), Seq(LocalVarDecl(heapName, heapTyp), LocalVarDecl(Identifier("pred"), predicateVersionFieldType())), Bool) ++
      Func(predicateTriggerAnyStateIdentifier(p), Seq(LocalVarDecl(Identifier("pred"), predicateVersionFieldType())), Bool) ++
      {
        // axiom that two predicate identifiers can only be the same, if all arguments
        // are the same (e.g., we immediatly know that valid(1) != valid(2))
        if (vars.size == 0) Nil
        else {
          val varDecls2 = varDecls map (
            v => LocalVarDecl(Identifier(v.name.name + "2")(v.name.namespace), v.typ))
          val vars2 = varDecls2 map (_.l)
          var varsEqual = All((vars zip vars2) map {
            case (v1, v2) => v1 === v2
          })
          val f0_2 = FuncApp(predicate, vars2, t)
          val f2_2 = FuncApp(pmField, vars2, t)
          Axiom(Forall(varDecls ++ varDecls2, Trigger(Seq(f0, f0_2)),
            (f0 === f0_2) ==> varsEqual)) ++
            Axiom(Forall(varDecls ++ varDecls2, Trigger(Seq(f2, f2_2)),
              (f2 === f2_2) ==> varsEqual))
        }
      }
  }
*/

  /** Return the identifier corresponding to a Viper location. */
  private def locationIdentifier(f: sil.Location): Identifier = {
    Identifier(f.name)(fieldNamespace)
  }

/*
  private def predicateMaskIdentifer(f: sil.Location): Identifier = {
    Identifier(f.name + "#sm")(fieldNamespace)
  }

  def wandMaskIdentifier(f: Identifier) = {
    Identifier(f.name + "#sm")(fieldNamespace)
  }

  def wandFtIdentifier(f: Identifier) = {
    Identifier(f.name + "#ft")(fieldNamespace)
  }


  /**
    * @param maskField the field with which the predicate mask is accessed in the heap
    * @param mask the predicate mask itself (for example, Heap[null, [[maskField]]])
    */
  case class PredicateMask(maskField: Exp, mask: Exp)

  private def predicateMask(loc: sil.PredicateAccess) : PredicateMask = {
    predicateMask(loc, heapExp)
  }

  private def predicateMask(loc: sil.PredicateAccess, heap: Exp) : PredicateMask = {
    val predicate = verifier.program.findPredicate(loc.predicateName)
    val t = predicateMaskFieldTypeOf(predicate)
    val pmaskFieldRep = FuncApp(predicateMaskIdentifer(predicate), loc.args map translateExp, t)
    PredicateMask(pmaskFieldRep, lookup(heap, nullLit, pmaskFieldRep))
  }

  private def curHeapAssignUpdatePredWandMask(maskField: Exp, newMask: Exp) = {
    heap := heapUpdate(heap, nullLit, maskField, newMask)
  }

  private def wandMask(wandMaskRep: Exp) = {
    lookup(heapExp, nullLit, wandMaskRep)
  }

  private def predicateTriggerIdentifier(f: sil.Predicate): Identifier = {
    Identifier(f.name + "#trigger")(fieldNamespace)
  }
  private def predicateTriggerAnyStateIdentifier(f: sil.Predicate): Identifier = {
    Identifier(f.name + "#everUsed")(fieldNamespace)
  }
  private def predicateTrigger(extras : Seq[Exp], predicate: sil.Predicate, predicateField: Exp): Exp = {
    FuncApp(predicateTriggerIdentifier(predicate), extras ++ Seq(predicateField), Bool)
  }
  private def predicateTriggerAnyState( predicate: sil.Predicate, predicateField: Exp): Exp = {
    FuncApp(predicateTriggerAnyStateIdentifier(predicate), Seq(predicateField), Bool)
  }
  override def predicateTrigger(extras : Seq[Exp], pred: sil.PredicateAccess, anyState : Boolean = false): Exp = {
    val predicate = verifier.program.findPredicate(pred.predicateName)
    val location = translateResource(pred)
    if (anyState) predicateTriggerAnyState(predicate, location) else predicateTrigger(extras, predicate, location)
  }
*/

  /** 
   * Returns a heap-lookup of the allocated field of an object. 
   * (should only be used for known-non-null references) 
   */
  private def alloc(o: Expr) = lookup(heapExp(allocType.typVars), o, Const(allocName, allocType)) // B3 NOTE: see note above val allocName

  /** Returns assignment that updates heap to reflect that @{code ref} is assigned  */
  private def allocUpdateRef(ref: Expr): Stmt = currentHeapAssignUpdate(ref, Const(allocName, allocType), isAlloc)

  /** 
   * Returns a heap-lookup for o.f in a given heap h.
   * 
   * @param h Must have a concrete NamedType of form "HeapType A B"
   * @param o Must have Type "Ref"
   * @param f Must have a concrete NamedType of form "Field A B"
   * @param isPMask returns PMask-lookup instead
   * 
   * (A and B must match for all parameters)
   */
  private def lookup(h: Expr, o: Expr, f: Expr, isPMask: Boolean = false): Expr = {
    // B3 INFO: removed usePolyMapsInEncoding version.
    if (isPMask) {
      FunctionCallExpr(permModule.pmaskTypeDesugared.selectId, Seq(h,o,f), Bool)
    } else {
      FunctionCallExpr(readHeapName, Seq(h,o,f), heapMapRangeTypeFromField(f.typ))
    }
  }

  def rcvAndFieldExp(f: sil.ResourceAccess) : (Expr, Expr) =
    f match {
      case sil.FieldAccess(rcv, _) => (translateExp(rcv), translateResource(f))
      case sil.PredicateAccess(_, _) => (nullLit, translateResource(f))
      case w: sil.MagicWand => (nullLit, translateResource(f))
    }

  override def currentHeapAssignUpdate(f: sil.LocationAccess, newVal: Expr): Stmt = {
    val (rcvExp, fieldExp) = rcvAndFieldExp(f)
    currentHeapAssignUpdate(rcvExp, fieldExp, newVal)
  }

  private def currentHeapAssignUpdate(rcv: Expr, field: Expr, newVal: Expr): Stmt = {
    val splitVariant = field.typ.asInstanceOf[NamedType].typVars
    heap(splitVariant) := heapUpdate(heap(splitVariant), rcv, field, newVal)
  }

/*
  private def heapUpdateLoc(heap: Exp, f: sil.LocationAccess, newVal: Exp, isPMask: Boolean = false): Exp = {
    val (rcvExp, fieldExp) = rcvAndFieldExp(f)
    heapUpdate(heap, rcvExp, fieldExp, newVal, isPMask)
  }
*/

  /** 
   * Returns a heap-update for o.f to v in a given heap h.
   * 
   * @param h Must have a concrete NamedType of form "HeapType A B"
   * @param o Must have Type "Ref"
   * @param f Must have a concrete NamedType of form "Field A B"
   * @param v Must have a concrete Type "B"
   * @param isPMask (optional) returns PMask-lookup instead
   * 
   * (A and B must match for all parameters)
   */
  private def heapUpdate(h: Expr, o: Expr, f: Expr, v: Expr, isPMask: Boolean = false): Expr = {
    FunctionCallExpr(if(isPMask) permModule.pmaskTypeDesugared.storeId else updateHeapName, Seq(h, o, f, v), h.typ)
  }

  override def translateResourceAccess(f: sil.ResourceAccess): Expr = {
    val heapTypVars = f match {
      case sil.FieldAccess(_, field) => Seq(normalFieldType, translateType(field.typ))
      case sil.PredicateAccess(_, _) => sys.error("B3 LATER (predicates): need to implement translation for resource-type sil.PredicateAccess")
      case w: sil.MagicWand => sys.error("B3 ADVANCED (wand): need to implement translation for resource-type sil.MagicWand")
    } 
    val fieldExp = translateResource(f)
    translateResourceAccess(f, heapExp(heapTypVars))
  }

  private def translateResourceAccess(f: sil.ResourceAccess, heap: Expr, isPMask: Boolean = false): Expr = {
    val (rcvExp, fieldExp) = rcvAndFieldExp(f)
    lookup(heap, rcvExp, fieldExp, isPMask)
  }

/*
  override def translateLocationAccess(rcv: Exp, loc:Exp):Exp = {
    //FIXME: should the first argument be @{code heapExp}?
    lookup(heap, rcv, loc)
  }
*/

  override def translateResource(l: sil.ResourceAccess): Expr = {
    l match {
      case sil.PredicateAccess(args, predName) =>
        LATER_Expr_bool("predicates", "DHeapM->translateResource")
/*
        val pred = verifier.program.findPredicate(predName)
        val t = predicateMetaTypeOf(pred)
        FuncApp(locationIdentifier(pred), args map translateExp, t)
*/
      case sil.FieldAccess(rcv, field) =>
        Const(locationIdentifier(field), fieldTypeOf(translateType(field.typ)))
      case w: sil.MagicWand =>
        ADVANCED_Expr_bool("wand", "DHeapM->translateResource->sil.MagicWand")
/*
        wandModule.getWandRepresentation(w)
*/
    }
  }

/*
  override def translateLocation(pred: sil.Predicate, args: Seq[Exp]): Exp = {
    val t = predicateMetaTypeOf(pred)
    FuncApp(locationIdentifier(pred), args, t)
  }
*/

  override def handleStmt(s: sil.Stmt, statesStack: List[Any] = null, allStateAssms: Expr = TrueLit(), insidePackageStmt: Boolean = false) : (Block => Block) = {

      stmt => (
        s match {
          case sil.MethodCall(_, _, targets) if enableAllocationEncoding =>
            stmt ++ (targets filter (_.typ == sil.Ref) map translateExp map {
              t =>
                Assume(validReference(t))
            })
          case sil.Fold(sil.PredicateAccessPredicate(loc, perm)) => // AS: this should really be taken care of in the FuncPredModule (and factored out to share code with unfolding case, if possible)
            LATER_Stmt("predicates", "DHeapM->handleStmt->sil.Fold-case") +++ stmt
/* 
            if(usingOldState) sys.error("heap module: fold is executed while using old state")
            stmt ++ ({val newVersion = LocalVar(Identifier("freshVersion"), funcPredModule.predicateVersionType)
              val resetPredicateInfo : Stmt =
                curHeapAssignUpdatePredWandMask(predicateMask(loc).maskField, zeroPMask) ++
                Havoc(newVersion) ++
                currentHeapAssignUpdate(loc, newVersion)

              If(UnExp(Not,hasDirectPerm(loc)), resetPredicateInfo, Nil) ++
                addPermissionToPMask(loc) ++ stateModule.assumeGoodState}  )
*/
          case sil.FieldAssign(lhs, rhs) =>
            if(usingOldState) sys.error("heap module: field is assigned while using old state")
            stmt ++ (currentHeapAssignUpdate(lhs, translateExp(rhs))) // after all checks
          case _ => simpleHandleStmt(s) ++ stmt
        }
      )

  }

  override def simpleHandleStmt(stmt: sil.Stmt, statesStack: List[Any] = null, allStateAssms: Expr = TrueLit(), insidePackageStmt: Boolean = false): Stmt = {
    stmt match {
      case sil.NewStmt(target,fields) =>
        Reinit(freshObjectVar) ::
          // assume the fresh object is non-null and not allocated yet.
          // this means that whenever we allocate a new object and havoc freshObjectVar, we
          // assume that we consider a newly allocated cell, which gives the prover
          // the information that this object is different from anything allocated
          // earlier. Note that "validReference" must be used in appropriate places
          // in the encoding to get this fact (e.g. below for method targets, and also
          // for loops (see the StateModule implementation)
          Assume(if(enableAllocationEncoding) (freshObjectVar !== nullLit) && validReference(freshObjectVar).not else (freshObjectVar !== nullLit)) ::
          (if(enableAllocationEncoding) allocUpdateRef(freshObjectVar) :: (translateExp(target) := freshObjectVar) :: Nil else (translateExp(target) := freshObjectVar) :: Nil)
      case _ => EmptyStmt
    }
  }

  override def freeAssumptions(e: sil.Exp): Stmt = {
    e match {
      case sil.Unfolding(sil.PredicateAccessPredicate(loc, _), _) if !usingOldState =>
        LATER_Stmt("predicates", "DefaultHeapModule->freeAssumptions")
/*
        addPermissionToPMask(loc) ++ assumeGoodState
*/
      case _ => Nil
    }
  }

/*
  override def addPermissionToWMask(wMaskField: Exp, e: sil.Exp): Stmt = {
    if(usingOldState) { sys.error("Updating wand mask while using old state") }
    e match {
      case sil.FieldAccessPredicate(loc, perm) =>
        curHeapAssignUpdatePredWandMask(wMaskField, heapUpdateLoc(wandMask(wMaskField), loc, TrueLit(), true))
      case sil.PredicateAccessPredicate(loc, perm) =>
        val newPMask = LocalVar(Identifier("newPMask"), pmaskType)
        val obj = LocalVarDecl(Identifier("o")(axiomNamespace), refType)
        val field = LocalVarDecl(Identifier("f")(axiomNamespace), fieldType)
        val pm1 = lookup(wandMask(wMaskField), obj.l, field.l, true)
        val pm2 = lookup(predicateMask(loc).mask, obj.l, field.l, true)
        val pm3 = lookup(newPMask, obj.l, field.l, true)
        Havoc(newPMask) ++
          Assume(Forall(Seq(obj, field), Seq(Trigger(pm3)), (pm1 || pm2) ==> pm3)) ++
          curHeapAssignUpdatePredWandMask(wMaskField, newPMask)

      case _ =>
        Statements.EmptyStmt
    }
  }
  /**
   * Adds the permissions from the body of a predicate to its permission mask.
   */
  private def addPermissionToPMask(loc: sil.PredicateAccess): Stmt = {
    val predBody = loc.predicateBody(verifier.program, env.allDefinedNames(program)).get
    addPermissionToPMaskHelper(predBody, loc, predicateMask(loc,heap))
  }
  /**
   * Adds the permissions from an expression to a permission mask.
   */
  private def addPermissionToPMaskHelper(e: sil.Exp, loc: sil.PredicateAccess, pmask: PredicateMask): Stmt = {
    if(usingOldState) { sys.error("Updating wand mask while using old state") }
    e match {
      case QuantifiedPermissionAssertion(forall, cond, acc: sil.FieldAccessPredicate) =>
        val vs = forall.variables // TODO: Generalise to multiple quantified variables
        val fieldAccess = acc.loc

        // alpha renaming, to avoid clashes in context
        val vsFresh = vs.map(v => env.makeUniquelyNamed(v))
        vsFresh.foreach(vFresh => env.define(vFresh.localVar))

        def renaming[E <: sil.Exp] = (e:E) => Expressions.instantiateVariables(e, vs.map(_.localVar),  vsFresh.map(_.localVar))

        val (renamingCond,renamingFieldAccess) = (renaming(cond),renaming(fieldAccess))
        val translatedCond = translateExp(renamingCond)

        val newPMask = LocalVar(Identifier("newPMask"), pmaskType)
        val obj = LocalVarDecl(Identifier("o")(axiomNamespace), refType)
        val field = LocalVarDecl(Identifier("f")(axiomNamespace), fieldType)
        val pm1 = lookup(pmask.mask, obj.l, field.l, true)
        val pm2 = lookup(newPMask, obj.l, field.l, true)
        val res =
          MaybeComment("register all known folded permissions guarded by predicate " + loc.predicateName,
            Havoc(newPMask) ++
              Assume(Forall(Seq(obj, field), Seq(Trigger(pm2)), (pm1 ==> pm2))) ++
                Assume(Forall(vsFresh.map(vFresh => translateLocalVarDecl(vFresh)),Seq(),translatedCond ==> (translateResourceAccess(renamingFieldAccess, newPMask, true) === TrueLit()) ))) ++
            curHeapAssignUpdatePredWandMask(pmask.maskField, newPMask)
        vsFresh.foreach(vFresh => env.undefine(vFresh.localVar))
        res
      case sil.FieldAccessPredicate(loc, perm) =>
        curHeapAssignUpdatePredWandMask(pmask.maskField, heapUpdateLoc(pmask.mask, loc, TrueLit(), true))
      case sil.PredicateAccessPredicate(loc, perm) =>
        val newPMask = LocalVar(Identifier("newPMask"), pmaskType)
        val obj = LocalVarDecl(Identifier("o")(axiomNamespace), refType)
        val field = LocalVarDecl(Identifier("f")(axiomNamespace), fieldType)
        val pm1 = lookup(pmask.mask, obj.l, field.l, true)
        val pm2 = lookup(predicateMask(loc).mask, obj.l, field.l, true)
        val pm3 = lookup(newPMask, obj.l, field.l, true)
        Havoc(newPMask) ++
          Assume(Forall(Seq(obj, field), Seq(Trigger(pm3)), (pm1 || pm2) ==> pm3)) ++
          curHeapAssignUpdatePredWandMask(pmask.maskField, newPMask)
      case sil.And(e1, e2) =>
        addPermissionToPMaskHelper(e1, loc, pmask) ::
          addPermissionToPMaskHelper(e2, loc, pmask) ::
          Nil
      case sil.Implies(e1, e2) =>
        If(translateExp(e1), addPermissionToPMaskHelper(e2, loc, pmask), Statements.EmptyStmt)
      case sil.CondExp(c, e1, e2) =>
        If(translateExp(c), addPermissionToPMaskHelper(e1, loc, pmask), addPermissionToPMaskHelper(e2, loc, pmask))
      case _ => Nil
    }
  }
*/

  override def validValue(typ: sil.Type, variable: IdExpr, isParameter: Boolean): Option[Expr] = {
    if(enableAllocationEncoding) typ match {
      case sil.Ref => Some(validReference(variable))
      case _ => None
    } else None
  }

  private def validReference(exp: Expr): Expr = {
    /*exp === nullLit ||*/ alloc(exp) === isAlloc // B3 NOTE: see note above val allocName
  }

  override def translateNull: Expr = nullLit

  def initBoogieState: Seq[Stmt] = {
    heaps = originalHeaps
    Nil
  }
  def resetBoogieState: Seq[Stmt] = {
    allFieldsTypVars map {ftvars => Reinit(heapVar(ftvars))}
  }

  def staticStateContributions(ftvars: Seq[Type], withHeap: Boolean, withPermissions: Boolean): Seq[FParameter] = if(withHeap) { FParameter(heapName(ftvars), heapTyp(ftvars)) } else Seq()
/*
  def currentStateContributions: Seq[LocalVarDecl] = Seq(LocalVarDecl(heap.name, heapTyp))
*/
  def currentStateVars: Seq[IdExpr] = heaps
  def currentStateExps: Seq[Expr] = allFieldsTypVars map {heapExp(_)}


  override def freshTempState(name: String): Seq[IdExpr] = {
    heapTypes map {htyp => IdExpr(Identifier(s"${name}"+heapName(htyp.typVars)), htyp)}
  }

  override def restoreState(s: Seq[IdExpr]): Unit = {
    heaps = s // note: this should be accessed via heapVar or heapExp as appropriate (whether a variable is essential or not)
  }

/*
  def equateWithCurrentHeap(s: Seq[IdExpr]): Stmt = {
    Assume(heap === s(0))
  }
*/

  override def usingOldState = stateModuleIsUsingOldState

  override def usingPureState = stateModuleIsUsingPureState

  override def beginExhale: Stmt = {
//    Havoc(exhaleHeap) //<--(non-B3 comment)
    EmptyStmt
  }

  override def endExhale: Stmt = {
    if (!usingOldState) { 
      allFieldsTypVars flatMap { ftvars =>
        Reinit(exhaleHeap(ftvars)) ++
          LATER_Stmt("predicates", "DHeapM->endExhale (identicalOnKnownLocsName-part)") ++ // Assume(FunctionCallExpr(identicalOnKnownLocsName, Seq(heapExp(ftvars), exhaleHeap(ftvars)) ++ currentMask(fieldIdx(ftvars)), Bool)) ++
          (heapVar(ftvars) := exhaleHeap(ftvars))
      }
    } else Nil
  }

  /**
   * Reset the state of this module so that it can be used for new program. This method is called
   * after verifier gets a new program.
   */
  override def reset() = {
    addLATER("predicates", "DHeapM->reset")
/*
    PredIdMap = Map()
    NextPredicateId = 0
*/
    heaps = originalHeaps
  }

  override def currentHeap = heaps

/*
  override def identicalOnKnownLocations(otherHeap:Seq[Exp],otherMask:Seq[Exp]):Exp =
    FuncApp(identicalOnKnownLocsName,otherHeap ++ heap ++ otherMask, Bool)
*/
}
