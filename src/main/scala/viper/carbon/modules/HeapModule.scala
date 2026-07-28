// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2021 ETH Zurich.

package viper.carbon.modules

import viper.silver.{ast => sil}
import viper.carbon.b3.B3Nodes._
import viper.carbon.modules.components.CarbonStateComponent
import viper.carbon.utility.PolyMapDesugarHelper
import viper.silver.ast.{LocationAccess, MagicWand}
import viper.carbon.CarbonConfig

/**
 * A module for translating heap expressions (access, updating) and determining
 * the heap encoding.
 */
trait HeapModule extends Module with CarbonStateComponent {

  /**
    * The types used for all heap-splits.
    */
  def heapTypes: Seq[NamedType]

  /**
   * The type used for references.
   */
  def refType: Type

  /**
   * The types used for fields.
   */
  def fieldTypes: Seq[Type]

  /**
   * Creates a Field type corresponding to the given (Field-)typeVars
   */
  def fieldType(ftVars: Seq[Type]): NamedType

  /**
   * The type used for fields of type t.
   */
  def fieldTypeOf(t: Type): Type

  /**
   * Modifies 'baseName' and returns new, unique name that matches the given Field variant
   */
  def addFieldMark(baseName: String, ftVars: Seq[Type]): String


  /** 
   * Returns all unique typVars-pairs of the field type.
   * 
   * (If no field types exist this returns a fake typVars-pair. This ensures that at least one heap-splits 
   * is created. Without any heap-splits the heap would not exist, which would cause an errors. Since we are 
   * apparently never interacting with the heap, we dont care that this is a heap-split with a fake type.)
   */
  def allFieldsTypVars: Seq[Seq[Type]]

  /**
   * Collects all Field types and updates the corresponding collection.  
   */
  def resetFields(program: sil.Program, config: CarbonConfig): Unit

  /**
   * Returns the Field index of the given Field type variables. (i.e. the index in allFieldsTypVars) 
   */
  def fieldIdx(ftVars: Seq[Type]): Int

  def forallFields(formula: Seq[Type] => Any): Seq[Any]

  /**
    * Represents the B3 type constructor for fields.
    * The first element specifies how many type arguments (n_ty_args) the field type constructor takes and
    * the second element provides a function to construct a field type given n_ty_args many type arguments
    */
  def fieldTypeConstructor: Seq[Type] => NamedType

  /**
    * A function that, given a Field Type, returns the range type that the matching desugared map has.
    */
  def heapMapRangeTypeFromField: Type => Type

  /** 
   * Returns all unique typVars-pairs of the field type.
   * (If no field types exist this returns a fake typVars-pair. This ensures that at least one heap-splits 
   * is created. Without any heap-splits the heap would not exist, which would cause an errors. Since we are 
   * apparently never interacting with the heap, we dont care that this is a heap-split with a fake type.)
   */

/*
  /**
   * The type used for predicates.
   */
  def predicateVersionFieldType(genericT: String = "A"): Type

  /**
   * The type used for predicates mask fields.
   */
  def predicateMaskFieldType: Type

  /**
   * The type used for predicates mask fields of a given predicate family.
   */
  def predicateMaskFieldTypeOf(p: sil.Predicate): Type

  /**
   * The type used for predicates of a given family.
   */
  def predicateVersionFieldTypeOf(p: sil.Predicate): Type

  /**
   * Get a function application representing that one heap-state (as represented by currentStateContributions of HeapModule) is a predecessor of another
   */ //B3 LATER: check if we can use a sub-class of LocalVarDecl isntead 
  def successorHeapState(first: Seq[LocalVarDecl], second: Seq[LocalVarDecl]) : Expr

  /**
   * The type used for wands.
   */
  def wandFieldType(wandName: String): Type

  /**
   * new type introduced for a wand
   */
  def wandBasicType(wandName: String): Type
*/

  /**
   * Definitions for a field. (B3: This is also where all field types are collected. 
   * Must be called before any of the other translate functions)
   */
  def translateField(f: sil.Field): Seq[Decl]

/*
  /**
   * Definitions for the ghost field of a predicate.
   */
  def predicateGhostFieldDecl(f: sil.Predicate): Seq[Decl]
*/

  /**
   * Translation of a field read, predicate instance, or wand instance.
   */
  def translateResourceAccess(f: sil.ResourceAccess): Expr
/*
  /**
    * Translation of a field read.
    */
  def translateLocationAccess(rcv: Expr, loc: Expr): Expr
*/

  def translateResource(f: sil.ResourceAccess): Expr
/*
  def translateLocation(pred: sil.Predicate, args: Seq[Expr]): Expr
*/

  /**
   * Translation of the null literal.
   */
  def translateNull: Expr

  /**
   * Check that the receiver of a location access is non-null.
   */
  def checkNonNullReceiver(loc: sil.LocationAccess): Expr = {
    loc match {
      case sil.FieldAccess(rcv, _) =>
        verifier.expModule.translateExp(rcv) !== translateNull
      case _ => TrueLit()
    }
  }

/*
  def checkNonNullReceiver(rcv: Expr): Expr = {
    rcv !== translateNull
  }
*/

  /**
   * Begin of exhale.
   */
  def beginExhale: Stmt

  /**
   * End of exhale
   */
  def endExhale: Stmt

/*
  /**
   * Is the given field a predicate field?
   */
  def isPredicateField(f: Expr): Expr

  /**
    * get Predicate or wand Id (unique for each Predicate or wand)
    */
  def getPredicateOrWandId(f:Expr): Expr

  /**
    * Predicate or (internal) wand name mapping to Id
    */
  def getPredicateOrWandId(s:String):BigInt
  /**
   * Is the given field a wand field?
   */
  def isWandField(f: Expr): Expr

  def predicateTrigger(extras: Seq[Expr], pred: sil.PredicateAccess, anyState: Boolean = false): Expr
*/

  def currentHeap: Seq[Expr]

  /**
    * store {@code newVal} at {@code loc} in the current heap
    */
  def currentHeapAssignUpdate(loc: sil.LocationAccess, newVal: Expr): Stmt

/*
  def identicalOnKnownLocations(heap:Seq[Expr],mask:Seq[Expr]):Expr

  /**
    * Adds assumption that current heap equals heap represented by s
    */
  def equateWithCurrentHeap(s: Seq[IdExpr]): Stmt

  // returns wand#sm (secondary mask for the wand)
  def wandMaskIdentifier(f: Identifier): Identifier

  // returns wand#ft (footprint of the magic wand)
  // this is inhaled at the beginning of packaging a wand to frame fields while the wand being packaged (
  // as the permission to the wand is gained at the end of the package statement)
  def wandFtIdentifier(f: Identifier): Identifier

  def predicateMaskFieldTypeOfWand(wand: String): Type

  def predicateVersionFieldTypeOfWand(wand: String): Type

  // adds permission to field e to the secondary mask of the wand
  def addPermissionToWMask(wMask: Expr, e: sil.Exp): Stmt

  // If expression evaluates to true then resultHeap is the sum of of heap1, where mask1 is defined,
  // and heap2, where mask2 is defined.
  def sumHeap(resultHeap: Expr, heap1: Expr, mask1: Expr, heap2: Expr, mask2: Expr): Expr

*/
}
