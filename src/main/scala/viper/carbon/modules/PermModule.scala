// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2021 ETH Zurich.

package viper.carbon.modules

import viper.carbon.b3.B3Nodes.{Expr, Stmt, Type, Binding}
import viper.carbon.b3.B3Naming._
import viper.carbon.modules.components.CarbonStateComponent
import viper.silver.{ast => sil}

case class PMaskDesugaredRep(selectId: Identifier, storeId: Identifier)

/**
 * The permission module determines the encoding of permissions and allows to add or remove
 * permission.
 */
trait PermModule extends Module with CarbonStateComponent {

  /**
   * The type used to represent permissions.
   */
  def permType: Type

/*
  /**
   * Translate a permission amount
   */
  def translatePerm(e: sil.Exp): Expr

  /**
   * Translate a permission comparison
   */
  def translatePermComparison(e: sil.Exp): Expr
*/

  /**
   * Returns an expression representing that a permission amount is positive
   *
   * @param permission the permission amount to be checked
   * @param zeroOK whether the comparison should (not) be strict, or not
   * @return the expression representing the fact that the permission is positive
   */
  def permissionPositive(permission: Expr, zeroOK : Boolean = false): Expr

/*
  def conservativeIsPositivePerm(e: sil.Exp): Boolean

  /**
    * Returns an expression representing that a permission amount is positive.
    * Similar to [[permissionPositive]], but works directly on Viper expressions, *including* ones containing
    * wildcards, and performs more aggressive simplifications.
    *
    * @param e the permission amount to be checked
    * @return the expression representing the fact that the permission is positive
    */
  def isStrictlyPositivePerm(e: sil.Exp): Expr
*/

  /**
   * The current mask.
   */
  def currentMask: Seq[Expr]

  /**
   * A static reference to the mask.
   */
  def staticMask(ftvars: Seq[Type]): Binding

/*
  /**
   * Is the permission for a given expression positive (using the static mask).
   */
  def staticPermissionPositive(rcv: Expr, loc: Expr): Expr

  /**
   * The predicate mask field of a given predicate (as its ghost location).
   */
  def predicateMaskField(pred: Expr): Expr

  /**
    * The wand mask field of a given wand (as its ghost location).
    */
  def wandMaskField(wand: Expr): Expr
*/

  /**
    * The type used for mask-splits.
    */
  def maskTypes: Seq[Type]

  /**
   * The type used to for predicate masks.
   */
  def pmaskType(ftvars: Seq[Type]): Type

  /**
    * The desugared poly map version of [[pmaskType]].
    * TODO: It may make sense to move the representation of predicate masks to another module. Right now the representation
    *       seems to be shared among multiple modules.
    */
  def pmaskTypeDesugared: PMaskDesugaredRep

  def zeroPMask(ftvars: Seq[Type]): Expr

/*
  def hasDirectPerm(ra: sil.ResourceAccess): Expr

  /**
   * The expression for the current permission at a location.
   */
  def currentPermission(loc: sil.ResourceAccess): Expr

  def currentPermission(rcv:Expr, loc: Expr): Expr

  /**these methods are for experimental purposes, not yet finalized **/
  /*def beginSumMask : Stmt

  def sumMask : Expr

  def endSumMask: Stmt*/
/*
  def setSummandMask1
  def setSummandMask2
  def sumMask(assmsToSmt: Expr => Stmt):Stmt
  */

  /**
    *
    * @param summandMask1
    * @param summandMask2
    * @return expression for which its validity implies that the current mask is the sum of the two input masks
    */
  def sumMask(summandMask1: Seq[Expr], summandMask2: Seq[Expr]): Expr

  /**
    *
    * @param resultMask
    * @param summandMask1
    * @param summandMask2
    * @return expression for which its validity implies that @{code resultMask} is the sum of the other two input
    *         masks
    */
  def sumMask(resultMask: Seq[Expr], summandMask1: Seq[Expr], summandMask2: Seq[Expr]) : Expr

    /** returns a mask and the returned statement ensures that the mask  has non-zero permission at rcv.loc and zero
      * permission at all other location
      * this should only be used temporarily, i.e. if there are two calls to this then the previous tempMask returned
      * will be overwritten in the Boogie code
      */
  def tempInitMask(rcv: Expr, loc: Expr): (Seq[Expr], Stmt)

  def getCurrentAbstractReads(): collection.mutable.ListBuffer[String]

  /**
    * Checks if expression e contains instances of wildcards
    */

  def containsWildCard(e: sil.Exp): Boolean

  // adds permission to w#ft (footprint of the magic wand) (See Heap module for w#ft description)
  def inhaleWandFt(w: sil.MagicWand): Stmt

  // removes permission to w#ft (footprint of the magic wand) (See Heap module for w#ft description)
  def exhaleWandFt(w: sil.MagicWand): Stmt

  def setCheckReadPermissionOnly(readOnly: Boolean): Boolean

  def assumePermUpperBounds(doAssume: Boolean): Stmt
*/

}
