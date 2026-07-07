// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2021 ETH Zurich.

package viper.carbon.modules

import viper.carbon.b3.B3Nodes.Expr
import viper.silver.ast.LocationAccess
import viper.silver.verifier.{PartialVerificationError, VerificationError, reasons}

import viper.silver.{ast => sil}

sealed trait TransferableEntity {
  def rcv: Expr
  def loc: Expr
  def transferAmount: Expr
  def originalSILExp: sil.Exp

  def transferError(error: PartialVerificationError):VerificationError
}

object TransferableEntity {
  def unapply(te: TransferableEntity) = Some((te.rcv,te.loc,te.transferAmount))
}

sealed trait TransferableAccessPred extends TransferableEntity {
  override def transferError(error: PartialVerificationError):VerificationError =
    originalSILExp.loc match {
      case loc: LocationAccess => error.dueTo(reasons.InsufficientPermission(loc))
      case other => sys.error(s"Unexpectedly found resource access node $other")
  }
  override def originalSILExp: sil.AccessPredicate
}

object TransferableAccessPred {
  def unapply(tap: TransferableAccessPred) = Some((tap.rcv,tap.loc,tap.transferAmount, tap.originalSILExp))
}

case class TransferableFieldAccessPred(rcv: Expr, loc: Expr, transferAmount: Expr,originalSILExp: sil.AccessPredicate) extends TransferableAccessPred

case class TransferablePredAccessPred(rcv: Expr, loc: Expr, transferAmount: Expr,originalSILExp: sil.AccessPredicate) extends TransferableAccessPred

case class TransferableWand(rcv: Expr, loc: Expr, transferAmount: Expr,originalSILExp: sil.MagicWand) extends TransferableEntity {
  override def transferError(error: PartialVerificationError): VerificationError = {
    error.dueTo(reasons.MagicWandChunkNotFound(originalSILExp))
  }
}
