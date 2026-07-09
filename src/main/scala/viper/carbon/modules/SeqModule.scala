// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2021 ETH Zurich.

package viper.carbon.modules

import viper.silver.{ast => sil}
import viper.carbon.b3.B3Nodes.{Expr, Type}

/**
 * A module for translating sequences.

 */
trait SeqModule extends Module {
  def translateSeqExp(exp: sil.Exp): Expr
/*
  def translateSeqType(seqType: sil.SeqType): Type
  def rewriteToTermsInTriggers(e: Expr) : Expr
*/
}
