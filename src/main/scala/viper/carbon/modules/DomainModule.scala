// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2021 ETH Zurich.

package viper.carbon.modules

import viper.silver.{ast => sil}
import viper.carbon.b3.B3Nodes.{Type, Decl, Expr}

/**
 * A module for translating Viper domains.

 */
trait DomainModule extends Module {
  def translateDomain(exp: sil.Domain): Seq[Decl]
  def translateDomainFuncApp(fa: sil.DomainFuncApp): Expr
  def translateDomainTyp(typ: sil.DomainType): Type
}
