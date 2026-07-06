// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2021 ETH Zurich.

package viper.carbon.modules.impls

import viper.carbon.modules.{StatelessComponent, TypeModule}
import viper.silver.{ast => sil}
import viper.carbon.b3.B3Nodes._
import viper.carbon.verifier.Verifier

/**
 * The default implementation of a [[viper.carbon.modules.TypeModule]].
 */
class DefaultTypeModule(val verifier: Verifier) extends TypeModule with StatelessComponent {

  import verifier._
  import heapModule._
  import domainModule._
  import permModule._
  import seqModule._
  import setModule._
  import mapModule._

  def name = "Type module"
  override def translateType(t: sil.Type): Type = {
    t match {
      case sil.Bool =>
        Bool
      case sil.Int =>
        Int
      case sil.Ref =>
        refType // B3 TODO: was DefaultHeapModule -> refType. Check if there can be a conflict (i.e. can "Ref" also be a custom type from Viper?)
      case sil.Perm =>
        permType // B3 TODO: replace with real as soon as real is supported
      case t: sil.SeqType =>
        sys.error("B3 TODO: SeqType not supported yet.") //translateSeqType(t)
      case t: sil.SetType =>
        sys.error("B3 TODO: SetType not supported yet.") //translateSetType(t)
      case t: sil.MultisetType =>
        sys.error("B3 TODO: MultisetType not supported yet.") //translateMultisetType(t)
      case t: sil.MapType =>
        sys.error("B3 TODO: MapType not supported yet.") //translateMapType(t)
      case sil.InternalType =>
        sys.error("This is an internal type, not expected here")
      case sil.TypeVar(name) =>
        TypeVar(name)
      case t@sil.DomainType(_, _) =>
        sys.error("B3 TODO: DomainType not supported yet.") //translateDomainTyp(t)
      case sil.BackendType(_, interpretations) if interpretations.contains("Boogie") => sys.error("B3 TODO: BackendType with 'Boogie' not supported yet.") //NamedType(interpretations("Boogie"))
      case sil.BackendType(_, _) => sys.error("Found non-Boogie-compatible backend type.")
      case _ => sys.error("Viper type didn't match any existing case.")
    }
  }
}
