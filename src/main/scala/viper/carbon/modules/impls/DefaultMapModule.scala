// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2021 ETH Zurich.

package viper.carbon.modules.impls

import viper.carbon.boogie._
import viper.carbon.modules.MapModule
import viper.carbon.modules.components.{DefinednessComponent, DefinednessState}
import viper.carbon.modules.impls.map_axioms.MapAxiomatization
import viper.carbon.verifier.Verifier
import viper.silver.verifier.{PartialVerificationError, reasons}
import viper.silver.{ast => sil}
import viper.carbon.boogie.Implicits._


class DefaultMapModule(val verifier: Verifier) extends MapModule with DefinednessComponent {
  import verifier._
  import typeModule._
  import expModule._
  import DefaultMapModule._

  /** The name of this module. */
  override def name: String = "Map module"

  implicit val namespace = verifier.freshNamespace("map")

  /** Have maps been used so far (to determine if we need to include the set axiomatisation in the prelude). */
  private var used = false

  override def isUsed() : Boolean = used


  // val fieldNamespace = verifier.freshNamespace("seq.fields")
  def axiomNamespace = verifier.freshNamespace("map.axiom")


  // private val mapTypeName = "Map"
  // def mapType = NamedType(mapTypeName, Seq(TypeVar("U"), TypeVar("V")))
  // private val setTypeName = "Set"
  // def setUType = NamedType(setTypeName, Seq(TypeVar("U")))
  // def setVType = NamedType(setTypeName, Seq(TypeVar("U")))
  // def boogieUVMapType = MapType(TypeVar("U"), TypeVar("V"), Seq(TypeVar("U"), TypeVar("V")))
  // private val mapName = Identifier("m")
  // private val map0Name = Identifier("m0")
  // private val map1Name = Identifier("m1")
  // private val map2Name = Identifier("m2")
  // private val mapDomainName = Identifier("Map#Domain")
  // private val mapElementsName = Identifier("Map#Elements")
  // private val mapCardName = Identifier("Map#Card")
  // private val setCardName = Identifier("Set#Card")
  // private val mapValuesName = Identifier("Map#Values")
  // private val mapDisjointName = Identifier("Map#Disjoint")
  // private val mapEqualName = Identifier("Map#Equal")
  // private val mapEmptyName = Identifier("Map#Empty")
  // private val mapBuildName = Identifier("Map#Build")

  override def preamble : Seq[Decl] = {
    // val mapUV = LocalVarDecl(Identifier("m")(axiomNamespace), mapType)
    // val map2 = LocalVarDecl(Identifier("m2")(axiomNamespace), mapType)
    // val uU = LocalVarDecl(Identifier("u")(axiomNamespace), TypeVar("U"))
    // val uU2 = LocalVarDecl(Identifier("u2")(axiomNamespace), TypeVar("U"))
    // val vV = LocalVarDecl(Identifier("v")(axiomNamespace), TypeVar("V"))

    // def mapCardApp(param: Exp) = FuncApp(mapCardName, Seq(param), Int)
    // def setCardApp(param: Exp) = FuncApp(setCardName, Seq(param), Int)
    // def mapDomainApp(param: Exp) = FuncApp(mapDomainName, Seq(param), setUType)
    // def mapElementsApp(param: Exp) = FuncApp(mapElementsName, Seq(param), boogieUVMapType)
    // def mapValuesApp(param: Exp) = FuncApp(mapValuesName, Seq(param), setVType)
    
    // def mapDisjointApp(p1: Exp, p2: Exp) = FuncApp(mapDisjointName, Seq(p1, p2), Bool)
    // def mapEqualApp(p1: Exp, p2: Exp) = FuncApp(mapEqualName, Seq(p1, p2), Bool)
    // def mapEmptyApp = FuncApp(mapEmptyName, Seq(), mapType)
    // def mapBuildApp(p1: Exp, p2: Exp, p3: Exp) = FuncApp(mapBuildName, Seq(p1, p2, p3), mapType)

    // def mapDomain_m_u = MapSelect(mapDomainApp(mapUV.l), uU.l) // Map#Domain(m)[u] (m: Map U V, u: U)
    // def mapValues_m_v = MapSelect(mapValuesApp(mapUV.l), vV.l) // Map#Values(m)[v] (m: Map U V, v: V)
    // def mapElements_m_u = MapSelect(mapElementsApp(mapUV.l), uU.l) // Map#Elements(m)[u] (m: Map U V, u: U)


    // // val map = LocalVarDecl(Identifier("f")(axiomNamespace), fieldType)
    // // val value : String =
    // //   """
    // //     |type Map U V;
    // TypeDecl(mapType) ++
    // //     |
    // //     |// A Map is defined by three functions, Map#Domain, Map#Elements, and #Map#Card.
    // DeclComment("A Map is defined by three functions, Map#Domain, Map#Elements, and #Map#Card.") ++
    // //     |
    // //     |function Map#Domain<U,V>(Map U V) : Set U;
    // Func(mapDomainName, Seq(LocalVarDecl(mapName, mapType)), setUType) ++
    // //     |
    // //     |function Map#Elements<U,V>(Map U V) : [U]V;
    // Func(mapElementsName, Seq(LocalVarDecl(mapName, mapType)), boogieUVMapType) ++
    // //     |
    // //     |function Map#Card<U,V>(Map U V) : int;
    // Func(mapCardName, Seq(LocalVarDecl(mapName, mapType)), Int) ++
    // //     |
    // //     |axiom (forall<U,V> m: Map U V :: { Map#Card(m) } 0 <= Map#Card(m));
    // Axiom(Forall(
    //   mapUV, 
    //   Trigger(Seq(mapCardApp(mapUV.l))), 
    //   (IntLit(0) <= mapCardApp(mapUV.l)))) ++
    // //     |
    // //     |// The set of Keys of a Map are available by Map#Domain, and the cardinality of that
    // DeclComment("The set of Keys of a Map are available by Map#Domain, and the cardinality of that") ++
    // //     |// set is given by Map#Card.
    // DeclComment("set is given by Map#Card.") ++
    // //     |
    // //     |  /* added second trigger set */
    // DeclComment(" /* added second trigger set */") ++
    // //     |
    // //     |axiom (forall<U,V> m: Map U V :: { Set#Card(Map#Domain(m)) } { Map#Card(m) }
    // //     |  Set#Card(Map#Domain(m)) == Map#Card(m));
    // Axiom(Forall(
    //   mapUV, 
    //   Seq(Trigger(mapCardApp(mapDomainApp(mapUV.l))), Trigger(mapCardApp(mapUV.l))), 
    //   (mapCardApp(mapDomainApp(mapUV.l)) === mapCardApp(mapUV.l)) )) ++
    // //     |
    // //     |// The set of Values of a Map can be obtained by the function Map#Values, which is
    // //     |// defined as follows.  Remember, a Set is defined by membership (using Boogie's
    // //     |// square brackets) and Map#Card, so we need to define what these mean for the Set
    // //     |// returned by Map#Values.
    // DeclComment("The set of Values of a Map can be obtained by the function Map#Values, which is") ++
    // DeclComment("defined as follows.  Remember, a Set is defined by membership (using Boogie's") ++
    // DeclComment("square brackets) and Map#Card, so we need to define what these mean for the Set") ++
    // DeclComment("returned by Map#Values.") ++
    // //     |
    // //     |function Map#Values<U,V>(Map U V) : Set V;
    // Func(mapValuesName, Seq(LocalVarDecl(mapName, mapType)), setVType) ++
    // //     |
    // //     |  /* split axiom into each direction */
    // DeclComment(" /* split axiom into each direction */") ++
    // //     |
    // //     |axiom (forall<U,V> m: Map U V, v: V :: { Map#Values(m)[v] }
    // //     |  Map#Values(m)[v] ==>
    // //     |	(exists u: U :: { Map#Domain(m)[u] } { Map#Elements(m)[u] }
    // //     |	  Map#Domain(m)[u] &&
    // //     |    v == Map#Elements(m)[u]));
    // Axiom(Forall(
    //   Seq(mapUV, vV),
    //   Trigger(mapValues_m_v),
    //   (mapValues_m_v ==> Exists(
    //     uU, 
    //     Seq(Trigger(mapDomain_m_u), Trigger(mapElements_m_u)),
    //     (mapDomain_m_u && (vV.l === mapElements_m_u))
    //   )))) ++
    // //     |
    // //     |axiom (forall<U,V> m: Map U V, u: U ::  { Map#Elements(m)[u] } // { Map#Domain(m)[u] } // REMOVED this trigger due to a potential for matching loops
    // //     |	  Map#Domain(m)[u]
    // //     |    ==> Map#Values(m)[Map#Elements(m)[u]]);
    // Axiom(Forall(
    //   Seq(mapUV, uU),
    //   Trigger(mapElements_m_u),
    //   (mapDomain_m_u ==> MapSelect(mapValuesApp(mapUV.l), mapElements_m_u))
    //   )) ++
    // //     |// There's a potential for matching loops with the extra trigger if two maps have equal domains:
    // //     |// v in range(m1); some k in dom(m1) = dom(m2) s.t. m1[k] = v; m2[k] in range(m2); some k' in dom(m2) s.t. m2[k'] = m2[k]
    // DeclComment("There's a potential for matching loops with the extra trigger if two maps have equal domains:") ++
    // DeclComment("v in range(m1); some k in dom(m1) = dom(m2) s.t. m1[k] = v; m2[k] in range(m2); some k' in dom(m2) s.t. m2[k'] = m2[k]") ++
    // //     |
    // //     |axiom (forall<U,V> m: Map U V, u: U ::  { Map#Domain(m)[u] } { Map#Elements(m)[u] }
    // //     |	  Map#Domain(m)[u]
    // //     |    ==> Set#Card(Map#Values(m)) > 0); // weaker property than above, with weaker triggers
    // Axiom(Forall(
    //   Seq(mapUV, uU),
    //   Seq(Trigger(mapDomain_m_u), Trigger(mapElements_m_u)),
    //   (mapDomain_m_u ==> (setCardApp(mapValuesApp(mapUV.l)) > IntLit(0)))
    // )) ++
    // //     |
    // //     | // Here are the operations that produce Map values.
    // DeclComment("Here are the operations that produce Map values.") ++
    // //     |
    // //     |function Map#Empty<U, V>(): Map U V;
    // Func(mapEmptyName, Seq(), mapType) ++
    // //     |axiom (forall<U, V> u: U ::
    // //     |        { Map#Domain(Map#Empty(): Map U V)[u] }
    // //     |        !Map#Domain(Map#Empty(): Map U V)[u]);
    // Axiom(Forall(
    //   uU,
    //   Trigger(MapSelect(mapDomainApp(mapEmptyApp), uU.l)),
    //   UnExp(Not, MapSelect(mapDomainApp(mapEmptyApp), uU.l))
    // )) ++
    // //     |
    // //     |axiom (forall<U, V> m: Map U V :: { Map#Card(m) }
    // //     | (Map#Card(m) == 0 <==> m == Map#Empty()) &&
    // //     | (Map#Card(m) != 0 ==> (exists x: U :: Map#Domain(m)[x])) &&
    // //     | ((forall x: U :: {Map#Domain(m)[x]} Map#Domain(m)[x] ==> Map#Card(m) != 0)));
    // Axiom(Forall(
    //   mapUV,
    //   Trigger(mapCardApp(mapUV.l)),
    //   (mapCardApp(mapUV.l) === IntLit(0) <==> mapUV.l === mapEmptyApp) &&
    //   (mapCardApp(mapUV.l) !== IntLit(0) ==> Exists(uU2, Seq(), MapSelect(mapDomainApp(mapUV.l), uU2.l))) 
    //   //TODO && (forall....)
    // )) ++
    // //     |
    // //     |//Build is used in displays, and for map updates
    // //     |function Map#Build<U, V>(Map U V, U, V): Map U V;
    // Func(mapBuildName, 
    //   Seq(LocalVarDecl(map0Name, mapType), 
    //       LocalVarDecl(map1Name, mapType)), 
    //   mapType) ++
    // //     |
    // //     |/* added second trigger set (cf. example3 test case, test3) */
    // //     |axiom (forall<U, V> m: Map U V, u: U, u': U, v: V ::
    // //     |  { Map#Domain(Map#Build(m, u, v))[u'] } { Map#Domain(m)[u'],Map#Build(m, u, v) } { Map#Elements(Map#Build(m, u, v))[u'] }
    // //     |  (u' == u ==> Map#Domain(Map#Build(m, u, v))[u'] &&
    // //     |               Map#Elements(Map#Build(m, u, v))[u'] == v) &&
    // //     |  (u' != u ==> Map#Domain(Map#Build(m, u, v))[u'] == Map#Domain(m)[u'] &&
    // //     |               Map#Elements(Map#Build(m, u, v))[u'] == Map#Elements(m)[u']));
    // //     |/* added second trigger set (not sure of a test case needing it, though) */
    // //     |axiom (forall<U, V> m: Map U V, u: U, v: V :: { Map#Card(Map#Build(m, u, v)) }{ Map#Card(m),Map#Build(m, u, v) }
    // //     |  Map#Domain(m)[u] ==> Map#Card(Map#Build(m, u, v)) == Map#Card(m));
    // //     |/* added second trigger set (not sure of a test case needing it, though) */
    // //     |axiom (forall<U, V> m: Map U V, u: U, v: V :: { Map#Card(Map#Build(m, u, v)) }{ Map#Card(m),Map#Build(m, u, v) }
    // //     |  !Map#Domain(m)[u] ==> Map#Card(Map#Build(m, u, v)) == Map#Card(m) + 1);
    // //     |
    // //     |//equality for maps
    // //     |  // this axiom is only needed in one direction; the other is implied by the next axiom
    // //     |
    // //     |function Map#Equal<U, V>(Map U V, Map U V): bool;
    // Func(mapEqualName, 
    //   Seq(LocalVarDecl(map0Name, mapType), 
    //       LocalVarDecl(map1Name, mapType)), 
    //   Bool) ++
    // //     |axiom (forall<U, V> m: Map U V, m': Map U V::
    // //     |  { Map#Equal(m, m') }
    // //     |   (forall u : U :: Map#Domain(m)[u] == Map#Domain(m')[u]) &&
    // //     |     (forall u : U :: Map#Domain(m)[u] ==> Map#Elements(m)[u] == Map#Elements(m')[u]) ==> Map#Equal(m, m'));
    // //     |// extensionality
    // //     |axiom (forall<U, V> m: Map U V, m': Map U V::
    // //     |  { Map#Equal(m, m') }
    // //     |    Map#Equal(m, m') ==> m == m');
    // //     |
    // //     |function Map#Disjoint<U, V>(Map U V, Map U V): bool;
    // Func(mapDisjointName, 
    //   Seq(LocalVarDecl(map0Name, mapType), 
    //       LocalVarDecl(map1Name, mapType)), 
    //   Bool) ++
    // //     |// split in both directions
    // //     |axiom (forall<U, V> m: Map U V, m': Map U V ::
    // //     |  { Map#Disjoint(m, m') }
    // //     |    Map#Disjoint(m, m') ==> (forall o: U :: {Map#Domain(m)[o]} {Map#Domain(m')[o]} !Map#Domain(m)[o] || !Map#Domain(m')[o]));
    // //     |axiom (forall<U, V> m: Map U V, m': Map U V ::
    // //     |  { Map#Disjoint(m, m') }
    // //     |    !Map#Disjoint(m, m') ==> (exists o: U :: {Map#Domain(m)[o]} {Map#Domain(m')[o]} Map#Domain(m)[o] && Map#Domain(m')[o]));
    // //     |
    // //     |""".stripMargin
    // DeclComment("TODO: Remove this")

    if (used) Seq(LiteralDecl(MapAxiomatization.value)) else Seq()
  }

  override def start() : Unit = expModule.register(this)

  override def translateMapExp(exp : sil.Exp) : Exp = {
    used = true

    def rec(e : sil.Exp) = translateExp(e) // recurse
    val typ = translateType(exp.typ)

    exp match {
      case _: sil.EmptyMap => {
        val fa = FuncApp(Identifier(mapEmptyOpName), Nil, typ)
        fa.showReturnType = true // needed (in general) to avoid Boogie complaints about ambiguous type variable
        fa
      }

      case exp: sil.ExplicitMap =>
        translateMapExp(exp.desugared) // desugar into a series of map updates starting from an empty map
      case sil.MapCardinality(base) =>
        FuncApp(Identifier(mapCardOpName), Seq(rec(base)), Int)
      case sil.MapDomain(base) =>
        FuncApp(Identifier(mapDomainOpName), Seq(rec(base)), typ)
      case sil.MapRange(base) =>
        FuncApp(Identifier(mapValuesOpName), Seq(rec(base)), typ)
      case sil.MapUpdate(base, key, value) =>
        FuncApp(Identifier(mapBuildOpName), Seq(rec(base), rec(key), rec(value)), typ)
      case exp: sil.MapContains =>
        translateExp(exp.desugared)
      case sil.EqCmp(left, right) =>
        FuncApp(Identifier(mapEqualOpName), List(rec(left), rec(right)), typ)
      case sil.NeCmp(left, right) =>
        UnExp(Not, FuncApp(Identifier(mapEqualOpName), List(rec(left), rec(right)), typ))

      case sil.MapLookup(base, key) => base.typ match {
        case sil.MapType(keyType, valueType) => {
          val mTyp = MapType(Seq(translateType(keyType)), translateType(valueType))
          val mExp = FuncApp(Identifier(mapElementsOpName), Seq(rec(base)), mTyp)
          MapSelect(mExp, Seq(rec(key)))
        }
        case t => sys.error(s"expected a map type, but found $t")
      }

      case _ => sys.error("not a map expression")
    }
  }

  override def translateMapType(mapType : sil.MapType) : Type = {
    used = true
    NamedType("Map", Seq(translateType(mapType.keyType), translateType(mapType.valueType)))
  }

  override def simplePartialCheckDefinednessAfter(exp: sil.Exp, error: PartialVerificationError, makeChecks: Boolean, definednessStateOpt: Option[DefinednessState]): Stmt = {
    if (makeChecks) exp match {
      case sil.MapLookup(base, key) => {
        val containsExp = translateMapExp(sil.MapContains(key, base)(exp.pos, exp.info, exp.errT))
        Assert(containsExp, error.dueTo(reasons.MapKeyNotContained(base, key)))
      }
      case _ => Statements.EmptyStmt
    }
    else Statements.EmptyStmt
  }

  override def reset() : Unit = used = false
}

object DefaultMapModule {
  private def opName(name : String) = s"Map#$name"

  val mapBuildOpName : String = opName("Build")
  val mapCardOpName : String = opName("Card")
  val mapDomainOpName : String = opName("Domain")
  val mapElementsOpName : String = opName("Elements")
  val mapEmptyOpName : String = opName("Empty")
  val mapEqualOpName : String = opName("Equal")
  val mapValuesOpName : String = opName("Values")
}