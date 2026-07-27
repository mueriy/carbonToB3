package viper.carbon.utility

import viper.carbon.b3.B3Nodes.{Axiom, Forall, Function, FunctionCallExpr, FParameter, NamedType, Pattern, Type, TypeVar}
import viper.carbon.b3.B3Naming.{Namespace, Identifier}

/**
  * Representation of desugared version of polymorphic type
  * @param select B3 function for map lookups
  * @param store B3 function for map stores
  * @param axioms axioms constraining the select and store functions
  */
case class PolyMapRep(select: Seq[Function], store: Seq[Function], axioms: Seq[Axiom])

/***
  *  Class that can desugar a specific category of polymorphic Boogie maps, namely Boogie maps of the form
  *  {@code <...>[ref, Field ...]RangeType} where \code{ref} is type representing references (no type arguments),
  *  {@code Field} is a type constructor representing fields.
  * @param fieldTypeVariants The typeArgs of all Field variants for which we want to create a mapping. 
  * @param refType type representing references
  * @param fieldTypeConstructor constructs a field type given the type arguments provided by [[fieldTypeVariants]]
  * @param namespace
  */
case class PolyMapDesugarHelper(fieldTypeVariants: Seq[Seq[Type]], refType: Type, fieldTypeConstructor: Seq[Type] => NamedType, namespace: Namespace) {
  implicit val ns = namespace

  /**
    * Creates store and select functions with corresponding axioms to desugar a Boogie map of the form
    * {@code <...>[ref, Field ... ...]RangeType} .
    * @param mapRepNameConstr         the name constructor that should be used to construct the map name representer
    * @param mapRepTypeConstr         the type constructor that should be used to construct the map type representer
    * @param selectAndStoreId         the identifiers for selection and store functions
    * @param mapRangeTypeFromField    the range type of the map as a function of the field type
    * @return [[PolyMapRep]] representation of desugared type
    */
  def desugarPolyMap(mapRepNameConstr: Seq[Type] => String,
                     mapRepTypeConstr: Seq[Type] => NamedType,
                     selectAndStoreId: (Identifier, Identifier),
                     mapRangeTypeFromField: Type => Type): PolyMapRep =  {
    val (selectId, storeId) = selectAndStoreId
    var polyMapRep = PolyMapRep(Seq(), Seq(), Seq())
    fieldTypeVariants map {case typArgs =>
      val mapTypeId = Identifier(mapRepNameConstr(typArgs))
      val h = FParameter(mapTypeId, mapRepTypeConstr(typArgs))
      val obj = FParameter(Identifier("obj"), refType)
      val obj2 = FParameter(Identifier("obj2"), refType)

      val field = FParameter(Identifier("f"),
                    fieldTypeConstructor(typArgs))
      val field2 = FParameter(Identifier("f2"),
                    fieldTypeConstructor(typArgs))
      val declInHeapRange = FParameter(Identifier("v"), 
                    mapRangeTypeFromField(field.typ))

      val selectFun =
        Function(selectId,
          Seq(h, obj, field),
          mapRangeTypeFromField(field.typ))
      val storeFun =
        Function(storeId,
          Seq(h, obj, field, declInHeapRange),
          mapRepTypeConstr(typArgs))
      val readUpdateGeneral =
        FunctionCallExpr(selectId,
          Seq(FunctionCallExpr(storeId, Seq(h.l, obj.l, field.l, declInHeapRange.l), mapRepTypeConstr(typArgs)), obj2.l, field2.l),
          mapRangeTypeFromField(field2.typ)
        )
      val axioms =
        Seq(
          Axiom(Forall(
            Seq(h.toQ,obj.toQ,field.toQ,declInHeapRange.toQ),
            Seq(Pattern(Seq(FunctionCallExpr(storeId, Seq(h.l, obj.l, field.l, declInHeapRange.l), mapRepTypeConstr(typArgs))))),
            FunctionCallExpr(selectId,
              Seq(FunctionCallExpr(storeId, Seq(h.l, obj.l, field.l, declInHeapRange.l), mapRepTypeConstr(typArgs)), obj.l, field.l),
              mapRangeTypeFromField(field.typ)
            ) === declInHeapRange.l
          )),
            Axiom(Forall(
              Seq(h.toQ,obj.toQ,obj2.toQ, field.toQ,field2.toQ, declInHeapRange.toQ),
              Seq(Pattern(Seq(readUpdateGeneral))),
              ( (obj.l !== obj2.l) || (field.l !== field2.l) ) ==>
                ( readUpdateGeneral === FunctionCallExpr(selectId, Seq(h.l, obj2.l, field2.l),
                  mapRangeTypeFromField(field2.typ) ) )
            )))

      polyMapRep match {
        case PolyMapRep(selectFunPrev, storeFunPrev, axiomsPrev) => 
          polyMapRep = PolyMapRep(selectFunPrev:+selectFun, storeFunPrev:+storeFun, axiomsPrev++axioms)
      }
    }
    polyMapRep
  }
}