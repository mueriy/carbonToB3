package viper.carbon.b3
import viper.carbon.b3.{B3Adapter => B3}
import viper.carbon.boogie._
import language.implicitConversions

/**
 * An implementation for transformers to transform Boogie AST -> B3 AST (RawAst).
 * Cannot reuse the boogie-Transformer used by Carbon, because B3 AST-nodes don't have a shared ancestor like "Node".
 */
object BoogieToB3Transformer {
  // Uses the following implicits: idName (Identifier -> String)

  // IDENTIFIER UNIQUENESS (and other properties; stolen from PrettyPrinter; 'ident2doc' -> 'idName')
  /** The current mapping from identifier to names. */
  private val idnMap = collection.mutable.HashMap[Identifier, String]()

  /** B3NameGenerator instance. */ 
  private val names = new B3NameGenerator()



  /**
    * The current mapping from unique Boogie names to the original identifiers (inverse mapping of idnMap,
    * where the names of the identifiers are used directly).
    */
  val backMap = collection.mutable.HashMap[String, String]()

  /** Map an identifier to a string, making it unique first if necessary. */
  implicit def idName(i: Identifier): String = {
    idnMap.get(i) match {
      case Some(s) => s
      case None =>
        val s = names.createUniqueIdentifier(i.preferredName)
        idnMap.put(i, s)
        backMap.update(s, i.name)
        s
    }
  }

  /** The current store for where clauses of identifiers. */
  private val whereMap = collection.mutable.HashMap[Identifier, Exp]()
  /** All global variables of the Program (as 'GlobalVarDecl's) */
  private var globalVars = Seq[GlobalVarDecl]()
  /** All global variables of the Program (as B3 'PParameter's) */
  private var globalInOutPPar = Seq[RawAst.PParameter]()

  /** Map of (Const) Identifier -> corresponding Type name. Used to know the type of constants when they are used somewhere. */
  private val constIdentifierTypeMap = collection.mutable.HashMap[Identifier, String]()


  // FUNCTION NAMING (special because of parametrized functions)
  /** Stores all in/output combos of functions that were used somewhere and have not yet been declared.
   * These all need to be declared! Don't add any already declared function here (see declaredFuncs).
   * A combo for some functionName is of the form: Seq(inTyp1Name,...,inTypNName, outTypName) */
  private val usedButUndeclaredFuncs = collection.mutable.MultiDict.empty[String, Seq[String]]
  /** Collection of all functions that have been declared. */
  private val declaredFuncs = collection.mutable.Set[(String, Seq[String])]()
  /** stores for each parametric function how to name it given concrete input and output types 
   * It maps Identifiers to an Int sequence. Each value x in the sequence says: "the x'th parameter's type should be used 
   * for naming". Here, the output type counts as the "last parameter", so if x == |in-parameters| then the output type is also
   * included in the name. */
  private val paramFuctionMap = collection.mutable.HashMap[Identifier, Seq[Int]]()
  /** returns the correct name to use for the given identifier. For parametric functions, the name depends on
   * the given parameter and output types, according to what is defined in paramFuctionMap */
  private def fctName(name: Identifier, parameterTypeNames: Seq[String], outputTypeName: String): String = {
    paramFuctionMap.get(name) match {
      case Some(paramFuctionHandler) => 
        info("FunctionName: ", "("+name.name+", "+(parameterTypeNames ++ Seq(outputTypeName))+", "+paramFuctionHandler+")")
        name+"%F"+paramFuctionHandler.collect(parameterTypeNames ++ Seq(outputTypeName)).mkString("%%")
      case None => name
    } 
  }

  // ALL TYPES
  /** Helper class to declare all needed types and for use by triggers to generate for all combinations */
  private val allTypeDecls = new AllTypesInB3()


  // DEVELOPMENT & DEBUGGING
  // settings
  private val UNPACK_SEQN = true // true = easier to ready code, false = easier to compare to PrettyPrinted code (see method: uncomment)
  private val DEBUG_MODE = 1 // 0 = ignore, 1 = collect (+ print later), 2 = collect & print (+ print later)
  private val USE_CHECK = false

  // other
  private val debug_infoPlus = collection.mutable.Map[String, Seq[String]]()
  private val debug_info = collection.mutable.Set[String]()
  private def info(baseMsg: String, moreInfo: String = ""): Unit = {
    if (DEBUG_MODE == 0) {
      return
    }
    if (DEBUG_MODE >= 1) {
      if (moreInfo == "") {
        debug_info += baseMsg
      } else {
        debug_infoPlus.update(baseMsg, Seq(moreInfo) ++ debug_infoPlus.getOrElse(baseMsg, Nil))
      }
    }
    if (DEBUG_MODE == 2) {
      println(baseMsg + moreInfo)
    }
  }
  def printInfo(): Unit = {
    if (DEBUG_MODE == 0) {
      return
    }
    println("====== COLLECTED INFOS START ======")
    debug_infoPlus.foreach{case (k, v) => println(k + "\n  " + v.toSet.mkString(", "))}
    debug_info.foreach{v => println(v)}
    println("======= COLLECTED INFOS END =======")
  }

  /** returns the name of the input's class as a string */ 
  def printClass[T](x: T): String = {
    x.getClass.getSimpleName
  }





  // TRANSFORM AST NODES
  /**
   * Transforms a Boogie AST into the corresponding raw B3 AST.
   * Currently only ONE usage per run of carbon is supported! (To allow conversion of multiple programs
   *  with one run of carbon, some vals/vars of the BoogieToB3Transformer object would need to be reset)
   * 
   * @param prog An (extended) boogie AST (Program)
   * @return A raw B3 AST
   */
  def transformProgram(prog: Program): RawAst.Program = {
    // (Ignore header field - we cannot convert comments)
    // Eliminate all CommentedDecl-s
    val flatDeclSeq = flattenedDecl(prog.decls)

    // return B3.Program(Seq(), Seq(), Seq(), Seq(), Seq(), Seq(), Seq())
    
    // TODO: eliminate LiteralDecl [written boogie code], as that is not convertible

    // 1) collect all types, create type hooks
    //TypeDecl
    allTypeDecls.collectTypeCombos(prog)
    // TODO: (not here) add updates to allTypeDecls everywhere where new parameter combos can be found

    // 1.5) GlobalVars & Procedures (can easily keep order)
    //GlobalVars
    // Pre-prepare inout PParameters from global variables 
    // (Carbon declares all GlobalVars as being modified in every Procedure in Boogie 
    //  => we will always define all of them as inout parameters in B3) 
    globalVars = flatDeclSeq.collect({case gvar: GlobalVarDecl => gvar})
    globalVars map {gvar => info("collected GlobalVarDecls:", gvar.name)}
    globalInOutPPar = globalVars map {gvar => B3.PParameter(gvar.name, getNameFromTyp(gvar.typ), B3.INOUT)}
    //TODO: Remove globalInOutPPar-update in DEVELOPMENT part
    // TODO: WhereMap for global variables (e.g.: "var globalVarName: Int where globalVarName > 0")
    //Procedures
    val tProcedures = flatDeclSeq.collect({case proc: Procedure => transformProcedure(proc)})

    // 2) create hooks for functions & axioms (can include numbers to keep order)
    //Function
    // a) collect all functions (in order)
    // b) analyze naming pattern (i.e. naming parameters) and (system for) trigger types
    // c) instantiation for each type (combo)
    //Creating functions can 

    //Axioms TODO
    val tAxioms = Seq()//flatDeclSeq.collect({case ax: Axiom => transformAxiom(ax)})

    // 3) transform ConstDecl, TypeAlias (only 4 type aliases), and GlobalVars
    //ConstDecl
    // Const Id->TypName (constIdentifierTypeMap) is done by allTypeDecls
    // Instead of constants we use uninterpreted nullary functions, which always return the same value (= have constant value)
    // For unique constants we can use B3 tags, as all functions tagged with the same tag return pairwise distinct values.
    val const_tags = collection.mutable.Set[(String, String)]()
    val constFcts = flatDeclSeq.collect({
      case ConstDecl(name, typ, false) => B3.Function(name, Seq(), getNameFromTyp(typ))
      case ConstDecl(name, typ, true) => //unique
        val typName = getNameFromTyp(typ)
        val tagName = "%ConstTag_"+typName
        const_tags += ((tagName, typName))
        B3.Function(name, Seq(), typName, tagName)
    })
    val constTaggers = const_tags.map({case ((tag, typ)) => B3.Tagger(tag, typ)}).toSeq

    //TypeAlias
    val boogieTypeAliases = flatDeclSeq collect {case typAlias: TypeAlias => typAlias}
    boogieTypeAliases map {ta => info("INFO: has following type aliases: ", getNameFromTyp(ta.name))}
    boogieTypeAliases map { _ match {
      case ta: TypeAlias if getNameFromTyp(ta.name) == "Perm" => null //type "aliasing" is done by replacing "Perm" with "real" (or "int" until B3 supports "real")
      case ta: TypeAlias if getNameFromTyp(ta.name) == "HeapType" =>  "TODO"
        // type HeapType = <A, B> [Ref, Field A B]B;
        // TypeAlias(heapTyp, MapType(Seq(refType, fieldType), TypeVar("B"), Seq(TypeVar("A"), TypeVar("B"))))

        // TODO: double Map approach

      case ta: TypeAlias if Seq("MaskType", "PMaskType").contains(getNameFromTyp(ta.name)) =>  "TODO"
        // type MaskType = <A, B> [Ref, Field A B]Perm;
        // TypeAlias(maskType, MapType(Seq(refType, fieldType), permType, fieldType.freeTypeVars))
        // type PMaskType = <A, B> [Ref, Field A B]bool;
        // TypeAlias(pmaskType, MapType(Seq(refType, fieldType), Bool, fieldType.freeTypeVars))

        // TODO: Single Map approach

      case ta@_ => info("ERROR: unknown type alias: ", getNameFromTyp(ta.name))
      // TypeAlias(name: NamedType, definition: Type)
    }}
    // (only permType (Perm = Real), pmaskType (PMaskType), maskType (MaskType), heapTyp (HeapType) (3x boogie map))

    // TODO: normal map (types linked to Map/Set/Seq instantiations)







    // 5) activate triggers (sort result according to the numbers) 

    val funcs = flatDeclSeq.collect({case func: Func => func})
    funcs map {func => collectParamFuncs(func)}
    val tFuncs = funcs flatMap {func => declareAllFuncVariants(func)}
    // TODO: multiple rounds of:
    // - check if more axiom (variants) should be activated
    // - tFuncs = tFuncs ++ (funcs flatMap {func => declareAllFuncVariants(func)})
    // (and then obviously change val tFuncs -> var tFuncs)

    {// DEVELOPMENT vvv
    val usedMapping = collection.mutable.Set[String]()
    flatDeclSeq map { x => x match {
      case Axiom(exp) => 
        info("Existing Axiom with exp type: ", printClass(exp))
        usedMapping += "Axiom"
      case CommentedDecl(s, d, size, nLines) => usedMapping += "CommentedDecl"
      case ConstDecl(name, typ, unique) => 
        info("Existing ConstDecl: ", name.name)
        usedMapping += "ConstDecl"
      case DeclComment(s) => usedMapping += "DeclComment"
      case Func(name, args, typ, attributes) => usedMapping += "Func"
      case GlobalVarDecl(name, typ) => info("INFO: GlobalVarDecl of name: ", name); usedMapping += "GlobalVarDecl"
      case LiteralDecl(boogieString) => usedMapping += "LiteralDecl" // (<-- inserts boogie code as string; not translate-able)
      case Procedure(name, ins, outs, body) => usedMapping += "Procedure"
      case TypeAlias(name, definition) => usedMapping += "TypeAlias"
      case TypeDecl(NamedType(_, Seq())) => usedMapping += "TypeDecl simple"
      case TypeDecl(t) => usedMapping += "TypeDecl complex"
    }}
    usedMapping.map(_ match {
      case "Func" => None
      case "CommentedDecl" => None
      case "DeclComment" => None
      case "Procedure" => None
      case "Axiom" => None
      case "TypeDecl simple" => None
      case notImplementedDecl => info("TODO: Decl ", notImplementedDecl)})
    }
    // TODO: remove these after (re-)adding the preludes 
    val alwaysIncludeFcts = Seq() //Seq(B3.Function("dummyFunction", Seq(B3.FParameter("x", "int")), "bool"),
                                // for now we only support integer maps
                                // B3.Function("MapSelect", Seq(B3.FParameter("x", "int")), "int"),
                                // B3.Function("MapUpdate", Seq(B3.FParameter("x", "int")), "int"),
                                // B3.Function("ZeroMask", Seq(), "MaskType"),
                                // B3.Function("AssumePermUpperBound", Seq(), "bool"),
                                // B3.Function("state", Seq(B3.FParameter("x", "HeapType"), B3.FParameter("y", "MaskType")), "bool"),
                                // )
    // globalInOutPPar = globalInOutPPar ++ Seq(B3.PParameter("Mask", "MaskType", B3.INOUT), B3.PParameter("Heap", "HeapType", B3.INOUT))
    val alwaysIncludeTyps = Seq("real", "MaskType", "HeapType", "Perm", "Seq", "Field", "PMaskType").map(x => B3.TypeDecl(x))

    prog.decls.collect({case LiteralDecl(boogieString) => info("DEBUG: LiteralDecl", boogieString.take(20))})
    // DEVELOPMENT ^^^

    
    // Create B3 Program using the B3 version of the correct (Boogie) Decl nodes
    B3.Program(signatureTypes = Seq[String](), 
               domains = Seq[RawAst.Domain](),
               types = alwaysIncludeTyps ++ allTypeDecls.declareAllB3Types(),
               taggers = constTaggers,
               functions = alwaysIncludeFcts ++ constFcts ++ tFuncs,
               axioms = tAxioms,
               procedures = tProcedures)
  }

  
  /**
    * Transforms a Boogie function (Func) AST node into all corresponding raw B3 nodes. 
    * Creates one function-variant for each type combination that has appeared somewhere.
    * Requires that 'allUsedFuncs' has only correct function-combos saved!
    *
    * @param fct A boogie function (AST node). Attributes are currently not supported.
    * @return A Seq of the corresponding raw B3 nodes, or empty if the function was never used.
    */
  private def declareAllFuncVariants(func: Func): Seq[RawAst.Function] = {
    val usedCombosOfFunc = usedButUndeclaredFuncs.get(func.name)
    usedCombosOfFunc.toSeq map {argTypeCombo => 
      val argNames = func.args map {arg => arg.name}
      val argTypeNames = argTypeCombo.init
      val argAndTypeNames = argNames.zip(argTypeNames)
      val functionTypeName = argTypeCombo.last
      declaredFuncs += ((func.name, argTypeCombo))
      val funcName = fctName(func.name, argTypeNames, functionTypeName)
      B3.Function(funcName, argAndTypeNames map {p => B3.FParameter(p._1, p._2)}, functionTypeName)
    }
  }

  /** Adds the sequence of indexes of the func args that contain free typ vars to paramFuctionMap
   * (the function output is treated as if it was the last func arg) */
  private def collectParamFuncs(func: Func): Unit = {
    val inAndOutTypsWithIndexes = ((func.args map (_.typ)) ++ Seq(func.typ)).zipWithIndex
    val freeTypVarIndexes = inAndOutTypsWithIndexes.collect({
      case (arg, idx) if !arg.freeTypeVars.isEmpty => idx
    })
    if (!freeTypVarIndexes.isEmpty) {
      paramFuctionMap.addOne(func.name, freeTypVarIndexes)
    }
  }


  /**
    * Transforms a Boogie function (Func) AST node into the corresponding raw B3 node.
    *
    * @param fct A boogie function (AST node). Attributes are currently not supported.
    * @return The corresponding raw B3 node. 
    */
  private def transformFunction(fct: Func): RawAst.Function = {
    // TODO: Func also has .attributes, which is a Map that seems to be usually empty. Only translateBackendFunc 
    // in DefaultFuncPredModule creates a mapping there ("builtin" -> ...). Boogie's manual only has ":bvBuiltin". 
    // Find out what "builtin" does in Boogie, in what cases this is used, and then support it (if necessairy). 
    if (!fct.attributes.isEmpty) {
      sys.error("TODO: function attributes not supported (probably tried to use 'builtin: ...')")
    }
    B3.Function(fct.name, fct.args map {p => B3.FParameter(p.name, getNameFromTyp(p.typ))}, getNameFromTyp(fct.typ))
  }

  /** Transforms a Boogie Axiom AST node into the corresponding raw B3 node. */ 
  private def transformAxiom(axiom: Axiom): RawAst.Axiom = {
    /* According to https://b3-lang.org/krml301.html#sec-dependencies:
    "experience has shown that extraneous axioms, however irrelevant to the proof at hand, 
     can severely slow down solver performance." */
    /* This means we should restrict the use of axioms only to where it could actually be helpful.
    We can do that by defining "explains Identifier"s, which we can do by providing a Seq of the 
    Identifier's names as the first parameter of B3.Axiom. Only function identifiers! This means 
    (according to https://b3-lang.org/top-level-decls.html#axioms) that the axiom is only used 
    in proof obligations where all stated functions appear - either directly in the proof 
    obligation or in another active axiom. */
    /* At the same time, Carbon does not support this for Boogie, so we might have to come up with
    our own rules on how to infer this. */

    // Possible Axiom Expressions currently generated by Carbon:
    // Forall, MaybeForall, (DefaultStateModule->) FuncApp(Identifier(isGoodState), stateExps, Bool),  (DefaultHeapModule->) UnExp(Not, FuncApp) (DefaultDomainModule->) any Exp
    // Axiom(noPerm === RealLit(0))
    // Axiom(fullPerm === RealLit(1))

    // For now we do NOT use "explains", so all axioms are always used in the proof. TODO: infer "explains"-identifiers to increase efficiency. 
    B3.Axiom(Seq(), transformExpr(axiom.exp))
  }

  /** flattens the sequence by removing all CommentedDecl, but keeping all Decl it contains */
  private def flattenedDecl(decls: Seq[Decl]): Seq[Decl] = {
    decls flatMap {
      case commDecl: CommentedDecl => flattenedDecl(commDecl.d)
      case _: DeclComment => Seq()
      case decl => Seq(decl)
    }
  } 

  /** 
   * Removes Comment nodes from Seqn nodes and all CommentBlock nodes (recursively). 
   * Returns Seqn(Seq()) if there is no useful Stmt in the given Node-branch. 
   * If there is only one useful statement it returns that, otherwise it returns a Seqn.
   * Statement order remains the same.
   * All Seqn in the returned branch contain multiple useful statements (otherwise they
   *  would be replaced by the single useful statement they contain, or removed). 
   * If UNPACK_SEQN is set, nested Seqn are also un-nested in a single Seqn.
   * Not useful are comments, and any Seqn that are empty or contain no useful statement.
   * 
   * CommentBlock is replaced with the statement it contains. If CommentBlock is inside a
   * Seqn (A) and also contains a Seqn (B), then the elements of (B) are inserted into (A), 
   * keeping the order of statements. (i.e.. Seqn(a, b, CommentBlock(Seqn(x, y)), d) => Seqn(a, b, x, y, d)). 
   * 
   * TODO: PrettyPrinter also ignores LocalVarWhereDecl stmts; check what's up with that!
   */
  private def uncomment(stmt: Stmt): Stmt = {
    stmt match {
      case CommentBlock(_, cstmt) => uncomment(cstmt)
      case Seqn(stmts) =>
        // remove comments
        val uncommStmtSeq = stmts map uncomment filter {
          case Seqn(Seq()) => false
          case _: Comment => false
          case _ => true
        }
        // unpack seqn in seqn (if UNPACK_SEQN -> less fluff -> makes looking at code easier; however, fluff helps to compare with PrettyPrinted code, so we might not always want that)
        val unpackedStmtSeq = uncommStmtSeq flatMap {
          case Seqn(subStmts) if UNPACK_SEQN => subStmts
          case other => Seq(other)
        }
        // avoid unnecessairy Seqn
        unpackedStmtSeq match {
          case Seq() => Seqn(Seq())
          case Seq(oneStmt) => oneStmt
          case _ => Seqn(unpackedStmtSeq)
        }
      case _ => stmt
    }
  } 

  /** Returns the type name from a Boogie Type */ 
  private def getNameFromTyp(typ: Type): String = {
    typ match {
      case Bool => "bool"
      case Int => "int"
      case Real =>                     info("TODO: Perm/real Type; Currently replaced with 'int'"); "int"
      case MapType(_, _, _) =>         sys.error("ERROR: cannot get name from MapType. This should be part of a TypeAlias, whose name you probably want!")
      case NamedType(name, Seq()) => if (name == "Perm") { getNameFromTyp(Real) } else { name }
      case NamedType(name, typVars) => 
        val typNames = typVars map getNameFromTyp
        getNameFromParamTypeConstel(name, typNames)
      case TypeVar(name) =>            info("TODO: TypeVar: ", name); name // Probably use a "current replacement map" and replace with whatever is given there. That map would be set when triggering the axiom/function creation.
    }
  }

  /** 
   * Returns the B3-transformed type name for a parametrized type given its name and the names of (concrete) parameters.
   * E.g. for "Field Int Bool":
   * 
   * @param name name of the type (without parameters) (e.g. "Field")
   * @param typVarNames parameter names as Seq of Strings. (e.g. ["Int", "Bool"])
   * @return The same name as would be used in boogie, but spaces are replaced by %% (=> e.g. "Field%%Int%%Bool")
   */
  private def getNameFromParamTypeConstel(name: String, typVarNames: Seq[String]): String = {
    info("TODO: NamedType: ", name + " " + typVarNames.mkString(" "))
    // TODO: check that e.g. "Field (Field Int Int) Bool" -> "Field%%Field%%Int%%Int%%Bool" will really always 
    // be unique or if we need some %-code for ( and ). (I think it is, but need to get proof.)
    name + "%%" + typVarNames.mkString("%%")
  }

  /** Transform Boogie Procedure -> raw B3 Procedure */
  private def transformProcedure(proc: Procedure): RawAst.Procedure = {
    // LATER: implement LocalVarWhereDecl-functionality; this is only used for the permission value "wildcard"; 
    //        LocalVarWhereDecl means restricting the possible variable values when initiating/havocing to a random value 
    whereMap.clear()
    proc.body visit {
      case LocalVarWhereDecl(idn, where) =>
        whereMap.put(idn, where)
    }

    // define procedure body
    val body = uncomment(proc.body)
    val b3ProcBody = body match {
      case Seqn(Seq()) => 
        B3.Option_None[RawAst.Stmt]
      case Seqn(stmts) => 
        // Most variables are not declared in the body, so we add var-declarations at the start of the transformed procedure body. 
        // Collect undeclared variables (stolen from boogie.PrettyPrinter)
        val undecl = proc.body.undeclLocalVars.filter(v1 => (proc.ins ++ proc.outs).forall(v2 => v2.name != v1.name))
        // In B3, variables can only be used in the body of the corresponding VarDecl node (otherwise they are "out of scope")
        // This means we need to nest all VarDecls and define the (transformed) procedure body as the innermost body. 
        val transBody = transformStatement(body, false)
        val varDeclarations = undecl.foldRight(transBody)((l, r) => B3.Stmt_VarDecl(l.name, r, getNameFromTyp(l.typ))) 
        //LATER: support LocalVarWhereDecl-functionality
        B3.Option_Some(varDeclarations)
      case _ => 
        info("ERROR: Procedure body should be Seqn (or Seqn(Seq()) as placeholder), but is: ", body.getClass.getSimpleName);
        B3.Option_None[RawAst.Stmt]
    }
    
    // proc.body match {
    //   case Seqn(seq) => println("DEBUG: " + seq.foreach(d => println(d.getClass.getName)))
    //   case _ => println("DEBUG: " + proc.body.getClass.getName)
    // }

    // finally, creating raw B3 Procedure
    B3.Procedure(name = proc.name,
                 parameters = transformPParameters(proc.ins, proc.outs), // (Global vars are handled by transformPParameters directly)
                 pre = Seq[RawAst.AExpr](),     // No data for these, but also empty in Boogie
                 post = Seq[RawAst.AExpr](),    // No data for these, but also empty in Boogie
                                                // TODO-later: Boogie additionally has "modifies", which is used there for Heap stuff. Need to find workaround
                 body = b3ProcBody)
  }

  /** get B3 Procedure parameters from Boogie in & out parameters */
  private def transformPParameters(ins: Seq[LocalVarDecl], outs: Seq[LocalVarDecl]): Seq[RawAst.PParameter] = {
    // Since ins/outs are LocalVarDecl, which is neither LocalVarWhereDecl nor does it extend it, 
    // we dont have to worry about the boogies "where" functionality here
    val inPPar  = ins  map {par => B3.PParameter(par.name, getNameFromTyp(par.typ), B3.IN)}
    val outPPar = outs map {par => B3.PParameter(par.name, getNameFromTyp(par.typ), B3.OUT)}
    inPPar ++ outPPar ++ globalInOutPPar
  }


  /**
    * Transforms Boogie Stmt node -> (raw) B3 Stmt node. It also first simplifies the statement by
    * removing comments and maybe unnesting Seqn blocks (depends on DEBUG_MODE configuration) 
    *
    * @param stmt A Boogie Stmt node. 
    * @param removeComments set to false if the statement branch is already uncomment(...)-ed (default: true)
    * @return The corresponding (raw) B3 Stmt node.
    */
  private def transformStatement(stmtIn: Stmt, removeComments: Boolean = true): RawAst.Stmt = {
    val stmt = if (removeComments) uncomment(stmtIn) else stmtIn
    // info("DEBUG: transformStatement(x), where x has type ", stmt.getClass.getName)
    stmt match {
      case _: Goto => info("LATER: Goto");                                          B3.LATER_Stmt()
      case AssertImpl(exp, error) => 
        if (USE_CHECK) {
          B3.Stmt_Check(transformExpr(exp), error.readableMessage) // TODO: check vs assert (for now we use check to receive the result of each "assertion")
        } else {
          B3.Stmt_Assert(transformExpr(exp), error.readableMessage)
        }
      case Assign(lhs, rhs) =>
        lhs match {
          case LocalVar(name, typ)  => B3.Stmt_Assign(name, transformExpr(rhs))
          case GlobalVar(name, typ) => B3.Stmt_Assign(name, transformExpr(rhs))            
          case _ => sys.error("FAIL: Expected lhs of Assign stmt to be LocalVar (or GlobalVar), but it was " + lhs.getClass.getName)
        }
      case Assume(exp) => B3.Stmt_Assume(transformExpr(exp))
      case _: Comment => info("FAIL: Comment stmts should be pre-removed!!! Inserted empty stmt block instead"); B3.Stmt_Block(Seq())
      case CommentBlock(_, stmt) => info("ERROR: transformStatement(CommentBlock)"); transformStatement(stmt, false)
      case HavocImpl(vars) => 
        // Boogie can define variables with an added "where [expr]", which means that whenever this variable is assigned a random value
        // that this value fulfills [expr]. Since B3 does NOT have this, we need to add an assume after havoc-ing (= reinit-ing)
        // (see also LocalVarWhereDecl or the whereMap)
        val exprsToAssume = (vars map {v => whereMap.get(v.name)}).flatten 
        exprsToAssume match {
          case Seq() => B3.Stmt_Reinit(vars map {v => v.name})
          case _ => B3.Stmt_Block(Seq(B3.Stmt_Reinit(vars map {v => v.name})) ++
                                  exprsToAssume.map(exp => B3.Stmt_Assume(transformExpr(exp))))
        }
      case If(cond, thn, els) =>
        B3.Stmt_If(transformExpr(cond), transformStatement(thn), transformStatement(els)) //QUEST: for directly nested if's we could try whether If-case is more efficient
      case _: Label => info("LATER: Label");                                     B3.LATER_Stmt() //Carbon does not generate label: ... return, so this is only needed for goto
      case _: LocalVarWhereDecl => info("TODO: LocalVarWhereDecl");              B3.TODO_Stmt()
      case NondetIf(thn, els) => B3.Stmt_Choose(Seq(thn, els).map(transformStatement(_))) //QUEST: we could check if thn or els is another NondetIf and then add all substatements into a single Stmt_Choose
      case Seqn(stmts) =>
        B3.Stmt_Block(stmts.map(s => transformStatement(s, false)))
    }
  }

  /**
    * Transforms a Boogie Exp node (sub-tree) into the corresponding B3 Expr sub-tree.   
    *
    * @param exp
    * @return
    */
  private def transformExpr(exp: Exp): RawAst.Expr = {
    // info("DEBUG: +++ transformExpr(x), where x has type ", exp.getClass.getName)
    // B3 and Boogie operators seem to have the same associativity.
    // B3 does not have the Boogie operators ++ and <:, but both not used by Carbon. 
    // B3 does also not support the > and >= operators, which means that we swap the left and right expr and use < and <= instead
    // B3 does have the <== operator, but we dont use it, so we dont care about its associativity. 
    
    exp match {
      case BinExp(left, binop, right) =>
        binop match {
          case Div         => sys.error("TODO: (Real) Div")
          case GeCmp|GtCmp => B3.Expr_OperatorExpr(transformBinOp(binop), Seq(right, left) map transformExpr) // we must use right </<= left instead of left >/>= right
          case _           => B3.Expr_OperatorExpr(transformBinOp(binop), Seq(left, right) map transformExpr) 
        }
      case CondExp(cond, thn, els) => B3.Expr_OperatorExpr(B3.CondExp, Seq(cond, thn, els) map transformExpr)
      case Const(name) => B3.FunctionCallExpr(name, Seq()) //we can simulate Boogie-constants using a nullary function (we can keep the name)
      case Exists(vars, triggers, exp, weight) =>                    // TODO: find out why Exists does not have typeVars. Forall uses that for parametric types; can we not use parametric types in Exists stmts?
        if (weight != None) info("TODO: Exists with weight != None") // TODO: what is weight?

        val boundVars = vars map {vardecl => B3.Binding(vardecl.name, getNameFromTyp(vardecl.typ))}
        val patterns = triggers map {trigger => trigger.exps map {exp => transformExpr(exp)}}
        B3.Expr_QuantifierExpr(false, boundVars, patterns, transformExpr(exp))
      case FalseLit() => B3.Expr_BLiteral(false)
      case Forall(vars, triggers, exp, typeVars, weight) =>          // TODO: [typeVars show parametric types (e.g. A & B for Map<A, B>(...))] -> Check here again when implementing parametric types
        if (weight != None) info("TODO: Forall with weight != None") // TODO: what is weight?

        val boundVars = vars map {vardecl => B3.Binding(vardecl.name, getNameFromTyp(vardecl.typ))}
        val patterns = triggers map {trigger => trigger.exps map {exp => transformExpr(exp)}}
        B3.Expr_QuantifierExpr(true, boundVars, patterns, transformExpr(exp))
      case FuncApp(name, args, typ) => 
        val argTypes = args map getTypeOfExpr
        val argTypeNames = argTypes map getNameFromTyp
        val funcTypeName = getNameFromTyp(typ)
        val freeTypeVars = argTypes ++ Seq(typ) flatMap (_.freeTypeVars)
        val funcCombo = ((idName(name), argTypeNames++Seq(funcTypeName)))
        if (freeTypeVars.isEmpty && !declaredFuncs.contains(funcCombo)) {
          usedButUndeclaredFuncs += funcCombo
        }
        B3.FunctionCallExpr(fctName(name, argTypeNames, funcTypeName), args map transformExpr)
      case GlobalVar(name, _) => B3.Expr_IdExpr(name)
      case IntLit(i) => B3.Expr_ILiteral(i)
      case LocalVar(name, _) => B3.Expr_IdExpr(name)
      case MapSelect(_, _) => info("TODO: MapSelect");                              B3.TODO_Expr_int()
      case MapUpdate(map, idxs, value) => 
        // The following checks are just there to see if there is ever a special case.
        idxs map {idx => if (!(idx.isInstanceOf[LocalVar] || idx.isInstanceOf[Const] || idx.isInstanceOf[FuncApp]))
                            info("INFO: MapUpdate with special idx type: ", idx.getClass.getName)
        }
        if (!map.isInstanceOf[LocalVar]) {
          info("INFO: MapUpdate of non-LocalVar-map. Instead of: ", map.getClass.getName)
        }
        // info("INFO: MapUpdate 'map[idxs] := x', where 'x.class' = ", value.getClass.getName)
        B3.FunctionCallExpr("MapUpdate", idxs.map(transformExpr) ++ Seq(transformExpr(value)))
      case Old(_) => info("TODO: Old");                                             B3.TODO_Expr_int()
      case RealConv(_) => info("TODO: RealConv");                                   B3.TODO_Expr_int()
      case RealLit(_) => info("TODO: RealLit");                                     B3.TODO_Expr_int()
      case TrueLit() => B3.Expr_BLiteral(true)
      case UnExp(unop, exp) => 
        unop match {
          case Not => B3.Expr_OperatorExpr(B3.Not, Seq(transformExpr(exp)))
          case Minus => B3.Expr_OperatorExpr(B3.Minus, Seq(transformExpr(exp)))
        }
    }
  }

  /** returns the type of the expression (as Type) */
  private def getTypeOfExpr(exp: Exp): Type = {
    exp match {
      case BinExp(left, binop, right) =>
        binop match {
          case LtCmp|LeCmp|GtCmp|GeCmp|EqCmp|NeCmp => Bool
          case And|Equiv|Implies|Or => Bool
          case Div => Real
          case IntDiv => Int
          case Add|Sub|Mul|Mod => Int //TODO: could also be Real, I think?
        }
      case CondExp(cond, thn, els) => getTypeOfExpr(thn)
      case Const(name) => 
        constIdentifierTypeMap.get(name) match {
          case Some(typeName) => NamedType(typeName) // Const never has freeTypVars
          case None => 
            info(f"ERROR: Using default type 'int' because couldn't find type name of constant: ", name)
            Int
        }        
      case Exists(_, _, _, _) => Bool
      case FalseLit() => Bool
      case Forall(_, _, _, _, _) => Bool
      case FuncApp(_, _, typ) => typ
      case GlobalVar(_, typ) => typ
      case IntLit(_) => Int
      case LocalVar(_, typ) => typ
      case MapSelect(map, idxs) => 
        // if (getTypeOfExpr(map) == "HeapType") {
        //   getTypeOfExpr(idxs(2))
        // }
        NamedType("TODO_MapSelect") // Name of this and MapUpdate depend on actual implementation of map. 'map' is an Expr, and it 
      case MapUpdate(map, idxs, value) =>           NamedType("TODO_MapUpdate") // might be slightly tricky to get the name for that, but that is a problem for future self. 
      case Old(oldexp) => getTypeOfExpr(oldexp)
      case RealConv(_) => Real
      case RealLit(_) => Real
      case TrueLit() => Bool
      case UnExp(Not, _) => Bool
      case UnExp(Minus, expr) => getTypeOfExpr(expr)
    }
  }

  /** 
   * returns the equivalend (raw) B3 Operator for Boogie's BinOp Operators. 
   * Note that > and >= are converted to < and <=, respectively. Therefore, the compared 
   * expressions must be switched for the translation to be correct!
  */
  def transformBinOp(op: BinOp): RawAst.Operator = {
    op match {
      case Add => B3.Add
      case And => B3.And
      case Div => B3.Div
      case EqCmp => B3.EqCmp
      case Equiv => B3.Equiv
      case GeCmp => B3.LeCmp // (B3 has no GeCmp)
      case GtCmp => B3.LtCmp // (B3 has no GtCmp)
      case Implies => B3.Implies
      case IntDiv => B3.IntDiv
      case LeCmp => B3.LeCmp
      case LtCmp => B3.LtCmp
      case Mod => B3.Mod
      case Mul => B3.Mul
      case NeCmp => B3.NeCmp
      case Or => B3.Or
      case Sub => B3.Sub
    }
  }

  // private def createVarDecl((v.name, v.typ, whereMap.get(v.name))): = {

  // }




// Parametric types:
//  type name, type parameters
//  Set to add all combinations of type parameters
//  functions to access subcombinations (e.g. all "Field A Bool"s) => can use Seq of Strings, placing null at positions where we want to use type parameters
private class AllTypesInB3() {
  /**
   * Stores all type parameter combinations that where found in the program.
   * Format is: map[typeName, (entryNr, nrOfTypeParameters, (SetOfSeq'sOf)TypeParameterCombinations)] (typeName is w/o parameters)
   */
  private val typeCollection = collection.mutable.Map[String, (Int, Int, collection.mutable.Set[Seq[String]])]()
  /** used to keep order in which types are declared */
  private var entryNr = 0

  /** Add new Type defined by a boogie node to the collection */
  def addType(typ: TypeDecl) = {
    val typName = typ.t.name
    val nrOfParams = typ.t.typVars.length
    val noCombosYet = collection.mutable.Set[Seq[String]]()
    typeCollection.put(typName, (entryNr, nrOfParams, noCombosYet))
    entryNr += 1
  }

  /** returns string to use in B3 for the type representing the given type and parameter combination */
  private def concreteName(name: String, combo: Seq[String]): String = {
      getNameFromParamTypeConstel(name, combo)
  }

  /** Returns a Seq of all type names of all existing type combos for parametric types and all non-parametric types.
   * They are in the same order as in the Boogie AST.
   */
  def declareAllTypes(): Seq[String] = {
    val unsortedResult = (typeCollection map {typEntry => typEntry match {
      case (name, (nr, 0, _)) => Seq((nr, name)) // <- non-parametric type
      case (name, (nr, _, set)) => set.toSeq map {combo => (nr, concreteName(name, combo))}
    }}).toSeq.flatten
    //sort by (entry)nr and remove it afterwards
    unsortedResult.sortBy(_._1) map {_._2}
  }

  /** Returns a Seq of B3 TypeDecl nodes for all existing type combos for parametric types and all non-parametric types.
   * The TypeDecls are in the same order as in the Boogie AST.
   */
  def declareAllB3Types(): Seq[RawAst.TypeDecl] = {
    declareAllTypes() map {x => B3.TypeDecl(x)}
  }


  /**
    * Adds the parameter combination as an existing combination. 
    *
    * @param combo The parameter combination to add. The type must exist in the collection, with matching nr of parameters, 
    *  and all entries must correspond to an existing type (includes other parametrized types with a corresponding entry). 
    */
  def addCombo(name: String, combo: Seq[String]): Unit = {
    val typeInfo = typeCollection(name)
    if (combo.length != typeInfo._2) {
      sys.error(f"Tried to add $combo (length=${combo.length}) to $name (paramNr=${typeInfo._2}). Length doesn't match!")
    }
    typeInfo._3 += combo
  }

  /**
    * Adds the parameter combination of given typ if and only if it is a parametric type and all parameters are concrete
    * (i.e. no free type variables). If the given type parameters are also parametric types, they are also added.
    *
    * @param typ The typ whose combination to (possibly) add. The type must exist in the collection, with matching
    * nr of parameters, and all entries must correspond to an existing type (includes other parametrized types with
    * a corresponding entry). 
    */
  def possiblyAddCombos(typ: Type): Unit = {
    typ match {
      case Bool | Int | Real | MapType(_,_,_) | NamedType(_, Seq()) | TypeVar(_) => None // types without parameters don't matter
      case NamedType(name, typVars) => 
        if (typ.freeTypeVars.length == 0) { // types with freeTypeVars don't matter
            val typNames = typVars map {getNameFromTyp(_)}
          addCombo(name, typNames)
          concreteName(name, typNames)
        }
        typVars map {possiblyAddCombos(_)} // but single type parameters may still consist of valid combos!
        // (the reason for that is that e.g. T1 Int doesnt exist, but T1 T1 Int does and there is an axiom with 
        // "forall <A> x: T1 A, y: A :: ..." => T1 Int is also needed, otherwise other T1 A axioms that would cover 
        // properties for y there might be missing)
    }
  }

  /**
    * Returns all combos for the given type.
    *
    * @param name base-name of type (without type parameters)
    * @return Seq of all existing combos
    */
  def getAllCombos(name: String): Seq[Seq[String]] = {
    val typeInfo = typeCollection(name)    
    typeInfo._3.toSeq
  }

  /**
    * Returns type names of all combos for the given type that match the given pattern.
    *
    * @param name base-name of type (without type parameters)
    * @param paramPattern Seq of parameters, where specific names are them as string, but parameters are null.
    *   E.g. for "Field A Bool" (assuming A is a parameter and not a type) you must use paramPattern = [null, "Bool"]
    * @return Seq of existing combos that match the given pattern
    */
  def getCombosOfPattern(name: String, paramPattern: Seq[String]): Seq[Seq[String]] = {
    val typeInfo = typeCollection(name)
    if (paramPattern.length != typeInfo._2) {
      sys.error(f"Tried to match pattern $paramPattern (length=${paramPattern.length}) to $name (paramNr=${typeInfo._2}). Length doesn't match!")
    }

    typeInfo._3.toSeq filter {x => x.zip(paramPattern).forall{
      case (null, _) => true
      case (_, null) => true
      case (a, b) => a == b
    }}
  }


    /** Collects and adds all combos in the program. This only includes types existing in the Boogie AST.
     * Any "combos" added by BoogieToB3Transformer (e.g. the type for the "Field Int Int"-Heap-submap) must be
     * added through use of addCombo/possiblyAddCombos. Also handles the collection of ConstID=>TypeName info.
   */
    def collectTypeCombos(prog: Program): Unit = {
      val flatDeclSeq = flattenedDecl(prog.decls)
      val boogieTypeDecls = flatDeclSeq collect {case typDecl: TypeDecl => typDecl}
      boogieTypeDecls map {allTypeDecls.addType(_)}
      prog.decls map {decl => collectTypeCombosFromDecl(decl)}
  }

  /** Handles 'Decl's for collectTypeCombos (see there) */
  private def collectTypeCombosFromDecl(decl: Decl): Unit = {
    decl match {
      case Axiom(exp) => collectTypeCombosFromExp(exp)
      case CommentedDecl(s, d, size, nLines) => d map {decl => collectTypeCombosFromDecl(decl)}
        case ConstDecl(name, typ, unique) => 
          val typName = getNameFromTyp(typ)
          constIdentifierTypeMap.put(name, typName)
          possiblyAddCombos(typ)
      case DeclComment(s) => None
      case Func(name, args, typ, attributes) =>
        possiblyAddCombos(typ)
        args map {lvdef => collectTypeCombosFromLocalVarDecl(lvdef)}
        // TODO: attributes
      case GlobalVarDecl(name, typ) => possiblyAddCombos(typ)
      case LiteralDecl(boogieString) => None
      case Procedure(name, ins, outs, body) => 
        ins map {lvdef => collectTypeCombosFromLocalVarDecl(lvdef)}
        outs map {lvdef => collectTypeCombosFromLocalVarDecl(lvdef)}
        collectTypeCombosFromStmt(body)
      case TypeAlias(name, definition) => None
        case TypeDecl(_) => None // (always 'T1' or 'T2 A B', never 'T2 Int Int')
    }
  }

  /** Handles 'LocalVarDecl's for collectTypeCombos (see there) */
  private def collectTypeCombosFromLocalVarDecl(lvdef: LocalVarDecl): Unit = {
    possiblyAddCombos(lvdef.typ)
    // TODO: lvdef.where
  }

  /** Handles 'Stmt's for collectTypeCombos (see there) */
  private def collectTypeCombosFromStmt(stmt: Stmt): Unit = {
    stmt match {
      case Goto(dests) => None
      case AssertImpl(exp, error) => 
        collectTypeCombosFromExp(exp)
      case Assign(lhs, rhs) =>
        collectTypeCombosFromExp(lhs)
        collectTypeCombosFromExp(rhs)
      case Assume(exp) => 
        collectTypeCombosFromExp(exp)
      case Comment(s) => None 
      case CommentBlock(_, stmt) => 
        collectTypeCombosFromStmt(stmt)
      case HavocImpl(vars) => 
        vars map {vr => collectTypeCombosFromExp(vr)}
      case If(cond, thn, els) =>
        collectTypeCombosFromExp(cond)
        collectTypeCombosFromStmt(thn)
        collectTypeCombosFromStmt(els)
      case Label(lbl) => None
      case LocalVarWhereDecl(name, where) => None //types of local vars are given by the args of e.g. procedure 
      case NondetIf(thn, els) => 
        collectTypeCombosFromStmt(thn)
        collectTypeCombosFromStmt(els)
      case Seqn(stmts) =>
        stmts map {stm => collectTypeCombosFromStmt(stm)}
    }
  }

  /** Handles 'Exp's for collectTypeCombos (see there) */
  private def collectTypeCombosFromExp(expr: Exp): Unit = {
    expr match {
      case BinExp(left, binop, right) =>
        collectTypeCombosFromExp(left)
        collectTypeCombosFromExp(right)
      case CondExp(cond, thn, els) => 
        collectTypeCombosFromExp(cond)
        collectTypeCombosFromExp(thn)
        collectTypeCombosFromExp(els)
      case Const(name) => None // type is defined on instantiation
      case Exists(vars, triggers, exp, weight) => 
        vars map {lvar => collectTypeCombosFromLocalVarDecl(lvar)}
        triggers map {trigger => trigger.exps map {exp => collectTypeCombosFromExp(exp)}}
        collectTypeCombosFromExp(exp)
      case FalseLit() => None
      case Forall(vars, triggers, exp, typeVars, weight) => 
        vars map {lvar => collectTypeCombosFromLocalVarDecl(lvar)}
        triggers map {trigger => trigger.exps map {exp => collectTypeCombosFromExp(exp)}}
        collectTypeCombosFromExp(exp)
      case FuncApp(name, args, typ) => 
        args map {exp => collectTypeCombosFromExp(exp)}
        possiblyAddCombos(typ)
      case GlobalVar(name, typ) => 
        possiblyAddCombos(typ)
      case IntLit(_) => None
      case LocalVar(name, typ) => 
        possiblyAddCombos(typ)
      case MapSelect(map, idxs) => 
        collectTypeCombosFromExp(map)
        idxs map {exp => collectTypeCombosFromExp(exp)}
      case MapUpdate(map, idxs, value) => 
        collectTypeCombosFromExp(map)
        idxs map {exp => collectTypeCombosFromExp(exp)}
        collectTypeCombosFromExp(value)
      case Old(oldexp) => 
        collectTypeCombosFromExp(oldexp)
      case RealConv(_) => None
      case RealLit(_) => None
      case TrueLit() => None
      case UnExp(unop, exp) => 
        collectTypeCombosFromExp(exp)
    }
  }
}
}
