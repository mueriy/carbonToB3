package viper.carbon.b3
import viper.carbon.b3.{B3Adapter => B3}
import viper.carbon.boogie._
import language.implicitConversions

/**
 * An implementation for transformers to transform Boogie AST -> B3 AST (RawAst).
 * Cannot reuse the boogie-Transformer used by Carbon, because B3 AST-nodes don't have a shared ancestor like "Node".
 */
object BoogieToB3Transformer {
  // Uses the following implicits: useIdent (Identifier -> String)

  // IDENTIFIER UNIQUENESS (and other properties; stolen from PrettyPrinter; useIdent <-> ident2doc)
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
  implicit def useIdent(i: Identifier): String = {
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


  // DEVELOPMENT & DEBUGGING
  // settings
  private val UNPACK_SEQN = true // true = easier to ready code, false = easier to compare to PrettyPrinted code (see method: uncomment)
  private val DEBUG_MODE = 1 // 0 = ignore, 1 = collect (+ print later), 2 = collect & print (+ print later)
  private val USE_CHECK = true

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
   * 
   * @param prog An (extended) boogie AST (Program)
   * @return A raw B3 AST
   */
  def transformProgram(prog: Program): RawAst.Program = {
    // (Ignore header field - we cannot convert comments)
    // Eliminate all CommentedDecl-s
    val flatDeclSeq = flattenedDecl(prog.decls)

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
      case GlobalVarDecl(name, typ) => usedMapping += "GlobalVarDecl"
      case LiteralDecl(boogieString) => usedMapping += "LiteralDecl"
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
    val alwaysIncludeFcts = Seq(B3.Function("AssumeFunctionsAbove", Seq(), "int"),
                                B3.Function("AssumePermUpperBound", Seq(), "bool"),
                                B3.Function("Heap", Seq(), "HeapType"),
                                B3.Function("EmptyFrame", Seq(), "FrameType"),
                                B3.Function("Mask", Seq(), "MaskType"),
                                B3.Function("ZeroMask", Seq(), "MaskType"),
                                B3.Function("dummyFunction", Seq(B3.FParameter("x", "int")), "bool"),
                                B3.Function("state", Seq(B3.FParameter("heap", "HeapType"), B3.FParameter("mask", "MaskType")), "bool"))
    val alwaysIncludeTyps = Seq("MaskType", "HeapType", "Perm", "Seq", "Field", "PMaskType", "FrameType", "Ref")
    // DEVELOPMENT ^^^

    // Type declaration
    // TODO: NamedType with typVars (= Parametric types)
    val normalTyps = flatDeclSeq.collect({case TypeDecl(NamedType(name, Seq())) => B3.TypeDecl(name)})
    val paramTyps = flatDeclSeq.collect({case TypeDecl(NamedType(name, seq)) if seq != Seq() => (Seq(name) ++ seq.map{getNameFromTyp(_)}).flatten})
    // val typeAliases = :TODO && TODO: use paramTyps
    // Boogie constants
    // We use uninterpreted nullary functions instead of constants, but these always return the same value (= have constant value)
    // For constants with the unique tag we can use B3 tags, as all functions tagged with the same tag return pairwise distinct values (same as 'unique' const)
    val const_tags = collection.mutable.Set[(String, String)]()
    val constFcts = flatDeclSeq.collect({
      case ConstDecl(name, typ, false) => B3.Function(name, Seq(), getNameFromTyp(typ))
      case ConstDecl(name, typ, true) => 
        val typName = getNameFromTyp(typ)
        val tagName = "Const_Tag_"+typName
        const_tags += ((tagName, typName))
        B3.Function(name, Seq(), typName, tagName)
    })
    val constTaggers = const_tags.map({case ((tag, typ)) => B3.Tagger(tag, typ)}).toSeq


    // Create B3 Program using the B3 version of the correct (Boogie) Decl nodes
    B3.Program(signatureTypes = Seq[String](), 
               domains = Seq[RawAst.Domain](),
               types = alwaysIncludeTyps ++ normalTyps,
               taggers = constTaggers,
               functions = alwaysIncludeFcts ++ constFcts ++ flatDeclSeq.collect({case func: Func => transformFunction(func)}),
               axioms = flatDeclSeq.collect({case ax: Axiom => transformAxiom(ax)}),
               procedures = flatDeclSeq.collect({case proc: Procedure => transformProcedure(proc)}))
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
    // Forall, MaybeForall, (DefaultStateModule->) FuncApp(Identifier(isGoodState), stateExps, Bool), (DefaultHeapModule->) UnExp(Not, FuncApp) (DefaultDomainModule->) any Exp
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
      case Real =>              sys.error("TODO: Real Type")
      case MapType(_, _, _) =>  sys.error("TODO: MapType")
      case NamedType(name, Seq()) => name
      case NamedType(name, typVars) => info("TODO: NamedType: ", name + " " + typVars.map(getNameFromTyp(_)).mkString(" ")); name
      case TypeVar(_) =>        sys.error("TODO: TypeVar")
    }
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
                 parameters = transformPParameters(proc.ins, proc.outs),
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
    inPPar ++ outPPar
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
          case LocalVar(identif, typ) =>
            // info("DEBUG: Assign lhs: (name, type) = ", "(" + useIdent(identif) + ", " + lhs.getClass.getName + ")")
            B3.Stmt_Assign(identif, transformExpr(rhs))
          case GlobalVar(_, typ) => info("TODO: Assign to GlobalVar of type: ", getNameFromTyp(typ));      
                                                                                    B3.TODO_Stmt()
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
        // cond match {
        //   case LocalVar(name, _) => println("DEBUG: ---------> " + cond.getClass.getName + " and " + useIdent(name))
        //   case _ => println("DEBUG: ---------> " + cond.getClass.getName)
        // }
        B3.Stmt_If(transformExpr(cond), transformStatement(thn), transformStatement(els)) //QUEST: for directly nested if's we could try whether If-case is more efficient
      case _: Label => info("LATER: Label");                                     B3.LATER_Stmt() //Carbon does not generate label: ... return, so this is only needed for goto
      case _: LocalVarWhereDecl => info("TODO: LocalVarWhereDecl");              B3.TODO_Stmt()
      case NondetIf(thn, els) => B3.Stmt_Choose(Seq(thn, els).map(transformStatement(_))) //QUEST: we could check if thn or els is another NondetIf and then add all substatements into a single Stmt_Choose
      case Seqn(stmts) =>
        B3.Stmt_Block(stmts.map(s => transformStatement(s, false)))
    }
  }
  /** temporary dummy to use instead of actually implementing global variables (= only needed for impure features) */
  private def TODO_GlobalVar(name: Identifier): RawAst.Expr_FunctionCallExpr = {
    B3.FunctionCallExpr(name, Seq())
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
      case FuncApp(name, args, _) => B3.FunctionCallExpr(name, args map transformExpr)
      case GlobalVar(name, typ) => 
        info("(TODO): GlobalVar (name, type): ", "("+name.name+": "+getNameFromTyp(typ)+")")
        TODO_GlobalVar(name)
      case IntLit(i) => B3.Expr_ILiteral(i)
      case LocalVar(name, _) => B3.Expr_IdExpr(name)
      case MapSelect(_, _) => info("TODO: MapSelect");                              B3.TODO_Expr_int()
      case MapUpdate(_, _, _) => info("TODO: MapUpdate");                           B3.TODO_Expr_int()
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

  /** returns the equivalend (raw) B3 Operator for Boogie's BinOp Operators */
  def transformBinOp(op: BinOp): RawAst.Operator = {
    op match {
      case Add => B3.Add
      case And => B3.And
      case Div => B3.Div
      case EqCmp => B3.EqCmp
      case Equiv => B3.Equiv
      case GeCmp => B3.LeCmp
      case GtCmp => B3.LtCmp
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

}