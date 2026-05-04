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

  /** BoogieNameGenerator instance. */ 
  private val names = new BoogieNameGenerator() // TODO: Check if we need a B3NameGenerator

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
  val DEBUG_MODE = 1 // 0 = ignore, 1 = collect (+ print later), 2 = collect & print (+ print later)
  val debug_infoPlus = collection.mutable.Map[String, Seq[String]]()
  val debug_info = collection.mutable.Set[String]()
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
      case Axiom(exp) => usedMapping += "Axiom"
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
      case TypeDecl(t) => usedMapping += "TypeDecl"
    }}
    println(usedMapping)

    }
    // We should define these somewhere else LATER and then "import" from there 
    val alwaysIncludeFcts = Seq(B3.Function("AssumeFunctionsAbove", Seq(), "int"),
                                B3.Function("AssumePermUpperBound", Seq(), "bool"),
                                B3.Function("state", Seq(B3.FParameter("heap", "HeapType"), B3.FParameter("mask", "MaskType")), "bool"))
    val alwaysIncludeTyps = Seq("HeapType", "MaskType")
    // DEVELOPMENT ^^^

    // Create B3 Program using the B3 version of the correct (Boogie) Decl nodes
    B3.Program(types = alwaysIncludeTyps ++ Seq(),        //TODO
               taggers = Seq(),                           //TODO
               functions = alwaysIncludeFcts ++ flatDeclSeq.collect({case func: Func => transformFunction(func)}),
               axioms = Seq(),                            //TODO
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

  /** flattens the sequence by removing all CommentedDecl, but keeping all Decl it contains */
  private def flattenedDecl(decls: Seq[Decl]): Seq[Decl] = {
    decls flatMap {
      case commDecl: CommentedDecl => flattenedDecl(commDecl.d)
      case decl => Seq(decl)
    }
  } 

  /** Returns the type name from a Boogie Type */ 
  private def getNameFromTyp(typ: Type): String = {
    typ match {
      case Bool => "bool"
      case Int => "int"
      case Real =>              sys.error("TODO: Real Type")
      case MapType(_, _, _) =>  sys.error("TODO: MapType")
      case NamedType(name, typVars) =>   info("TODO: NamedType; this has name = ", name); name
      case TypeVar(_) =>        sys.error("TODO: TypeVar")
    }
  }

  /** Transform Boogie Procedure -> raw B3 Procedure */
  private def transformProcedure(proc: Procedure): RawAst.Procedure = {
    // collect all where clauses (important for declaring and havoc-ing these variables)
    whereMap.clear()
    proc.body visit {
      case LocalVarWhereDecl(idn, where) =>
        whereMap.put(idn, where)
    }
    // Define variable declarations for all undeclared variables in the body, to be inserted at start of transformed procedure body. 
    val undecl = proc.body.undeclLocalVars.filter(v1 => (proc.ins ++ proc.outs).forall(v2 => v2.name != v1.name)) // (<- stolen from boogie.PrettyPrinter)
    // Only variables used in the body of the var decl are in scope => need to define procedure bode and each var decl as body of previous var decl
    val varDeclarations = undecl.foldRight(transformStatement(proc.body))((l, r) => B3.Stmt_VarDecl(l.name, r, getNameFromTyp(l.typ))) //TODO: add assume [expr] for where [expr] VarDecls (i.e. for all in whereMap)
    // info("DEBUG: ========> ", undecl.size)
    // Use first ("outermost") var decl as Procedure body. (TODO: check if it is ever possible to have an empty body -> B3.Option_None)
    val b3ProcBody = B3.Option_Some(varDeclarations)
    
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
   * Unpacks CommentBlock stmts and nested Seqs, but output is functionally the same as input.
   * Returns Comment("Useless!") if no useful stmts. It is the caller's reponsibility to handle this correctly. 
   */
  private def unpackStmtBranch(stmt: Stmt): Stmt = {
    // We cannot use comments, so we try to remove Comment stmts and (iteratively) take the Stmt contained in CommentBlock stmts
    // Seqn stmts are only useful if they contain multiple elements (not counting any non-useful elements)

    // TODO: PrettyPrinter also ignores LocalVarWhereDecl stmts; check what's up with that!
    // println(""DEBUG: unpackStmtBranch stmt type: " + stmt.getClass.getName)
    stmt match {
      case commBlock: CommentBlock => unpackStmtBranch(commBlock.stmt) 
      case Seqn(stmtSeq) =>
        val seq = stmtSeq map unpackStmtBranch filter {
          case Comment(_) => false
          case Seqn(Seq()) => false
          case _ => true
        }
        if (seq.size == 0) {
          Comment("Useless!")
        } else if (seq.size == 1) {
          seq.head
        } else {
          Seqn(seq)
        }
      case anyStmt => anyStmt
    }
  }


  private def transformStatement(stmt: Stmt): RawAst.Stmt = {
    info("DEBUG: transformStatement(x), where x has type ", stmt.getClass.getName)
    stmt match {
      case _: Goto => info("LATER: Goto");                                          B3.LATER_Stmt()
      case AssertImpl(exp, error) => B3.Stmt_Assert(transformExpr(exp), error.readableMessage)
      case Assign(lhs, rhs) =>
        lhs match {
          case LocalVar(identif, typ) =>
            // info("DEBUG: Assign lhs: (name, type) = ", "(" + useIdent(identif) + ", " + lhs.getClass.getName + ")")
            B3.Stmt_Assign(identif, transformExpr(rhs))
          case GlobalVar(_, typ) => info("TODO: (assign) GlobalVar of type: ", getNameFromTyp(typ));      
                                                                                    B3.TODO_Stmt()
          case _ => sys.error("FAIL: Expected lhs of Assign stmt to be LocalVar (or GlobalVar), but it was " + lhs.getClass.getName)
        }
      case Assume(exp) => B3.Stmt_Assume(transformExpr(exp))
      case _: Comment => info("FAIL: Comment stmts should be pre-removed!!! Inserted empty stmt block instead"); B3.Stmt_Block(Seq())
      case CommentBlock(_, stmt) => transformStatement(stmt)
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
      case NondetIf(thn, els) => B3.Stmt_Choose(Seq(thn, els) map transformStatement) //QUEST: we could check if thn or els is another NondetIf and then add all substatements into a single Stmt_Choose
      case seqn: Seqn => 
        // We always create a Stmt_Block here, but we first have to eliminate all unneccessairy Seqn-nestings, and ignore empty Seqn (Comment stmts dont count) 
        val unpackedStmtSeq = unpackStmtBranch(seqn) match {
          case Seqn(seq) => seq
          case Comment(_) => Seq()
          case stmt => Seq(stmt) 
        }
        B3.Stmt_Block(unpackedStmtSeq map transformStatement) // TODO: improve block-removal even more. I think mainly flatten Seqn stmts in CommentBlock stmts
    }
  }

   // TODO: actually implement this method
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
      case Exists(_, _, _, _) => info("TODO: Exists");                              B3.TODO_Expr_int()
      case FalseLit() => B3.Expr_BLiteral(false)
      case Forall(_, _, _, _, _) => info("TODO: Forall");                           B3.TODO_Expr_int()
      case FuncApp(name, args, _) => 
        args.map(x => info("FuncApp args: ", x.toString))
        B3.FunctionCallExpr(name, args map transformExpr)
      case GlobalVar(_, typ) => info("TODO: GlobalVar of type: ", getNameFromTyp(typ)); B3.TODO_Expr_int()
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