package viper.carbon.b3
import viper.carbon.b3.{B3Helper => B3}
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

    // Create B3 Program using the B3 version of the correct (Boogie) Decl nodes
    B3.Program(types = Seq(),       //TODO
               taggers = Seq(),     //TODO
               functions = Seq(),   //TODO
               axioms = Seq(),      //TODO
               procedures = flatDeclSeq.collect({case proc: Procedure => transformProcedure(proc)}))
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
      case NamedType(_, _) =>   sys.error("TODO: NamedType")
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
    // println("DEBUG: ========> " + undecl.size)
    // Use first ("outermost") var decl as Procedure body. (TODO: check if it is ever possible to have an empty body -> B3.Option_None)
    val b3ProcBody = B3.Option_Some(varDeclarations)
    
    // proc.body match {
    //   case Seqn(seq) => println("DEBUG: " + seq.foreach(d => println(d.getClass.getName)))
    //   case _ => println("DEBUG: " + proc.body.getClass.getName)
    // }

    // finally, creating raw B3 Procedure
    B3.Procedure(name = proc.name,
                 parameters = Seq[RawAst.PParameter](),     // TODO
                 pre = Seq[RawAst.AExpr](),     // No data for these, but also empty in Boogie
                 post = Seq[RawAst.AExpr](),    // No data for these, but also empty in Boogie
                                                // TODO-later: Boogie additionally has "modifies", which is used there for Heap stuff. Need to find workaround
                 body = b3ProcBody)
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
    println("DEBUG: transformStatement(x), where x has type " + stmt.getClass.getName)
    stmt match {
      case _: Goto => println("LATER: Goto");                                        B3.LATER_Stmt()
      case AssertImpl(exp, error) => B3.Stmt_Assert(transformExpr(exp), error.readableMessage)
      case Assign(lhs, rhs) =>
        lhs match {
          case LocalVar(identif, typ) =>
            // println("DEBUG: Assign lhs: name = " + useIdent(identif) + " type = " + lhs.getClass.getName)
            B3.Stmt_Assign(identif, transformExpr(rhs))
          case _: GlobalVar =>                                                      sys.error("TODO: GlobalVar")
          case _ => sys.error("FAIL: Expected lhs of Assign stmt to be LocalVar (or GlobalVar), but it was " + lhs.getClass.getName)
        }
      case Assume(exp) => B3.Stmt_Assume(transformExpr(exp))
      case _: Comment => println("FAIL: Comment stmts should be pre-removed!!! Inserted empty stmt block instead"); B3.Stmt_Block(Seq())
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
      case _: Label => println("TODO: Label");                                      B3.LATER_Stmt() //Carbon does not generate label: ... return, so this is only needed for goto
      case _: LocalVarWhereDecl => println("TODO: LocalVarWhereDecl");              B3.TODO_Stmt()
      case NondetIf(thn, els) => B3.Stmt_Choose(Seq(thn, els) map transformStatement) //QUEST: we could check if thn or els is another NondetIf and then add all substatements into a single Stmt_Choose
      case seqn: Seqn => 
        // We always create a Stmt_Block here, but we first have to eliminate all unneccessairy Seqn-nestings, and ignore empty Seqn (Comment stmts dont count) 
        val unpackedStmtSeq = unpackStmtBranch(seqn) match {
          case Seqn(seq) => seq
          case Comment(_) => Seq()
          case stmt => Seq(stmt) 
        }
        B3.Stmt_Block(unpackedStmtSeq map transformStatement)
    }
  }

   // TODO: actually implement this method
  private def transformExpr(exp: Exp): RawAst.Expr = {
    println("DEBUG: +++ transformExpr(x), where x has type " + exp.getClass.getName)
    // B3 and Boogie operators seem to have the same associativity.
    // B3 does not have the Boogie operators ++ and <:, but both not used by Carbon. 
    // B3 does also not support the > and >= operators, which means that we swap the left and right expr and use < and <= instead
    // B3 does have the <== operator, but we dont use it, so we dont care about its associativity. 
    
    exp match {
      case BinExp(left, binop, right) =>
        binop match {
          case Add => B3.Expr_OperatorExpr(B3.Add, Seq(left, right) map transformExpr)
          case And => B3.Expr_OperatorExpr(B3.And, Seq(left, right) map transformExpr)
          case Div => B3.Expr_OperatorExpr(B3.Div, Seq(left, right) map transformExpr) // TODO: check if we can really use this
          case EqCmp => B3.Expr_OperatorExpr(B3.EqCmp, Seq(left, right) map transformExpr)
          case Equiv => B3.Expr_OperatorExpr(B3.Equiv, Seq(left, right) map transformExpr)
          case GeCmp => B3.Expr_OperatorExpr(B3.LeCmp, Seq(right, left) map transformExpr) //we must use right <= left instead of left >= right
          case GtCmp => B3.Expr_OperatorExpr(B3.LtCmp, Seq(right, left) map transformExpr) //we must use right < left instead of left > right
          case Implies => B3.Expr_OperatorExpr(B3.Implies, Seq(left, right) map transformExpr)
          case IntDiv => B3.Expr_OperatorExpr(B3.IntDiv, Seq(left, right) map transformExpr)
          case LeCmp => B3.Expr_OperatorExpr(B3.LeCmp, Seq(left, right) map transformExpr)
          case LtCmp => B3.Expr_OperatorExpr(B3.LtCmp, Seq(left, right) map transformExpr)
          case Mod => B3.Expr_OperatorExpr(B3.Mod, Seq(left, right) map transformExpr)
          case Mul => B3.Expr_OperatorExpr(B3.Mul, Seq(left, right) map transformExpr)
          case NeCmp => B3.Expr_OperatorExpr(B3.NeCmp, Seq(left, right) map transformExpr)
          case Or => B3.Expr_OperatorExpr(B3.Or, Seq(left, right) map transformExpr)
          case Sub => B3.Expr_OperatorExpr(B3.Sub, Seq(left, right) map transformExpr)
        }
      case CondExp(cond, thn, els) => B3.Expr_OperatorExpr(B3.CondExp, Seq(cond, thn, els) map transformExpr)
      case Const(_) => println("TODO: Const");                                      B3.TODO_Expr_int()
      case Exists(_, _, _, _) => println("TODO: Exists");                           B3.TODO_Expr_int()
      case FalseLit() => B3.Expr_BLiteral(false)
      case Forall(_, _, _, _, _) => println("TODO: Forall");                        B3.TODO_Expr_int()
      case FuncApp(_, _, _) => println("TODO: FuncApp");                            B3.TODO_Expr_int()
      case GlobalVar(_, _) => println("TODO: GlobalVar");                           B3.TODO_Expr_int()
      case IntLit(i) => B3.Expr_ILiteral(i)
      case LocalVar(name, _) => B3.Expr_IdExpr(name)  // TODO: check whats up with the second field (typ: Type)!
      case MapSelect(_, _) => println("TODO: MapSelect");                           B3.TODO_Expr_int()
      case MapUpdate(_, _, _) => println("TODO: MapUpdate");                        B3.TODO_Expr_int()
      case Old(_) => println("TODO: Old");                                          B3.TODO_Expr_int()
      case RealConv(_) => println("TODO: RealConv");                                B3.TODO_Expr_int()
      case RealLit(_) => println("TODO: RealLit");                                  B3.TODO_Expr_int()
      case TrueLit() => B3.Expr_BLiteral(true)
      case UnExp(unop, exp) => 
        unop match {
          case Not => B3.Expr_OperatorExpr(B3.Not, Seq(transformExpr(exp)))
          case Minus => B3.Expr_OperatorExpr(B3.Minus, Seq(transformExpr(exp)))
        }
    }
  }

  // private def createVarDecl((v.name, v.typ, whereMap.get(v.name))): = {

  // }

}