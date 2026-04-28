package viper.carbon.b3
import viper.carbon.b3.{B3Helper => B3}
import viper.carbon.boogie._


/**
 * An implementation for transformers to transform Boogie AST -> B3 AST (RawAst).
 * Cannot reuse the boogie-Transformer used by Carbon, because B3 AST-nodes don't have a shared ancestor like "Node".
 */
object BoogieToB3Transformer {
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

  /** Transform Boogie Procedure -> raw B3 Procedure */
  private def transformProcedure(proc: Procedure): RawAst.Procedure = {
    // Define variable declarations for all undeclared variables in the body, to be inserted at start of transformed procedure body. 
    val undecl = proc.body.undeclLocalVars.filter(v1 => (proc.ins ++ proc.outs).forall(v2 => v2.name != v1.name)) // (<- stolen from boogie.PrettyPrinter)
    // Only variables used in the body of the var decl are in scope => need to define procedure bode and each var decl as body of previous var decl
    val varDeclarations = undecl.foldRight(transformStatement(proc.body))((l, r) => B3.Stmt_VarDecl(l.name.name, r))
    // println("DEBUG: ========> " + undecl.size)
    // Use first ("outermost") var decl as Procedure body. (TODO: check if it is ever possible to have an empty body -> B3.Option_None)
    val b3ProcBody = B3.Option_Some(varDeclarations)
    
    // proc.body match {
    //   case Seqn(seq) => println("DEBUG: " + seq.foreach(d => println(d.getClass.getName)))
    //   case _ => println("DEBUG: " + proc.body.getClass.getName)
    // }

    // finally, creating raw B3 Procedure
    B3.Procedure(name = proc.name.name,                     // TODO: Make sure that that name is valid! (proc.name contains different names, see definition of Identifier in boogie.scala)
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
      case Seqn(stmtSeq) => {
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
      }
      case anyStmt => anyStmt
    }
  }


  private def transformStatement(stmt: Stmt): RawAst.Stmt = {
    println("DEBUG: transformStatement(x), where x has type " + stmt.getClass.getName)
    stmt match {
      case _: Goto => println("TODO: Goto");                                        B3.TODO_Stmt()
      case AssertImpl(exp, error) => B3.Stmt_Assert(transformExpr(exp), error.readableMessage)
      case Assign(lhs, rhs) => {
        (lhs, rhs) match {
          case (LocalVar(identif, _), exp: Exp) => {
          println("DEBUG: Assign lhs: name = " + identif.name + " type = " + lhs.getClass.getName)
            B3.Stmt_Assign(identif.name, transformExpr(rhs))
          }
          case (_: GlobalVar, _) => sys.error("TODO: GlobalVar")
          case _ => sys.error("FAIL: Expected lhs of Assign stmt to be LocalVar (or GlobalVar), but it was " + lhs.getClass.getName)
        }
        // B3.TODO_Stmt()
      }
      case _: Assume => println("TODO: Assume");                                    B3.TODO_Stmt()
      case _: Comment => println("FAIL: Comment stmts should be pre-removed!!!");   B3.TODO_Stmt()
      case _: CommentBlock => println("TODO: CommentBlock");                        B3.TODO_Stmt()
      case _: HavocImpl => println("TODO: HavocImpl");                              B3.TODO_Stmt()
      case _: If => println("TODO: If");                                            B3.TODO_Stmt()
      case _: Label => println("TODO: Label");                                      B3.TODO_Stmt()
      case _: LocalVarWhereDecl => println("TODO: LocalVarWhereDecl");              B3.TODO_Stmt()
      case _: NondetIf => println("TODO: NondetIf");                                B3.TODO_Stmt()
      case seqn: Seqn => {
        // We always create a Stmt_Block here, but we first have to eliminate all unneccessairy Seqn-nestings, and ignore empty Seqn (Comment stmts dont count) 
        val unpackedStmtSeq = unpackStmtBranch(seqn) match {
          case Seqn(seq) => seq
          case Comment(_) => Seq()
          case stmt => Seq(stmt) 
        }
        B3.Stmt_Block(unpackedStmtSeq map transformStatement)
      }
    }
  }

   // TODO: actually implement this method
  private def transformExpr(exp: Exp): RawAst.Expr = {
    println("DEBUG: +++ transformExpr(x), where x has type " + exp.getClass.getName)
    
    exp match {
      case BinExp(left, binop, right) => {
        // (left: Exp, binop: BinOp, right: Exp)
        binop match {
          case Add => "ok"
          case And => "ok"
          case Div => "ok"
          case EqCmp => "ok"
          case Equiv => "ok"
          case GeCmp => "ok"
          case GtCmp => "ok"
          case Implies => "ok"
          case IntDiv => "ok"
          case LeCmp => "ok"
          case LtCmp => "ok"
          case Mod => "ok"
          case Mul => "ok"
          case NeCmp => "ok"
          case Or => "ok"
          case Sub => "ok"
        }
        println("TODO: BinExp"); 
        B3.TODO_Expr_int()
      }
      case CondExp(_, _, _) => println("TODO: CondExp");                            B3.TODO_Expr_int()
      case Const(_) => println("TODO: Const");                                      B3.TODO_Expr_int()
      case Exists(_, _, _, _) => println("TODO: Exists");                           B3.TODO_Expr_int()
      case FalseLit() => B3.Expr_BLiteral(false)
      case Forall(_, _, _, _, _) => println("TODO: Forall");                        B3.TODO_Expr_int()
      case FuncApp(_, _, _) => println("TODO: FuncApp");                            B3.TODO_Expr_int()
      case GlobalVar(_, _) => println("TODO: GlobalVar");                           B3.TODO_Expr_int()
      case IntLit(i) => B3.Expr_ILiteral(i)
      case LocalVar(_, _) => println("TODO: LocalVar");                             B3.TODO_Expr_int()
      case MapSelect(_, _) => println("TODO: MapSelect");                           B3.TODO_Expr_int()
      case MapUpdate(_, _, _) => println("TODO: MapUpdate");                        B3.TODO_Expr_int()
      case Old(_) => println("TODO: Old");                                          B3.TODO_Expr_int()
      case RealConv(_) => println("TODO: RealConv");                                B3.TODO_Expr_int()
      case RealLit(_) => println("TODO: RealLit");                                  B3.TODO_Expr_int()
      case TrueLit() => B3.Expr_BLiteral(true)
      case UnExp(_, _) => println("TODO: UnExp");                                   B3.TODO_Expr_int()
    }
  }

  // private def createVarDecl((v.name, v.typ, whereMap.get(v.name))): = {

  // }

}