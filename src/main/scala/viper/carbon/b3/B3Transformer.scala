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
    
    // proc.body should always be a Seqn, which will be converted to a Stmt_Block (<: Stmt), which is exactly what B3 requires  
    val body = proc.body match {
      case Seqn(Seq()) => B3.Option_None[RawAst.Stmt]
      case stmt: Seqn => B3.Option_Some[RawAst.Stmt](transformStatement(stmt))
      case _ => {
        println("FAIL: body of Boogie Procedure '" + proc.name.name + "' was not a Seqn stmt! It was: " + proc.body.getClass.getName)
        B3.Option_None[RawAst.Stmt]
      }
    }

    // proc.body match {
    //   case Seqn(seq) => println(seq.foreach(d => println(d.getClass.getName)))
    //   case _ => println(proc.body.getClass.getName)
    // }

    // finally, creating raw B3 Procedure
    B3.Procedure(name = proc.name.name,                     // TODO: Make sure that that name is valid! (proc.name contains different names, see definition of Identifier in boogie.scala)
                 parameters = Seq[RawAst.PParameter](),     // TODO
                 pre = Seq[RawAst.AExpr](),     // No data for these, but also empty in Boogie
                 post = Seq[RawAst.AExpr](),    // No data for these, but also empty in Boogie
                                                // TODO-later: Boogie additionally has "modifies", which is used there for Heap stuff. Need to find workaround
                 body = body)
  }

  // /** All Stmt except Comment, Seqn, and CommentBlock are always useful.
  //  *  CommentBlock and Seqn are only useful if they contain any useful Stmt.
  //  *  Comment is never useful */
  // private def stmtIsUseful(stmt: Stmt): Boolean = {
  //   unpackStmtToSeqn(stmt) match {
  //     case Seqn(stmts) => (stmts map isEmptyStmt).foldLeft(false)(_ || _)
  //     case Seqn(Seq()) => false
  //     case _: Comment => false
  //     case _ => true
  //   }
  // }

  /** Unpacks CommentBlock stmts and nested Seqs. Returns Comment("Useless!") if no useful stmts */
  private def unpackStmtToSeqn(stmt: Stmt): Stmt = {
    stmt match {
      case commBlock: CommentBlock => unpackStmtToSeqn(commBlock.stmt) 
      case Seqn(stmtSeq) => {
        val seq = stmtSeq map unpackStmtToSeqn filter {
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
    stmt match {
      case _: Goto => println("TODO: Goto");                                        B3.TODO_Stmt()
      case _: AssertImpl => println("TODO: AssertImpl");                            B3.TODO_Stmt()
      case _: Assign => println("TODO: Assign");                                    B3.TODO_Stmt()
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
        val unpackedStmtSeq = unpackStmtToSeqn(seqn) match {
          case Seqn(seq) => seq
          case Comment(_) => Seq()
          case stmt => Seq(stmt) 
        }
        B3.Stmt_Block(unpackedStmtSeq map transformStatement)
      }
    }
  }


}