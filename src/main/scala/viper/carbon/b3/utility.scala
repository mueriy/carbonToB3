// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2021 ETH Zurich.

package viper.carbon.b3
import viper.carbon.b3.B3Nodes._
import viper.carbon.b3.B3Implicits._


/**
 * Utility methods for statements.
 */
object Statements {
  /** An empty statement. */
  val EmptyStmt = Block(Seq())

  /**
   * Returns a list of all actual statements contained in a given statement.  That
   * is, all statements except `Block`, including statements in the branches of
   * if's.
   *
   * Taken from the Viper AST with minimal adaptation.
   */
  def children(s: Stmt): Seq[Stmt] = {
    s match {
      case If(_, thn, els) => Seq(s) ++ children(thn) ++ children(els)
      case Choose(thn, els) => Seq(s) ++ children(thn) ++ children(els)
      case Block(stmts) => stmts flatMap children
      case LabeledStmt(_, body) => Seq(s) ++ children(body) // B3 NOTE: maybe we should also add Seq(s) (?)
      case _ => List(s)
    }
  }

  /**
   * Returns a list of all undeclared local variables used in this statement.
   * If the same local variable is used with different
   * types, an exception is thrown.
   *
   * Taken from the Viper AST with minimal adaptation.
   */
  def undeclLocalVars(s: Stmt): Seq[IdExpr] = {
    def extractLocal(n: Node, decls: Seq[LocalVarDecl]) = n match {
      case l: IdExpr => decls.find(_.name == l.name) match {
        case None => List(l)
        case Some(d) if d.typ != l.typ => {
          sys.error(s"Local variable ${l.name} is declared with type ${d.typ} but used with type ${l.typ}.")
        }
        case _ => Nil
      }
      case _ => Nil
    }
    def combineLists(s1: Seq[IdExpr], s2: Seq[IdExpr]) = {
      for (l1 <- s1; l2 <- s2) {
        if (l1.name == l2.name && l1.typ != l2.typ) {
          sys.error("Local variable " + l1.name.name + " is used with different types " + l1.typ + " and " + l2.typ)
        }
      }
      (s1 ++ s2).distinct
    }
    def addDecls(n: Node, decls: Seq[LocalVarDecl]) = n match {
      case Exists(v, _, _, _) => decls ++ v
      case Forall(v, _, _, _, _) => decls ++ v
      case _ => decls
    }
    def combineResults(n: Node, decls: Seq[LocalVarDecl], localss: Seq[Seq[IdExpr]]) = {
      localss.fold(extractLocal(n, decls))(combineLists)
    }
    s.reduce(Nil, addDecls, combineResults)
  }
}

/**
 * Utility methods for AST nodes.
 */
object Nodes {

  /**
   * See Node.subnodes.
   */
  def subnodes(n: Node): Seq[Node] = {
    n match {
      case _: NOT_SUPPORTED => Nil
      case _: Type => Nil
      case Program(_, doms, typs, tags, fcts, axs, procs) =>
        doms ++ typs ++ tags ++ fcts ++ axs ++ procs
      case _: Variable => Nil
      case lvd:LocalVarDecl => 
        lvd match {
          case VarDecl(_, body, _, _, optInitVal) => body ++ optInitVal.toList
          case _: FParameter => Nil
          case _: PParameter => Nil
          case _: Binding => Nil
        }
      case ae: AExpr =>
        ae match {
          case AExpression(e) => e
          case AAssertion(s) => s
        }
      case d: Decl =>
        d match {
          case _:Domain => sys.error("Domain is NOT_SUPPORTED, so this should not be reachable") 
          case TypeDecl(_, _) => Nil
          case Tagger(_, _) => Nil
          case Function(_, args, _, _, optBody) => args ++ Seq() ++ optBody.toList
          case Axiom(exp, _) => exp
          case Procedure(_, args, optBody, pre, post) => args ++ pre ++ post ++ optBody.toList
        }
      case FunctionDef(body, when) => body
      case ss: Stmt =>
        ss match {
          case _:VarDecl => sys.error("this case should not be reachable (LocalVar is already handled by LocalVarDecl sub-case)")
          case Assign(lhs, rhs) => Seq(lhs, rhs)
          case Reinit(v) => v
          case Block(s) => s
          case Check(e, error) => e
          case Assume(e) => e
          case Assert(e, error, _) => e
          case Choose(thn, els) => Seq(thn, els)
          case If(cond, thn, els) => Seq(cond, thn, els)
          case LabeledStmt(_, body) => body
        }
      case e: Expr =>
        // Note: If you have to update this pattern match to make it exhaustive, it
        // might also be necessary to update the PrettyPrinter.toParenDoc method.
        e match {
          case BoolLit(_) => Nil
          case IntLit(_) => Nil
          case RealLit(_) => Nil
          case IdExpr(_,_,_) => Nil
          case OpExpr(_, es) => es
          case CondExp(cond, thn, els) => Seq(cond, thn, els)
          case FunctionCallExpr(_, args, _) => args
          case LabeledExpr(_, e) => e
          case Forall(v, pat, exp, _, _) => v ++ pat ++ exp
          case Exists(v, pat, exp, _) => v ++ pat ++ exp
          case Pattern(es) => es
        }
    }
  }

  /**
   * Transforms an expression using the function `f`;  if `f` returns `Some(e)`, then the previous expression
   * is replaced by e, and otherwise the previous expression is reused.
   * The function `f` must produce expressions that are valid in the given context.  For instance, it cannot
   * replace an integer literal by a boolean literal.
   */
  def transform(exp: Expr, f: PartialFunction[Expr, Option[Expr]]): Expr = {
    val func = (e: Expr) => transform(e, f)
    val t = if (f.isDefinedAt(exp)) f(exp) else None
    t match {
      case Some(se) => se
      case None =>
        exp match {
          case IntLit(_) => exp
          case BoolLit(_) => exp
          case RealLit(_) => exp
          // case RealConv(exp) => RealConv(func(exp))
          case IdExpr(_, _, _) => exp
          case OpExpr(op, es) => OpExpr(op, es map func)
          // case Const(i) => exp
          // case MapSelect(map, idxs) => MapSelect(func(map), idxs map func)
          // case MapUpdate(map, idxs, value) => MapUpdate(func(map), idxs map func, func(value))
          // case Old(e) => Old(func(e))
          case CondExp(cond, thn, els) => CondExp(func(cond), func(thn), func(els))
          case FunctionCallExpr(ff, args, typ) => FunctionCallExpr(ff, args map func, typ)
          case LabeledExpr(label, expr) => LabeledExpr(label, func(expr)) 
          case Exists(v, triggers, e, w) => Exists(v, (triggers map (_ match {case (es) => Pattern(es map func)})), func(e), w)
          case Forall(v, triggers, e, tv, w) => Forall(v, (triggers map (_ match {case Pattern(es) => Pattern(es map func)})), func(e), tv, w)
          case Pattern(es) => Pattern(es map func)
        }
    }
  }
}
