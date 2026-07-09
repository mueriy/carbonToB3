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
   * is, all statements except `Seqn`, including statements in the branches of
   * if's.
   *
   * Taken from the Viper AST with minimal adaptation.
   */
  def children(s: Stmt): Seq[Stmt] = {
    s match {
      case c: If => Seq(s) ++ children(c.thn) ++ children(c.els)
      case c: Choose => 
        // supports any number of branches
        val options = c.branches
        Seq(s) ++ (options flatMap children)
      case c: Block => c.stmts flatMap children
      // B3 TODO: check if there are other cases that we use in B3 (there is e.g. IfCase, but we dont use that, so we dont need it here - for now)
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
          sys.error("Local variable " + l.name + " is declared with type " + d.typ + " but used with type " + l.typ + ".")
        }
        case _ => Nil
      }
      case _ => Nil
    }
    def combineLists(s1: Seq[IdExpr], s2: Seq[IdExpr]) = {
      for (l1 <- s1; l2 <- s2) {
        if (l1.name == l2.name && l1.typ != l2.typ) {
          sys.error("Local variable " + l1.name + " is used with different types " + l1.typ + " and " + l2.typ)
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
      case Program(_, doms, typs, tags, fcts, axs, procs) =>
        doms ++ typs ++ tags ++ fcts ++ axs ++ procs
      case _: Variable => Nil
      case lvd:LocalVarDecl => 
        lvd match {
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
          case TypeDecl(_, _) => Nil
          case Tagger(_, _) => Nil
          case Function(_, args, _, _) => args
          case Axiom(_, exp) => exp
          case Procedure(_, args, pre, post, optBody) => args ++ pre ++ post ++ (optBody map {_.toSeq})
        }
      case ss: Stmt =>
        ss match {
          case VarDecl(_, body, _, _, optInitVal) => body ++ (optInitVal map {_.toSeq.asInstanceOf[Node]})
          case Assign(lhs, rhs) => Seq(lhs, rhs)
          case Reinit(v) => v
          case Block(s) => s
          case Check(e, error) => e
          case Assume(e) => e
          case Assert(e, error) => e
          case Choose(branches) => branches
          case If(cond, thn, els) => Seq(cond, thn, els)
          case Loop(inv, body) => inv ++ body
          case LabeledStmt(_, body) => body
        }
      case e: Expr =>
        // Note: If you have to update this pattern match to make it exhaustive, it
        // might also be necessary to update the PrettyPrinter.toParenDoc method.
        e match {
          case BoolLit(_) => Nil
          case IntLit(_) => Nil
          case IdExpr(_,_,_) => Nil
          case OperatorExpr(_, es) => es
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
      case Some(ee) => ee
      case None =>
        exp match {
          case IntLit(i) => exp
          case BoolLit(b) => exp
          // case RealLit(b) => exp
          // case RealConv(exp) => RealConv(func(exp))
          case IdExpr(n, t, b) => exp
          case OperatorExpr(op, exprs) => OperatorExpr(op, exprs map func)
          // case Const(i) => exp
          // case MapSelect(map, idxs) => MapSelect(func(map), idxs map func)
          // case MapUpdate(map, idxs, value) => MapUpdate(func(map), idxs map func, func(value))
          // case Old(e) => Old(func(e))
          case CondExp(cond, thn, els) => CondExp(func(cond), func(thn), func(els))
          case FunctionCallExpr(ff, args, typ) => FunctionCallExpr(ff, args map func, typ)
          case LabeledExpr(label, expr) => LabeledExpr(label, func(expr)) 
          case Exists(v, triggers, e, w) => Exists(v, (triggers map (_ match {case (es) => Pattern(es map func)})), func(e), w)
          case Forall(v, triggers, e, tv, w) => Forall(v, (triggers map (_ match {case Pattern(es) => Pattern(es map func)})), func(e), tv, w)
        }
    }
  }
}
