// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2021 ETH Zurich.

package viper.carbon.b3
import viper.carbon.b3.B3Nodes._


/**
 * An implementation for transformers of the Boogie AST.

 */
object Transformer {

  def transform[A <: Node](node: A,
                           pre: PartialFunction[Node, Node] = PartialFunction.empty)(
                            recursive: Node => Boolean = !pre.isDefinedAt(_),
                            post: PartialFunction[Node, Node] = PartialFunction.empty): A = {
    def go[B <: Node](root: B): B = {
      transform(root, pre)(recursive, post)
    }

    def recurse(parent: Node): Node = {
      parent match {
        case _: NOT_SUPPORTED => parent
        case Program(signatureTypes, domains, types, taggers, functions, axioms, procedures) =>
          Program(signatureTypes, domains map go, types map go, taggers map go, 
                  functions map go, axioms map go, procedures map go)
        case _: FParameter => parent
        case _: Variable => parent
        case _: PParameter => parent
        case _: Binding => parent
        case Pattern(es) => Pattern(es map go)
        case ae: AExpr =>
          ae match {
            case AExpression(e) => AExpression(go(e))
            case AAssertion(s) => AAssertion(go(s))
          }
        case d: Decl =>
          d match {
            // case ConstDecl(name, typ, unique) => ConstDecl(name, go(typ), unique)
            case TypeDecl(_, _) => parent
            // case TypeAlias(n, de) => TypeAlias(go(n), go(de))
            case Tagger(_,_) => parent
            case Function(name, args, typ, tag) => Function(name, args map go, typ, tag) //TODO: track if there are type modifications done anywhere; if so we need a Type Node
            case Axiom(explains, exp) => Axiom(explains, go(exp))
            // case GlobalVarDecl(name, typ) => GlobalVarDecl(name, go(typ))
            case Procedure(name, args, pre, post, optBody) => Procedure(name, args map go, pre map go, post map go, optBody map go)
            // case CommentedDecl(s, ds, a, b) => CommentedDecl(s, ds map go, a, b)
            // case DeclComment(_) => parent
            // case LiteralDecl(_) => parent
          }
        case ss: Stmt =>
          ss match {
            case VarDecl(name, body, typ, isMutable, optInitVal) => VarDecl(name, go(body), typ, isMutable, optInitVal map go)
            case Assign(lhs, rhs) => Assign(lhs, go(rhs)) //TODO: track if there are var name modifications done anywhere; if so we need a ID Node
            case Reinit(_) => parent            
            case Block(s) => Block(s map go)
            case Check(e, error) => Check(go(e), error)
            case Assume(e) => Assume(go(e))
            case Assert(e, error) => Assert(go(e), error)
            case Choose(branches) => Choose(branches map go)
            case If(cond, thn, els) => If(go(cond), go(thn), go(els))
            case Loop(inv, body) => Loop(inv map go, go(body))
            case LabeledStmt(lbl, body) => LabeledStmt(lbl, go(body))
          }
        case e: Expr =>
          // Note: If you have to update this pattern match to make it exhaustive, it
          // might also be necessary to update the PrettyPrinter.toParenDoc method.
          e match {
            case BoolLit(_) => parent
            case IntLit(_) => parent
            // case RealLit(_) => parent
            case IdExpr(_,_,_) => parent
            case OperatorExpr(op, es) => OperatorExpr(op, es map go)
            // case MapSelect(map, idxs) => MapSelect(go(map), idxs map go)
            // case MapUpdate(map, idxs, value) => MapUpdate(go(map), idxs map go, go(value))
            // case Old(exp) => Old(go(exp)) //TODO: check if this is actually ever used somewhere
            case FunctionCallExpr(func, args, typ) => FunctionCallExpr(func, args map go, typ)
            case LabeledExpr(l, e) => LabeledExpr(l, go(e))
            case Forall(v, pat, exp) => Forall(v map go, pat map go, go(exp))
            case Exists(v, pat, exp) => Exists(v map go, pat map go, go(exp))
          }
      }
    }

    val beforeRecursion = pre.applyOrElse(node, identity[Node])
    val afterRecursion = if (recursive(node)) {
      recurse(beforeRecursion)
    } else {
      beforeRecursion
    }
    post.applyOrElse(afterRecursion, identity[Node]).asInstanceOf[A]
  }
}


/*
object DuplicatingTransformer {

  def transform[A <: Node](node: A,
                           pre: PartialFunction[Node, Node] = PartialFunction.empty)(
                            recursive: Node => Boolean = !pre.isDefinedAt(_),
                            post: (Node => Seq[Node]) = (n => Seq(n))): Seq[A] = {
    def go[B <: Node](root: B): Seq[B] = {
      transform(root, pre)(recursive, post)
    }

    def goSeq[B <: Node](nodes: Seq[B]): Seq[Seq[B]] =
    {
      if (nodes.isEmpty) Seq(Seq()) else if (nodes.size == 1) go(nodes.head) map (Seq(_)) else {
        val headResults = go(nodes.head)
        val tailResults = goSeq(nodes.tail)
        for { hd <- headResults; tl <- tailResults } yield (hd +: tl)
      }
    }

    def recurse(parent: Node): Seq[Node] = {
      parent match {
        case Program(header, declarations) =>
          goSeq(declarations) map (Program(header,_))
        case LocalVarDecl(name, typ, Some(where)) =>
          for {typResult <- go(typ); whereResult <- go(where)} yield
          LocalVarDecl(name, typResult, Some(whereResult))
        case LocalVarDecl(name, typ, None) =>
          go(typ) map (LocalVarDecl(name, _, None))
        case Trigger(exps) =>
          goSeq(exps) map (Trigger(_))
        case _: Type => Seq(parent)
        case d: Decl =>
          d match {
            case ConstDecl(name, typ, unique) => go(typ) map (ConstDecl(name, _, unique))
            case TypeDecl(_) => Seq(parent)
            case TypeAlias(n, de) => for {nResult <- go(n); deResult <- go(de)} yield TypeAlias(nResult, deResult)
            case Func(name, args, typ, attrs) => for {argsResult <- goSeq(args); typResult <- go(typ)} yield Func(name, argsResult, typResult, attrs)
            case Axiom(exp) => go(exp) map (Axiom(_))
            case GlobalVarDecl(name, typ) => go(typ) map (GlobalVarDecl(name, _))
            case Procedure(name, ins, outs, body) =>
              for {insResult <- goSeq(ins); outsResult <- goSeq(outs); bodyResult <- go(body)} yield
                Procedure(name, insResult, outsResult, bodyResult)
            case CommentedDecl(s, ds, a, b) => goSeq(ds) map (CommentedDecl(s, _, a, b))
            case DeclComment(_) => Seq(parent)
            case LiteralDecl(_) => Seq(parent)
          }
        case ss: Stmt =>
          ss match {
            case Assign(lhs, rhs) => for {lhsResult <- go(lhs); rhsResult <- go(rhs)} yield Assign(lhsResult, rhsResult)
            case Assert(e, error) => go(e) map (Assert(_, error))
            case Assume(e) => go(e) map (Assume(_))
            case HavocImpl(es) => goSeq(es) map (HavocImpl(_))
            case Comment(_) => Seq(parent)
            case CommentBlock(s, stmt) => go(stmt) map (CommentBlock(s, _))
            case Seqn(s) => goSeq(s) map (Seqn(_))
            case If(cond, thn, els) =>
              for {condResult <- go(cond); thnResult <- go(thn); elsResult <- go(els)} yield
                (If(condResult, thnResult, elsResult))
            case NondetIf(thn, els) =>
              for {thnResult <- go(thn); elsResult <- go(els)} yield
                (NondetIf(thnResult, elsResult))
            case Label(_) => Seq(parent)
            case Goto(_) => Seq(parent)
            case LocalVarWhereDecl(idn, where) => go(where) map (LocalVarWhereDecl(idn, _))
          }
        case e: Exp =>
          // Note: If you have to update this pattern match to make it exhaustive, it
          // might also be necessary to update the PrettyPrinter.toParenDoc method.
          e match {
            case IntLit(_) => Seq(parent)
            case BoolLit(_) => Seq(parent)
            case RealLit(_) => Seq(parent)
            case RealConv(exp) => go(exp) map (RealConv(_))
            case LocalVar(n, t) => go(t) map (LocalVar(n, _))
            case GlobalVar(n, t) => go(t) map (GlobalVar(n, _))
            case Const(_) => Seq(parent)
            case MapSelect(map, idxs) =>
              for {mapResult <- go(map); idxsResult <- goSeq(idxs)} yield MapSelect(mapResult, idxsResult)
            case MapUpdate(map, idxs, value) =>
              for {mapResult <- go(map); idxsResult <- goSeq(idxs); valueResult <- go(value)} yield MapUpdate(mapResult, idxsResult, valueResult)
            case Old(exp) => go(exp) map (Old(_))
            case CondExp(cond, thn, els) =>
              for {condResult <- go(cond); thnResult <- go(thn); elsResult <- go(els)} yield
                (CondExp(condResult, thnResult, elsResult))
            case Exists(v, triggers, exp, w) =>
              for {vResult <- goSeq(v); triggersResult <- goSeq(triggers); expResult <- go(exp)} yield
                Exists(vResult, triggersResult, expResult, w)

            case Forall(v, triggers, exp, tv, w) =>
              for {vResult <- goSeq(v); triggersResult <- goSeq(triggers); expResult <- go(exp)} yield
              Forall(vResult, triggersResult, expResult, tv, w)
            case BinExp(left, binop, right) =>
              for {leftResult <- go(left); rightResult <- go(right)} yield BinExp(leftResult, binop, rightResult)
            case UnExp(unop, exp) => go(exp) map (UnExp(unop, _))
            case f@FuncApp(func, args, typ) => {
              val results =
                for {argsResult <- goSeq(args); typResult <- go(typ)} yield FuncApp(func, argsResult, typResult)
              results.foreach(fa => fa.showReturnType = f.showReturnType)
              results
            }
          }
      }
    }

    val beforeRecursion = pre.applyOrElse(node, identity[Node])
    val afterRecursion = if (recursive(node)) {
      recurse(beforeRecursion)
    } else {
      Seq(beforeRecursion)
    }
    (afterRecursion flatMap post).asInstanceOf[Seq[A]]
  }
}
*/
