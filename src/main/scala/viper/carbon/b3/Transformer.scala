// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2021 ETH Zurich.

package viper.carbon.b3
import viper.carbon.b3.B3Nodes._


/**
 * An implementation for transformers of the (internal) B3 AST.
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
        case _: Type => parent
        case Program(signatureTypes, domains, types, taggers, functions, axioms, procedures) =>
          Program(signatureTypes, domains map go, types map go, taggers map go, 
                  functions map go, axioms map go, procedures map go)
        case _: Variable => parent
        case lvd:LocalVarDecl => 
          lvd match {
            case VarDecl(name, body, typ, isMutable, optInitVal) => VarDecl(name, go(body), typ, isMutable, optInitVal map go)
            case _: FParameter => parent
            case _: PParameter => parent
            case _: Binding => parent
          }
        case ae: AExpr =>
          ae match {
            case AExpression(e) => AExpression(go(e))
            case AAssertion(s) => AAssertion(go(s))
          }
        case d: Decl =>
          d match {
            case _:Domain => sys.error("Domain is NOT_SUPPORTED, so this should not be reachable") 
            // case ConstDecl(name, typ, unique) => ConstDecl(name, go(typ), unique)
            case TypeDecl(_, _) => parent
            // case TypeAlias(n, de) => TypeAlias(go(n), go(de))
            case Tagger(_,_) => parent
            case Function(name, args, typ, optTag, optBody) => Function(name, args map go, typ, optTag, optBody map go)
            case Axiom(exp, explains) => Axiom(go(exp), explains)
            // case GlobalVarDecl(name, typ) => GlobalVarDecl(name, go(typ))
            case Procedure(name, args, optBody, pre, post) => Procedure(name, args map go, optBody map go, pre map go, post map go)
            // case CommentedDecl(s, ds, a, b) => CommentedDecl(s, ds map go, a, b)
            // case DeclComment(_) => parent
            // case LiteralDecl(_) => parent
          }
        case FunctionDef(body, when) => FunctionDef(go(body), when map go)
        case ss: Stmt =>
          ss match {
            case _:VarDecl => sys.error("this case should not be reachable (LocalVar is already handled by LocalVarDecl sub-case)")
            case Assign(lhs, rhs) => Assign(lhs, go(rhs))
            case Reinit(_) => parent            
            case Block(s) => Block(s map go)
            case Check(e, error) => Check(go(e), error)
            case Assume(e) => Assume(go(e))
            case Assert(e, error, b3Code) => Assert(go(e), error, b3Code)
            case Choose(thn, els) => Choose(go(thn), go(els))
            case If(cond, thn, els) => If(go(cond), go(thn), go(els))
            case LabeledStmt(lbl, body) => LabeledStmt(lbl, go(body))
          }
        case e: Expr =>
          // Note: If you have to update this pattern match to make it exhaustive, it
          // might also be necessary to update the PrettyPrinter.toParenDoc method.
          e match {
            case BoolLit(_) => parent
            case IntLit(_) => parent
            case RealLit(_) => parent
            case IdExpr(_,_,_) => parent
            case OpExpr(op, es) => OpExpr(op, es map go)
            case CondExp(cond, thn, els) => CondExp(go(cond), go(thn), go(els))
            // case MapSelect(map, idxs) => MapSelect(go(map), idxs map go)
            // case MapUpdate(map, idxs, value) => MapUpdate(go(map), idxs map go, go(value))
            // case Old(exp) => Old(go(exp)) //TODO: check if this is actually ever used somewhere
            case FunctionCallExpr(func, args, typ) => FunctionCallExpr(func, args map go, typ)
            case LabeledExpr(l, e) => LabeledExpr(l, go(e))
            case Forall(v, pat, exp, tv, w) => Forall(v map go, pat map go, go(exp), tv map go, w)
            case Exists(v, pat, exp, w) => Exists(v map go, pat map go, go(exp), w)
            case Pattern(es) => Pattern(es map go)
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
        case _: NOT_SUPPORTED => Seq(parent)
        case _: Type => Seq(parent)
        case Program(signatureTypes, domains, types, taggers, functions, axioms, procedures) =>
          for {domainsResult <- goSeq(domains); typesResult <- goSeq(types); 
               taggersResult <- goSeq(taggers); functionsResult <- goSeq(functions); 
               axiomsResult <- goSeq(axioms); proceduresResult <- goSeq(procedures)} yield 
                Program(signatureTypes, domainsResult, typesResult, taggersResult, 
                        functionsResult, axiomsResult, proceduresResult)
        case _: Variable => Seq(parent)
        case lvd: LocalVarDecl =>
          lvd match {
            case VarDecl(name, body, typ, isMutable, optInitVal) => 
              for {bodyResult <- go(body); initValResult <- {optInitVal match {
                case None => Seq(None)
                case Some(expr) => go(expr).map(Some(_))}
              }} yield VarDecl(name, bodyResult, typ, isMutable, initValResult)
            case _: FParameter => Seq(parent)
            case _: PParameter => Seq(parent)
            case _: Binding => Seq(parent)
          }
        case ae: AExpr =>
          ae match {
            case AExpression(e) => go(e) map (AExpression(_))
            case AAssertion(s) => go(s) map (AAssertion(_))
          }
        // case LocalVarDecl(name, typ, Some(where)) =>
        //   for {typResult <- go(typ); whereResult <- go(where)} yield
        //   LocalVarDecl(name, typResult, Some(whereResult))
        // case LocalVarDecl(name, typ, None) =>
        //   go(typ) map (LocalVarDecl(name, _, None))
        case d: Decl =>
          d match {
            case _:Domain => sys.error("Domain is NOT_SUPPORTED, so this should not be reachable") 
            // case ConstDecl(name, typ, unique) => go(typ) map (ConstDecl(name, _, unique))
            case TypeDecl(_,_) => Seq(parent)
            case Tagger(_,_) => Seq(parent)
            // case TypeAlias(n, de) => for {nResult <- go(n); deResult <- go(de)} yield TypeAlias(nResult, deResult)
            case Function(name, args, typ, optTag, optBody) => 
              for {argsResult <- goSeq(args); typResult <- go(typ); 
                bodyResult <- {optBody match {
                  case None => Seq(None)
                  case Some(fdef) => go(fdef).map({x => Some(x)})}
              }} yield Function(name, argsResult, typResult, optTag, bodyResult)
            case Axiom(exp, explains) => go(exp) map (Axiom(_, explains))
            // case GlobalVarDecl(name, typ) => go(typ) map (GlobalVarDecl(name, _))
            case Procedure(name, args, optBody, pre, post) =>
              for {argsResult <- goSeq(args); preResult <- goSeq(pre); postResult <- goSeq(post); 
                bodyResult <- {optBody match {
                  case None => Seq(None)
                  case Some(stmt) => go(stmt).map({x => Some(x)})}
              }} yield Procedure(name, argsResult, bodyResult, preResult, postResult)
          }
        case FunctionDef(body, when) => 
          for {bodyResult <- go(body); whenResult <- goSeq(when)} yield
            FunctionDef(bodyResult, whenResult)
        case ss: Stmt =>
          ss match {
            case _:VarDecl => sys.error("this case should not be reachable (LocalVar is already handled by LocalVarDecl sub-case)")
            case Assign(lhs, rhs) => for {lhsResult <- go(lhs); rhsResult <- go(rhs)} yield Assign(lhsResult, rhsResult)
            case Reinit(es) => goSeq(es) map (Reinit(_))
            case Block(s) => goSeq(s) map (Block(_))
            case Check(e, error) => go(e) map (Check(_, error))
            case Assume(e) => go(e) map (Assume(_))
            case Assert(e, error, b3Code) => go(e) map (Assert(_, error, b3Code))
            case Choose(thn, els) => for {thnResult <- go(thn); elsResult <- go(els)} yield Choose(thnResult, elsResult)
            case If(cond, thn, els) =>
              for {condResult <- go(cond); thnResult <- go(thn); elsResult <- go(els)} yield
                (If(condResult, thnResult, elsResult))
            case LabeledStmt(lbl, body) => go(body) map (LabeledStmt(lbl, _))
          }
        case e: Expr =>
          // Note: If you have to update this pattern match to make it exhaustive, it
          // might also be necessary to update the PrettyPrinter.toParenDoc method.
          e match {
            case BoolLit(_) => Seq(parent)
            case IntLit(_) => Seq(parent)
            case RealLit(_) => Seq(parent)
            case IdExpr(n,t,o) => go(t) map (IdExpr(n,_,o))
            // case RealConv(exp) => go(exp) map (RealConv(_))
            // case Const(_) => Seq(parent)
            // case MapSelect(map, idxs) =>
            //   for {mapResult <- go(map); idxsResult <- goSeq(idxs)} yield MapSelect(mapResult, idxsResult)
            // case MapUpdate(map, idxs, value) =>
            //   for {mapResult <- go(map); idxsResult <- goSeq(idxs); valueResult <- go(value)} yield MapUpdate(mapResult, idxsResult, valueResult)
            // case Old(exp) => go(exp) map (Old(_))
            case OpExpr(op, es) => goSeq(es) map (OpExpr(op,_))
            case CondExp(cond, thn, els) =>
              for {condResult <- go(cond); thnResult <- go(thn); elsResult <- go(els)} yield
                (CondExp(condResult, thnResult, elsResult))
            case FunctionCallExpr(func, args, typ) => {
              for {argsResult <- goSeq(args); typResult <- go(typ)} yield FunctionCallExpr(func, argsResult, typResult)
            }
            case LabeledExpr(lbl, e) => go(e) map (LabeledExpr(lbl, _))
            case Exists(v, triggers, exp, w) =>
              for {vResult <- goSeq(v); triggersResult <- goSeq(triggers); expResult <- go(exp)} yield
                Exists(vResult, triggersResult, expResult, w)
            case Forall(v, triggers, exp, tv, w) =>
              for {vResult <- goSeq(v); triggersResult <- goSeq(triggers); expResult <- go(exp)} yield
              Forall(vResult, triggersResult, expResult, tv, w)
            case Pattern(exps) => goSeq(exps) map (Pattern(_))
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
