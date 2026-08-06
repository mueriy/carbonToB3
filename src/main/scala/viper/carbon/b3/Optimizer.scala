// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2021 ETH Zurich.

package viper.carbon.b3
import viper.carbon.b3.B3Nodes._

/**
 * Optimize a given Boogie program or expression.

 */
object Optimizer {

  /**
   * Optimizes a B3 program or expression by performing the following simplifications:
   * - Constant folding for booleans, integers and reals.
   * - Removal of dead branches.
   * - Removal of assertions known to hold.
   *
   * Taken from boogie-Carbon's Optimizer (where it says: "Constant folding partly taken from 
   * Transformer.simplify from SIL, but added more optimizations.")
   */
  def optimize(n: Node): Node = {
    /* Always optimize children first, then treat parent. */
    Transformer.transform(n)(_ => true, {
      case OpExpr(Not, Seq(BoolLit(literal))) =>
        BoolLit(!literal)
      case OpExpr(Not, Seq(OpExpr(Not, Seq(single)))) => single

      case OpExpr(And, Seq(TrueLit(), right)) => right
      case OpExpr(And, Seq(left, TrueLit())) => left
      case OpExpr(And, Seq(FalseLit(), _)) => FalseLit()
      case OpExpr(And, Seq(_, FalseLit())) => FalseLit()

      case OpExpr(Or, Seq(FalseLit(), right)) => right
      case OpExpr(Or, Seq(left, FalseLit())) => left
      case OpExpr(Or, Seq(TrueLit(), _)) => TrueLit()
      case OpExpr(Or, Seq(_, TrueLit())) => TrueLit()

      case OpExpr(Implies, Seq(FalseLit(), _)) => TrueLit()
      case OpExpr(Implies, Seq(_, TrueLit())) => TrueLit()
      case OpExpr(Implies, Seq(TrueLit(), FalseLit())) => FalseLit()
      case OpExpr(Implies, Seq(TrueLit(), consequent)) => consequent

      case OpExpr(EqCmp, Seq(BoolLit(left), BoolLit(right))) => BoolLit(left == right)
      case OpExpr(EqCmp, Seq(FalseLit(), right)) => OpExpr(Not, Seq(right))
      case OpExpr(EqCmp, Seq(left, FalseLit())) => OpExpr(Not, Seq(left))
      case OpExpr(EqCmp, Seq(TrueLit(), right)) => right
      case OpExpr(EqCmp, Seq(left, TrueLit())) => left
      case OpExpr(EqCmp, Seq(IntLit(left), IntLit(right))) => BoolLit(left == right)
//B3 REAL: case OpExpr(EqCmp, Seq(RealLit(left), RealLit(right))) => BoolLit(left == right)

      case OpExpr(NeCmp, Seq(BoolLit(left), BoolLit(right))) => BoolLit(left != right)
      case OpExpr(NeCmp, Seq(FalseLit(), right)) => right
      case OpExpr(NeCmp, Seq(left, FalseLit())) => left
      case OpExpr(NeCmp, Seq(TrueLit(), right)) => OpExpr(Not, Seq(right))
      case OpExpr(NeCmp, Seq(left, TrueLit())) => OpExpr(Not, Seq(left))
      case OpExpr(NeCmp, Seq(IntLit(left), IntLit(right))) => BoolLit(left != right)
//B3 REAL: case OpExpr(NeCmp, Seq(RealLit(left), RealLit(right))) => BoolLit(left != right)

      case CondExp(TrueLit(), ifTrue, _) => ifTrue
      case CondExp(FalseLit(), _, ifFalse) => ifFalse
      case CondExp(_, FalseLit(), FalseLit()) =>
        FalseLit()
      case CondExp(_, TrueLit(), TrueLit()) =>
        TrueLit()
      case CondExp(condition, FalseLit(), TrueLit()) =>
        OpExpr(Not, Seq(condition))
      case CondExp(condition, TrueLit(), FalseLit()) => condition
      case CondExp(condition, FalseLit(), ifFalse) =>
        OpExpr(And, Seq(OpExpr(Not, Seq(condition)), ifFalse))
      case CondExp(condition, TrueLit(), ifFalse) =>
        OpExpr(Or, Seq(condition, ifFalse))
      case CondExp(condition, ifTrue, FalseLit()) =>
        OpExpr(And, Seq(condition, ifTrue))
      case CondExp(condition, ifTrue, TrueLit()) =>
        OpExpr(Or, Seq(OpExpr(Not, Seq(condition)), ifTrue))

      case Forall(_, _, BoolLit(literal), _, _) =>
        BoolLit(literal)
      case Exists(_, _, BoolLit(literal), _) =>
        BoolLit(literal)

      case OpExpr(Minus, Seq(IntLit(literal))) => IntLit(-literal)
//B3 REAL: case OpExpr(Minus, Seq(RealLit(literal))) => RealLit(-literal)
      case OpExpr(Minus, Seq(OpExpr(Minus, Seq(single)))) => single

      case OpExpr(LeCmp, Seq(IntLit(left), IntLit(right))) =>
        BoolLit(left <= right)
      case OpExpr(LtCmp, Seq(IntLit(left), IntLit(right))) =>
        BoolLit(left < right)

      case OpExpr(Add, Seq(IntLit(left), IntLit(right))) =>
        IntLit(left + right)
      case OpExpr(Sub, Seq(IntLit(left), IntLit(right))) =>
        IntLit(left - right)
      case OpExpr(Mul, Seq(IntLit(left), IntLit(right))) =>
        IntLit(left * right)
     // This case was removed - the evaluation as doubles and translation of RealLit can introduce rounding/precision errors
     /* case OpExpr(IntLit(left), Div, IntLit(right)) if right != 0 =>
        RealLit(left.toDouble / right.toDouble)*/

      /* In the general case, Carbon uses the SMT division and modulo. Scala's division is not in-sync with SMT division.
         For nonnegative dividends and divisors, all used division and modulo definitions coincide. So, in order to not
         not make any assumptions on the SMT division, division and modulo are simplified only if the dividend and divisor
         are nonnegative.
       */
      case OpExpr(IntDiv, Seq(IntLit(left), IntLit(right))) if left >= 0 && right > 0 =>
        IntLit(left / right)
      case OpExpr(Mod, Seq(IntLit(left), IntLit(right))) if left >= 0 && right > 0 =>
        IntLit(left % right)

/*B3 REAL
      case OpExpr(LeCmp, Seq(RealLit(left), RealLit(right))) =>
        BoolLit(left <= right)
      case OpExpr(LtCmp, Seq(RealLit(left), RealLit(right))) =>
        BoolLit(left < right)

      case OpExpr(Add, Seq(RealLit(left), RealLit(right))) =>
        RealLit(left + right)
      case OpExpr(Sub, Seq(RealLit(left), RealLit(right))) =>
        RealLit(left - right)
      case OpExpr(Mul, Seq(RealLit(left), RealLit(right))) =>
        RealLit(left * right)
      case OpExpr(Div, Seq(RealLit(left), RealLit(right))) if right != 0 =>
        RealLit(left / right)
      case OpExpr(Mod, Seq(RealLit(left), RealLit(right))) if right != 0 =>
        RealLit(left % right)
*/

      case If(TrueLit(), thn, _) => thn
      case If(FalseLit(), _, els) => els

      case If(_, thn, els) if thn.children.isEmpty && els.children.isEmpty =>
        Statements.EmptyStmt

      case Assert(TrueLit(), _, _) => Statements.EmptyStmt
      case Assume(TrueLit()) => Statements.EmptyStmt

      // --- New B3 optimizations ---
      // Block-Stmt optimization
      // There is no hard requirement anywhere that we need a Block-Stmt (instead of just any Stmt),
      // and there is no special functionality provided by Block-stmts, so we can remove Block-Stmts
      // wherever possible, which will make the AST-printout much more readable without changing its 
      // function.
      // For a single Stmt we dont need a Block around it.
      case Block(Seq(singleStmt)) => singleStmt 
      // Empty Block-stmts do nothing if they are inside of other Block-stmts.
      // (In other places, like the thn/els branch of an If-stmt, they are needed as "do nothing here"-Stmts)
      case Block(stmtSeq) => {
        val stmtSeqFiltered = stmtSeq filter {
          case Block(Seq()) => false
          case _ => true
        }
        // If there is only one stmt left, replace the original Block-stmt with that. 
        // Otherwise we get either an empty Block, or a normal Block-stmt containing multiple stmts. 
        stmtSeqFiltered match {
          case Seq(singleStmt) => singleStmt
          case seqOfStmtsOrEmpty => Block(seqOfStmtsOrEmpty)
        }
      }      
    })
  }
}
