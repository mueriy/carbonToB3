package viper.carbon.b3
import dafny._
import scala.jdk.CollectionConverters._
import scala.reflect.ClassTag

/** Helper methods to make it easier to work with dafny classes. */
object DafnyHelper {

  /** returns TypeDescriptor<T> of given type T (for td[T]) */
  private[b3] def td[T](implicit ct: ClassTag[T]): TypeDescriptor[T] = {
    TypeDescriptor.reference(ct.runtimeClass.asInstanceOf[Class[T]])
  }

  /** 
   * Returns an empty DafnySequence of type T. Example: "SeqT_empty[RawAst.Expr]"
   * 
   * @tparam T The type of the sequence
   * @return an empty DafnySequence<T>
   */
  def SeqT_empty[T](implicit ct: ClassTag[T]): DafnySequence[T] = {
    // This might only be necessairy internally
    DafnySequence.empty(td[T])
  }

  /**
   * Returns the DafnySequence<T> corresponding to the provided seq, where T is the provided type
   * 
   * @param seq Scala sequence with elements of type T, or of type T' where T' extends T
   * @tparam T The type of Seq. In case of Stmt, Expr, or AExpr, must use them and NOT their subtypes! 
   *           This SHOULD always be provided, since it MUST be provided in case that seq is empty.  
   * @return The DafnySequence<T> corresponding to the given seq
   */
  def SeqT_fromSeq[T](seq: Seq[T])(implicit ct: ClassTag[T]): DafnySequence[T] = {
    // (Type of non-empty seq could be inferred, but the caller should not have to know
    //  whether their seq is empty or not, so we always require and use T)
    if (seq.isEmpty) {
      return SeqT_empty[T]
    } else {
      return DafnySequence.fromArray(td[T], dafny.Array.fromList(td[T], seq.map(x => x: T).asJava))
    }
  } 

  /** returns DafnySequence<CodePoint> containing the provided (Scala) String str */
  def Seq_fromString(str: String): DafnySequence[CodePoint] = {
    DafnySequence.asUnicodeString(str)
  }

}

/** Helper methods to work with B3 */
object B3Adapter {
  import viper.carbon.b3.DafnyHelper._

  // B3 MAIN METHOD (+ AUXILIARY METHODS)
  /** uses B3 to print the RawAst Program (= stage 1/2) */
  def printRawAst(program: RawAst.Program): Unit = {
    Printer.__default.Program(program) 
  }

  /** uses B3 to print the Ast Program (= stage 2/2) */
  def printAst(program: Ast.Program): Unit = {
    ResolvedPrinter.__default.Program(program) 
  }

  /** Transforms options into what B3 expects the command line information (cli) to look like. Can be used as input for B3 methods that require this. */
  def parseOptions(options: Seq[String]): Std.Wrappers.Result[CommandLineOptions.CliResult[B3.Verb], DafnySequence[_ <: CodePoint]] = {
    // CliResult[B3.Verb] has the fields: verb, options, and files.
    // The first argument (in seqOfB3args) is ignored.
    // The second argument could be "parse", "resolve", or "verify", but in our case we require "verify"
    // The following arguments are interpreted as options if the string starts with "--" and as files (paths) otherwise
    // (files are ignored by us).

    // Transform "options" to what the corresponding "args" of B3's Main method would be  
    val scalaSeqOfB3args = (Seq("dotnet", "verify")++options).map(x => Seq_fromString(x))
    val dafnySeqOfB3args = SeqT_fromSeq[DafnySequence[CodePoint]](scalaSeqOfB3args)
    // Parse args
    CommandLineOptions.__default.Parse(B3.Verb._typeDescriptor(), new B3.B3CliSyntax(), dafnySeqOfB3args)
  }

  /** Run B3's ResolveAndTypeCheck (transforms RawAST -> AST and does type checks) */
  def resolveAndTypeCheck(rawB3Ast: RawAst.Program, cli: CommandLineOptions.CliResult[B3.Verb]): Std.Wrappers.Result[Ast.Program, DafnySequence[_ <: CodePoint]] = {
    B3.__default.ResolveAndTypeCheck(td[B3.Verb], rawB3Ast, cli)
  }

  def runVerify(b3Ast: Ast.Program, cli: CommandLineOptions.CliResult[B3.Verb]): Unit = {
    Verifier.__default.Verify(b3Ast, cli.dtor_options())
  }
  

  // The following imitates B3's Main method, but skipping e.g. the code -> RawAst part.
  /**
   * Runs B3 on the given B3 raw AST Program node (rawB3Ast) using the flags defined in (options) as B3 flags.
   * 
   * @param rawB3Ast A B3 raw AST Program
   * @param options used as if they were the flags used when running B3 normally. Use Seq("--flag1", "--flag2", ...) - format.
   * @return Nothing (Unit), but will print the same things that B3 would print (to stdout)
   */
  def runB3(rawB3Ast: RawAst.Program, options: Seq[String]): Unit = {

    //Parse options into the format that B3 expects them to be.
    val cliResult = parseOptions(options) 
    if (cliResult.is_Failure()) {
      sys.error("Parsing of B3 options failed: " + cliResult.dtor_error().toString)
    }
    val cli = cliResult.dtor_value()

    //Possibly print RawAst
    if (options.contains("--print")) {
      printRawAst(rawB3Ast);
    }

    // Transform RawAst -> Ast
    val resultResolver = resolveAndTypeCheck(rawB3Ast, cli)
    if (resultResolver.is_Failure()) {
      println("ERROR: (runB3 -> resolveAndTypeCheck) Resolving B3 RawAST to B3 AST or type check failed: " + resultResolver.dtor_error().toString)
      return
    }
    val b3 = resultResolver.dtor_value()

    // Run B3 (verification) on Ast
    runVerify(b3, cli)
  }


  // STANDALONE NODES
  /** creates a B3 RawAst Program node using the provided scala sequences */
  def Program(types: Seq[String], taggers: Seq[RawAst.Tagger], functions: Seq[RawAst.Function], 
              axioms: Seq[RawAst.Axiom], procedures: Seq[RawAst.Procedure]): RawAst.Program = {

    new RawAst.Program(SeqT_fromSeq[DafnySequence[CodePoint]](types.map(x => Seq_fromString(x))),
                        SeqT_fromSeq[RawAst.Tagger](taggers),
                        SeqT_fromSeq[RawAst.Function](functions),
                        SeqT_fromSeq[RawAst.Axiom](axioms),
                        SeqT_fromSeq[RawAst.Procedure](procedures))
  }

  /** creates a B3 RawAst Program node using the provided scala sequences + other inputs */
  def Procedure(name: String,
                parameters: Seq[RawAst.PParameter],
                pre: Seq[RawAst.AExpr],
                post: Seq[RawAst.AExpr],
                body: Std.Wrappers.Option[RawAst.Stmt]): RawAst.Procedure = {

    new RawAst.Procedure(Seq_fromString(name),
                         SeqT_fromSeq[RawAst.PParameter](parameters),
                         SeqT_fromSeq[RawAst.AExpr](pre),
                         SeqT_fromSeq[RawAst.AExpr](post),
                         body)
  }

  /**
    * Creates a (raw) B3 Procedure Parameter with given name, type, and mode.
    * Cannot make not make use of B3's (not fully documented) autoinv feature.
    *
    * @param name Parameter name
    * @param typ Type of the parameter. This must be either a type defined in the current Program, or one of the built-in types ("bool", "int", and "tag")
    * @param mode Is it a input (IN), output (OUT), or inout (INOUT) parameter? (All defined by the current object)
    * @return The corresponding (raw) PParameter
    */
  def PParameter(name: String, typ: String, mode: RawAst.ParameterMode = IN): RawAst.PParameter = {
    // val b3Mode = mode match {
    //   case IN => RawAst.ParameterMode_In
    //   case INOUT => RawAst.ParameterMode_InOut
    //   case OUT => RawAst.ParameterMode_Out
    // }
    new RawAst.PParameter(Seq_fromString(name), mode, Seq_fromString(typ), Option_None)
  }
  val IN = new RawAst.ParameterMode_In
  val INOUT = new RawAst.ParameterMode_InOut
  val OUT = new RawAst.ParameterMode_Out
  

  // Option Some/None:
  /** creates a B3/Dafny Option->Some instance: "Some(input)" */
  def Option_Some[T](input: T)(implicit ct: ClassTag[T]): Std.Wrappers.Option[T] = {
    Std.Wrappers.Option.create_Some(td[T], input)
  }
  /** creates a B3/Dafny Option->None instance of given Type T */
  def Option_None[T](implicit ct: ClassTag[T]): Std.Wrappers.Option[T] = {
    Std.Wrappers.Option.create_None(td[T])
  }


  /**
    * Creates a (raw) B3 Function node.
    *
    * @param name The function name.
    * @param parameters A Seq of the function's parameters (in B3's FParameter format)
    * @param resultType
    * @return A (raw) B3 function node according to the used parameters.
    */
  def Function(name: String, parameters: Seq[RawAst.FParameter], resultType: String): RawAst.Function = {
    new RawAst.Function(Seq_fromString(name), 
                        SeqT_fromSeq[RawAst.FParameter](parameters),
                        Seq_fromString(resultType),
                        Option_None,  // optional: tag
                        Option_None)  // optional: fct definition
  }
  /** Creates a (raw) B3 FParameter node, which defines a Function parameter. */
  def FParameter(name: String, typ: String): RawAst.FParameter = {
    // We currently do not support defining this parameter as injective (= 2nd arg)
    new RawAst.FParameter(Seq_fromString(name), false, Seq_fromString(typ))
  }

  /** Creates a (raw) B3 node that defines a Function call. The function must be defined in the current Program with matching number of args. */
  def FunctionCallExpr(name: String, args: Seq[RawAst.Expr]): RawAst.Expr_FunctionCallExpr = {
    new RawAst.Expr_FunctionCallExpr(Seq_fromString(name), SeqT_fromSeq[RawAst.Expr](args))
  }


  // STATEMENT NODES
  /** Corresponds to "{{check true}}" in raw AST format. Use this if a Stmt is required, but you dont want to implement it yet. */
  def TODO_Stmt(): RawAst.Stmt = {
    val expr = new RawAst.Expr_BLiteral(true)
    new RawAst.Stmt_Check(Expr_OperatorExpr(And, Seq(expr, Expr_OperatorExpr(And, Seq(expr, expr)))))
  }
  /** Corresponds to "{{check false}}" in raw AST format. Use this if a Stmt is required, but it is actually an advanced feature. */
  def LATER_Stmt(): RawAst.Stmt = {
    val expr = new RawAst.Expr_BLiteral(false)
    new RawAst.Stmt_Check(Expr_OperatorExpr(And, Seq(expr, Expr_OperatorExpr(And, Seq(expr, expr)))))
  }

  /** creates a B3 Block-Stmt containing the B3 statements provided by Sequence seq */
  def Stmt_Block(seq: Seq[RawAst.Stmt]): RawAst.Stmt_Block = {
    new RawAst.Stmt_Block(SeqT_fromSeq[RawAst.Stmt](seq))
  }

  /** created a raw B3 Assert-Stmt node using the provided expression. 
   * The 'error' parameter can current not be used by B3, but we require it for when that changes */
  def Stmt_Assert(exp: RawAst.Expr, error: String): RawAst.Stmt_Assert = {
    new RawAst.Stmt_Assert(exp)
  }

  /** created a raw B3 Assume-Stmt node using the provided expression. */
  def Stmt_Assume(exp: RawAst.Expr): RawAst.Stmt_Assume = {
    new RawAst.Stmt_Assume(exp)
  }

  /** creates a B3 Assign-Stmt '"assignToVar" = assignThisExpr'
   * assignToVar must be a variable in scope (= must be in body of the corresponding Stmt_VarDecl) */
  def Stmt_Assign(assignToVar: String, assignThisExpr: RawAst.Expr): RawAst.Stmt_Assign = {
    new RawAst.Stmt_Assign(Seq_fromString(assignToVar), assignThisExpr)
  }

  /** create B3 VarDecl-stmt; 'typ' must either be "bool", "int", or "tag" OR a type defined at the start of the program */
  def Stmt_VarDecl(name: String, body: RawAst.Stmt, typ: String, isMutable: Boolean = true): RawAst.Stmt_VarDecl = {
    val variable = new RawAst.Variable(Seq_fromString(name),              // name
                                       isMutable,                         // "isMutable" => var vs val
                                       Option_Some(Seq_fromString(typ)),  // "optionalType" => is NOT optional here (since we dont initiate a value)!
                                       Option_None[RawAst.Expr])          // optionalAutoInv => TODO: look if we can use this
    new RawAst.Stmt_VarDecl(variable, Option_None[RawAst.Expr], body) // Option_None ==> do not initiate variables (which we never want to do) 
  }

  /** create a B3 If-Stmt */
  def Stmt_If(cond: RawAst.Expr, thn: RawAst.Stmt, els: RawAst.Stmt): RawAst.Stmt_If = {
    new RawAst.Stmt_If(cond, thn, els)
  }

  /** create a B3 Choose stmt. This is basically a "If(*) {} else if (*) {} ... " stmt */
  def Stmt_Choose(stmts: Seq[RawAst.Stmt]): RawAst.Stmt_Choose = {
    new RawAst.Stmt_Choose(SeqT_fromSeq[RawAst.Stmt](stmts))
  }

  /** create a B3 Reinit-Stmt, which is equivalent to havoc */
  def Stmt_Reinit(vars: Seq[String]): RawAst.Stmt_Reinit = {
    new RawAst.Stmt_Reinit(SeqT_fromSeq[DafnySequence[CodePoint]](vars.map(x => Seq_fromString(x))))
  }





  // EXPRESSION NODES
    /** Corresponds to "true" in raw AST format. Use these if a bool expr is required, but you dont want to implement it yet. */
  def TODO_Expr_bool(): RawAst.Expr_BLiteral = {
    new RawAst.Expr_BLiteral(true)
  }
    /** Corresponds to "0" in raw AST format. Use these if a int expr is required, but you dont want to implement it yet. */
  def TODO_Expr_int(): RawAst.Expr_ILiteral = {
    new RawAst.Expr_ILiteral(java.math.BigInteger.valueOf(666))
  }

  def Expr_ILiteral(x: BigInt): RawAst.Expr_ILiteral = {
    new RawAst.Expr_ILiteral(x.bigInteger)
  }

  def Expr_BLiteral(b: Boolean): RawAst.Expr_BLiteral = {
    new RawAst.Expr_BLiteral(b)
  }

  def Expr_OperatorExpr(operator: RawAst.Operator, expressions: Seq[RawAst.Expr]): RawAst.Expr_OperatorExpr = {
      new RawAst.Expr_OperatorExpr(operator, SeqT_fromSeq[RawAst.Expr](expressions))
  }

  def Expr_IdExpr(name: String, isOld: Boolean = false): RawAst.Expr_IdExpr = {//(DafnySequence<? extends CodePoint> var1, boolean var2)
    new RawAst.Expr_IdExpr(Seq_fromString(name), isOld)
  }

  /** To enable using names of Boogie operators to define B3 operators. */
  val CondExp = new RawAst.Operator_IfThenElse
  val Add = new RawAst.Operator_Plus
  val And = new RawAst.Operator_LogicalAnd
  val Div = new RawAst.Operator_Div //TODO: This is WRONG! Div is for "RealLit"s, which we need to handle in a special way.
  val EqCmp = new RawAst.Operator_Eq
  val Equiv = new RawAst.Operator_Equiv
  val Implies = new RawAst.Operator_LogicalImp
  val IntDiv = new RawAst.Operator_Div // B3 uses the same div and mod that boogie uses (according to the B3 manual), so this works
  val LeCmp = new RawAst.Operator_AtMost
  val LtCmp = new RawAst.Operator_Less
  val Mod = new RawAst.Operator_Mod // B3 uses the same div and mod that boogie uses (according to the B3 manual), so this works
  val Mul = new RawAst.Operator_Times
  val NeCmp = new RawAst.Operator_Neq
  val Or = new RawAst.Operator_LogicalOr
  val Sub = new RawAst.Operator_Minus
  val Not = new RawAst.Operator_LogicalNot
  val Minus = new RawAst.Operator_UnaryMinus

    // datatype Expr = (remaining)
    // | CustomLiteral(s: string, typ: TypeName)
    // | FunctionCallExpr(name: string, args: seq<Expr>)
    // | LabeledExpr(name: string, expr: Expr)
    // | LetExpr(name: string, optionalType: Option<TypeName>, rhs: Expr, body: Expr)
    // | QuantifierExpr(univ: bool, bindings: seq<Binding>, patterns: seq<Pattern>, body: Expr)
    // | ClosureExpr(closureBindings: seq<ClosureBinding>, resultVar: string, resultType: TypeName, properties: seq<ClosureProperty>)

}


// B3 note: possible sequences, sorted by wether they have subtypes or not...
// normal: seq<string>, seq<Case>, seq<PParameter>, seq<Variable>, seq<TypeName> ("= seq<string>"), 
//          seq<Tagger>, seq<Function>, seq<Axiom>, seq<Procedure>, seq<FParameter>, seq<CallArgument>
//          seq<Pattern>, seq<Binding>, seq<ClosureBinding>, seq<ClosureProperty>
// Special (have subtypes and used as Seq in some places):
//      seq<AExpr>, Option<Stmt>, seq<Expr>

