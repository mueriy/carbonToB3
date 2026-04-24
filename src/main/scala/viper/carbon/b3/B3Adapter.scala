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
object B3Helper {
  import viper.carbon.b3.DafnyHelper._

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
    val scalaSeqOfB3args = (Seq("dotnet", "verify")++options).map(x => DafnyHelper.Seq_fromString(x))
    val dafnySeqOfB3args = DafnyHelper.SeqT_fromSeq[DafnySequence[CodePoint]](scalaSeqOfB3args)
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
      sys.error("Resolving B3 RawAST to B3 AST or type check failed: " + resultResolver.dtor_error().toString)
    }
    val b3 = resultResolver.dtor_value()

    // Run B3 (verification) on Ast
    runVerify(b3, cli)
  }

  /** Corresponds to "check true" in raw AST format. Use this if a Stmt is required, but you dont want to implement it yet. */
  def TODO_Stmt(): RawAst.Stmt_Check = {
    val expr = new RawAst.Expr_BLiteral(true)
    new RawAst.Stmt_Check(expr)
  }

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

  // Option Some/None:
  /** creates a B3/Dafny Option->Some instance: "Some(input)" */
  def Option_Some[T](input: T)(implicit ct: ClassTag[T]): Std.Wrappers.Option[T] = {
    Std.Wrappers.Option.create_Some(td[T], input)
  }
  /** creates a B3/Dafny Option->None instance of given Type T */
  def Option_None[T](implicit ct: ClassTag[T]): Std.Wrappers.Option[T] = {
    Std.Wrappers.Option.create_None(td[T])
  }

  def Stmt_Block(seq: Seq[RawAst.Stmt]): RawAst.Stmt_Block = {
    new RawAst.Stmt_Block(SeqT_fromSeq[RawAst.Stmt](seq))
  }
}




// B3 note: possible sequences, sorted by wether they have subtypes or not...
// normal: seq<string>, seq<Case>, seq<PParameter>, seq<Variable>, seq<TypeName> ("= seq<string>"), 
//          seq<Tagger>, seq<Function>, seq<Axiom>, seq<Procedure>, seq<FParameter>, seq<CallArgument>
//          seq<Pattern>, seq<Binding>, seq<ClosureBinding>, seq<ClosureProperty>
// Special (have subtypes and used as Seq in some places):
//      seq<AExpr>, Option<Stmt>, seq<Expr>

