package viper.carbon.b3
import dafny._
import _root_.scala.jdk.CollectionConverters._  // would also run without the "_root_.", but Metals doesnt like that 
import _root_.scala.reflect.ClassTag            // same
import _root_.scala.collection.mutable          // same
import viper.silver.ast.Member
import viper.silver.verifier.VerificationError

/** 
 * Helper methods to make it easier to work with "dafny code" (dafny library and B3.jar).
 * It should only be needed/used in the B3Adapter and B3Nodes objects. Otherwise, normal
 * Scala can be used.
 */
object DafnyHelper {
  /* The 'daf' method is an overloaded method that converts everything* from scala to its dafny/B3.jar
  versions. *For instances of e.g. Seq[T] or Object[T], the internal type T is NOT transformed! This 
  usually means T needs to be first converted to its B3 form using 'inst map {_.b3fy}'.
  (I.e. use 'daf(inst map {_.b3fy})' instead of 'daf(inst)'!) */

  // ***********************
  // DAFNY
  // ***********************

  /** 
   * = DafnySequence[_ <: CodePoint]
   * (the slightly easier to write 'DafnySequence[CodePoint]' cannot be used for Option, so just use this everywhere) 
   */
  type DString = DafnySequence[_ <: CodePoint]

  /** returns TypeDescriptor<T> of given type T (for td[T]) */
  private[b3] def td[T](implicit ct: ClassTag[T]): TypeDescriptor[T] = {
    TypeDescriptor.reference(ct.runtimeClass.asInstanceOf[Class[T]])
  }

  /** 
   * Returns an empty DafnySequence of type T. Example: "daf[RawAst.Expr]"
   * 
   * @tparam T The type of the sequence
   * @return an empty DafnySequence<T>
   */
  def daf[T](implicit ct: ClassTag[T]): DafnySequence[T] = emptyDSeqT[T]
  def emptyDSeqT[T](implicit ct: ClassTag[T]): DafnySequence[T] = {
    // This might only be necessairy internally
    DafnySequence.empty(td[T])
  }

  /**
   * Seq[T] -> DafnySequence[T], should be able to infer type T automatically, but it can
   * also be provided as type parameter (e.g. to force a subtype.)
   */
  def daf[T](seq: Seq[T])(implicit ct: ClassTag[T]): DafnySequence[T] = SeqToDSeq[T](seq)
  // /**
  //  * Returns the DafnySequence<T> corresponding to the provided seq, where T is the provided type
  //  * 
  //  * @param seq Scala sequence with elements of type T, or of type T' where T' extends T
  //  * @tparam T The type of Seq. In case of Stmt, Expr, or AExpr, must use them and NOT their subtypes! 
  //  *           This SHOULD always be provided, since it MUST be provided in case that seq is empty.  
  //  * @return The DafnySequence<T> corresponding to the given seq
  //  */
  def SeqToDSeq[T](seq: Seq[T])(implicit ct: ClassTag[T]): DafnySequence[T] = {
    return DafnySequence.fromArray(td[T], dafny.Array.fromList(td[T], seq.map(x => x: T).asJava))
    // // (Type of non-empty seq could be inferred, but the caller should not have to know
    // //  whether their seq is empty or not, so we always require and use T)
    // if (seq.isEmpty) {
    //   return emptyDSeqT[T]
    // } else {
    //   return DafnySequence.fromArray(td[T], dafny.Array.fromList(td[T], seq.map(x => x: T).asJava))
    // }
  } 

  /** 
   * Translates a Seq[String] to its dafny counterpart.
   * 
   * @param strSeq A Scala String Seq
   * @return DafnySequence<DafnySequence<CodePoint>>: a sequence containing the Strings in the input Seq converted to
   * DafnySequence<CodePoint> (in same order).
   */
  def daf(strSeq: Seq[String]): DafnySequence[DString] = SeqToDSeq_StringVersion(strSeq)
  def SeqToDSeq_StringVersion(strSeq: Seq[String]): DafnySequence[DString] = {
    SeqToDSeq[DString](strSeq.map(str => StringToDString(str)))
  }
  
  /** 
   * Translates a Seq[String] to its dafny !Set! counterpart.
   * 
   * @param strSeq A Scala String Seq
   * @return DafnySet<DafnySequence<CodePoint>>: a set containing the Strings in the input Seq converted to
   * DafnySet<CodePoint>.
   */
  def daf_Set(strSeq: Seq[String]): DafnySet[DString] = Set_fromStringSeq(strSeq)
  def Set_fromStringSeq(strSeq: Seq[String]): DafnySet[DString] = {
    val DstrSeq: Seq[DString] = strSeq.map(x => StringToDString(x))
    new DafnySet[DString](DstrSeq.asJava)
    // SeqToDSeq[DString](strSeq.map(str => StringToDString(str)))
  }

  /** returns DafnySequence<CodePoint> containing the provided (Scala) String str */
  def daf(str: String): DString = StringToDString(str)
  def StringToDString(str: String): DString = {
    DafnySequence.asUnicodeString(str)
  }



  // ***********************
  // B3.jar
  // ***********************

  /** 
   * scala Option[T] -> B3 Option[T]
   * 
   * !!! Use as: 'daf(opt.map{_.b3fy})' !!!
   * 
   * (Scala Option[T] has T' = "Scala-B3 node", but we want T = "B3 RawAst 'node'";
   * 'opt.map{_.b3fy}' converts Option[T'] -> Option[T] correctly, whether we have Some or None) 
   * 
   */
  def daf[T](opt: Option[T])(implicit ct: ClassTag[T]): Std.Wrappers.Option[T] = {
    opt match {
      case Some(value) => Std.Wrappers.Option.create_Some(td[T], value)
      case None => Std.Wrappers.Option.create_None(td[T])
    }
  }
}

/** Helper methods run B3 (run using runB3, or print a RawAst.Program using printRawAst) */
object B3Adapter {
  import viper.carbon.b3.DafnyHelper._

  // B3 MAIN METHOD (+ AUXILIARY METHODS)
  /** uses B3 to print the RawAst Program (= stage 1/2) */
  def printRawAst(program: RawAst.Program): Unit = {
    Printer.__default.Program(program) 
  }

  /** Transforms options into what B3 expects the command line information (cli) to look like. Can be used as input for B3 methods that require this. */
  private def parseOptions(options: Seq[String]): Std.Wrappers.Result[CommandLineOptions.CliResult[B3.Verb], DString] = {
    // CliResult[B3.Verb] has the fields: verb, options, and files.
    // The first argument (in seqOfB3args) is ignored.
    // The second argument could be "parse", "resolve", or "verify", but in our case we require "verify"
    // The following arguments are interpreted as options if the string starts with "--" and as files (paths) otherwise
    // (files are ignored by us).

    // Transform "options" to what the corresponding "args" of B3's Main method would be  
    val scalaSeqOfB3args = (Seq("dotnet", "verify")++options).map(x => StringToDString(x))
    val dafnySeqOfB3args = SeqToDSeq[DString](scalaSeqOfB3args)
    // Parse args
    CommandLineOptions.__default.Parse(B3.Verb._typeDescriptor(), new B3.B3CliSyntax(), dafnySeqOfB3args)
  }

  /** Run B3's ResolveAndTypeCheck (transforms RawAST -> AST and does type checks) */
  private def resolveAndTypeCheck(rawB3Ast: RawAst.Program, cli: CommandLineOptions.CliResult[B3.Verb]): Std.Wrappers.Result[Ast.Program, DString] = {
    B3.__default.ResolveAndTypeCheck(td[B3.Verb], rawB3Ast, cli)
  }

  private def runVerify(b3Ast: Ast.Program, cli: CommandLineOptions.CliResult[B3.Verb]): Unit = {
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
      System.out.println("-------------------------")
      System.out.println("ERROR: (runB3 -> resolveAndTypeCheck) Resolving B3 RawAST to B3 AST or type check failed: [" + resultResolver.dtor_error().verbatimString + "]")
      return
    }
    val b3 = resultResolver.dtor_value()

    // Run B3 (verification) on Ast
    runVerify(b3, cli)
  }
}

object B3Nodes {
  import viper.carbon.b3.DafnyHelper._

  /** 
   * The root of the Scala-B3 AST.
   * 
   * All nodes must have the b3fy method, which returns the corresponding RawAst "node".
   * This includes transforming all subnodes correctly (can use their own b3fy method for that).
   */
  sealed trait Node {
    //(Taken from boogie.scala -> Node) vvv
    /** 
     * 
    * This extra level of indirection (not calling transform directly), appears to affect the type-checking. We need to look into this.
    * The usage of this.type is also "suspect", since we are really casting in a way that can't be caught out at runtime..
    *
    * See Silver issue
    */
    def transform(pre: PartialFunction[Node, Node] = PartialFunction.empty)
                (recursive: Node => Boolean = !pre.isDefinedAt(_),
                  post: PartialFunction[Node, Node] = PartialFunction.empty)
    : this.type =
      Transformer.transform[this.type](this, pre)(recursive, post)
    //(Taken from boogie.scala -> Node) ^^^
  }
  sealed trait Decl extends Node // Domain, TypeDecl, Tagger, Function, Axiom, and Procedure. (TypeName is String)
  
  // "SPECIAL" NOODES
  /* Classes extending this trait are not supported for now */
  trait NOT_SUPPORTED extends Node
  sealed trait Type extends Node {
    def freeTypeVars: Seq[TypeVar] = Nil
    def b3fy: String
  }
  sealed trait BuiltInType extends Type
  case object Int extends BuiltInType { override def b3fy: String = "int" }
  case object Bool extends BuiltInType { override def b3fy: String = "bool" }
  case object Real extends BuiltInType { override def b3fy: String = "int" } // B3 TODO: change this to real as soon as that is possible
  /** This is only for temporary use. Using it in the final Program version will almost certainly fail. */
  case class TypeVar(name: String) extends Type {
    override def freeTypeVars: Seq[TypeVar] = Seq(this)
    override def b3fy: String = name
  }
  case class NamedType(n: String, typVars: Seq[Type] = Nil) extends Type {
    override def freeTypeVars: Seq[TypeVar] = typVars flatMap (_.freeTypeVars)
    val name = (n, typVars) match {
      case ("Perm", _) => "int" // B3 TODO: change "int" to "real" as soon as that is possible
      case (_, Seq()) => n
      case _ => getNameFromParamTypeConstel(n, typVars map {_.b3fy})
    } 
    override def b3fy: String = name
  }

  private val typeTracker = collection.mutable.MultiDict.empty[String, Seq[String]]
  def addType(name: String, typVarNames: Seq[String]): Unit = {
    typVarNames match {
      case Seq() => Nil
      case _ => typeTracker += ((name, typVarNames))
    }
  }

  /** 
   * Returns the B3-transformed type name for a parametrized type given its name and the names of (concrete) parameters.
   * Also adds them to 
   * 
   * @param name name of the type (without parameters) (e.g. "Field")
   * @param typVarNames parameter names as Seq of Strings. (e.g. ["Int", "Bool"])
   * @return The same name as would be used in boogie, but spaces are replaced by %% (=> e.g. "Field%%Int%%Bool")
   */
  def getNameFromParamTypeConstel(name: String, typVarNames: Seq[String]): String = {
    // info("TODO: NamedType: ", name + " " + typVarNames.mkString(" "))
    addType(name, typVarNames)
    name + "%%" + typVarNames.mkString("%%")
  }

  // STANDALONE NODES
  /** 
   * Scala representation of a B3 Program.
   * You should just use an empty Seq() as input for the following two parameters for now: 
   * 
   * @param signatureTypes not sure what these are for
   * @param domains currently B3's domain system has too many bugs => does not work;
   * Theoretically, a Domain would contain its own "Program", which could be instanciated
   * in the current program by using a DomainInstantiation when using a TypeDecl
   */
  case class Program(signatureTypes: Seq[String], domains: Seq[Domain],
                     types: Seq[TypeDecl], taggers: Seq[Tagger], 
                     functions: Seq[Function], axioms: Seq[Axiom], 
                     procedures: Seq[Procedure]) extends Node {
    def b3fy: RawAst.Program = {
      new RawAst.Program( daf_Set(signatureTypes), // TODO: Find out what "signatureTypes" is
                          daf(Seq[RawAst.Domain]()),//(domains map {_.b3fy}),
                          daf(types map {_.b3fy}),
                          daf(taggers map {_.b3fy}),
                          daf(functions map {_.b3fy}),
                          daf(axioms map {_.b3fy}),
                          daf(procedures map {_.b3fy}))
    }
  }
 
  class Domain extends NOT_SUPPORTED
  class DomainInstantiation extends NOT_SUPPORTED

  /**
   * Scala representation of a B3 RawAst TypeDecl node.
   * 
   * @param name Name of the type
   * @param domain Optional domain instantiation (CURRENTLY NOT SUPPORTED)
   * @return a B3 RawAst TypeDecl node (If domain is used, this corresponds to "type [name] = [domain(parameters)]". Otherwise simply "type [name]")
   */
  case class TypeDecl(name: String, domain: Option[DomainInstantiation] = None) extends Decl {
    def b3fy: RawAst.TypeDecl = {
      val dom = None
      // val dom = domain match {
      //   case Some(domIon) => Some(domIon.b3fy)
      //   case None => None
      // }
      new RawAst.TypeDecl(StringToDString(name), daf(None))
    }
  }

  /** Scala representation of a B3 RawAst Tagger node. (The outputs of functions tagged with the same tag will be disjoint)
   * @param name The tagger's name/identifier
   * @param typ for this type */
  case class Tagger(name: String, typ: Type) extends Decl {
    def b3fy: RawAst.Tagger = new RawAst.Tagger(daf(name), daf(typ.b3fy))
  }

    /**
    * Scala representation of a B3 RawAst Function node.
    *
    * @param name The function name.
    * @param parameters A Seq of the function's parameters (as FParameter)
    * @param resultType The function's return type 
    * @param tag The name of a tag (if any; default "" means no tag). 
    * The values returned by different functions with the same tag are disjoint.
    */
  case class Function(name: String, parameters: Seq[FParameter], resultType: String, tag: String = "") extends Decl {
    def b3fy: RawAst.Function = {
      val optB3Tag = if (tag == "") Option_None[DString] else Option_Some[DString](daf(tag))
      new RawAst.Function(daf(name), 
                          daf(parameters map {_.b3fy}),
                          daf(resultType),
                          optB3Tag,
                          Option_None)  // <- Carbon never uses function bodies/definitions (FunctionDefinition); it defines them using axioms.
    }
  }

  /** Scala representation of a B3 RawAst FParameter node (a function parameter) */
  case class FParameter(name: String, typ: Type, isInjective: Boolean = false) extends Node {
    def b3fy: RawAst.FParameter = new RawAst.FParameter(daf(name), isInjective, daf(typ.b3fy))
  }

  
  /** Scala representation of a B3 RawAst Axiom node. */
  case class Axiom(explains: Seq[String], expr: Expr) extends Decl {
    def b3fy: RawAst.Axiom = new RawAst.Axiom(daf(explains), expr.b3fy)
  }



  /** Scala representation of a B3 RawAst Procedure node */
  case class Procedure(name: String,
                      parameters: Seq[PParameter],
                      pre: Seq[AExpr],
                      post: Seq[AExpr],
                      body: Option[Stmt]) extends Decl {
    def b3fy: RawAst.Procedure = {
      new RawAst.Procedure(daf(name),
                          daf(parameters map {_.b3fy}),
                          daf(pre map {_.b3fy}),
                          daf(post map {_.b3fy}),
                          daf(body map {_.b3fy}))
    }
  }

  /** 
   * Scala representation of a B3 RawAst Variable 
   * (Used for VarDecl, but also nice way to store Var info in general)
   * 
   * @param name The name of the Variable
   * @param typ must either be "bool", "int", or "tag" OR a type defined by a TypeDecl
   * @param isMutable 'var' (true) vs 'val' (false)
   */
  case class Variable(name: String, typ: Type, isMutable: Boolean = true) extends Node {
    val varId: IdExpr = IdExpr(name, typ)
    def b3fy: RawAst.Variable = {
      new RawAst.Variable(daf(name),                  // var name
                          isMutable,                  // "isMutable" => var vs val
                          Option_Some(daf(typ.b3fy)), // "optionalType" => is NOT optional here (since we dont initiate values when using TypeDecl)!
                          Option_None[RawAst.Expr])   // optionalAutoInv => TODO: look if we can use this
    }
  }

  /**
    * Scala representation of a B3 RawAst PParameter node (a Procedure Parameter).
    * Does not support B3's (not fully documented) autoinv feature.
    *
    * @param name Parameter name
    * @param typ Type of the parameter. Must be an in-built type ("bool", "int", and "tag") or have its own TypeDecl
    * @param mode Is it a input (IN), output (OUT), or inout (INOUT) parameter? (IN/INOUT/OUT are variables defined in the current object)
    * @return The corresponding (raw) PParameter
    */
  case class PParameter(name: String, typ: Type, mode: RawAst.ParameterMode = IN) extends Node {
    def b3fy: RawAst.PParameter = new RawAst.PParameter(daf(name), mode, daf(typ.b3fy), Option_None)
  }
  /* All RawAst.ParameterMode versions, for easy use: */
  val IN = new RawAst.ParameterMode_In
  val INOUT = new RawAst.ParameterMode_InOut
  val OUT = new RawAst.ParameterMode_Out
  // TODO: better name ("IN" might be too "common") (can search for RawAst.ParameterMode to find all locations)
  

  // Option Some/None:
  /** creates a B3/Dafny Option->Some instance: "Some(input)" */
  def Option_Some[T](input: T)(implicit ct: ClassTag[T]): Std.Wrappers.Option[T] = {
    Std.Wrappers.Option.create_Some(td[T], input)
  }
  /** creates a B3/Dafny Option->None instance of given Type T */
  def Option_None[T](implicit ct: ClassTag[T]): Std.Wrappers.Option[T] = {
    Std.Wrappers.Option.create_None(td[T])
  }



  // STATEMENT NODES
  /** Corresponds to the Stmt: "'TODO_Stmt_info1': {check true}". Use this if a Stmt is required, but you dont want to implement it yet. */
  def TODO_Stmt(info1: String = "", info2: String = ""): Stmt = {
    B3Development.addTODO(info1, info2)
    LabeledStmt(s"TODO_Stmt_$info1", Block(Seq(Check(TrueLit(), "TODO"))))
  }
  /** Corresponds to the Stmt: "'LATER_Stmt_info1': {check true}". Use this if a Stmt is required, but it is actually an advanced feature. */
  def LATER_Stmt(info1: String = "", info2: String = ""): Stmt = {
    B3Development.addLATER(info1, info2)
    LabeledStmt(s"LATER_Stmt_$info1", Block(Seq(Check(TrueLit(), "LATER"))))
  }
  /** Corresponds to "'ADVANCED_Stmt_info1': {check true}". Use this if a Stmt is required, but it is actually an advanced feature. */
  def ADVANCED_Stmt(info1: String = "", info2: String = ""): Stmt = {
    B3Development.addADVANCED(info1, info2)
    LabeledStmt(s"ADVANCED_Stmt_$info1", Block(Seq(Check(TrueLit(), "ADVANCED"))))
  }

  /** An empty statement. */
  val EmptyStmt: Stmt = Block(Seq())

  sealed trait Stmt extends Node {
    /** All Stmt-versions can implement b3fy with output type RawAst.Stmt instead of
     * their specific stmt version (e.g. RawAst.Stmt_VarDecl), because B3 always expects
     * the RawAst.Stmt type and never a specific RawAst.Stmt_[stmtVersion] in the code.
     * (even if a specific stmt-version is required). */
    def b3fy: RawAst.Stmt
    /* Combines two Stmt's into a Block-Stmt, preserving order. If one of the Stmt's is a
     * Block-Stmt, then the Stmt's inside of it are placed into a new Block-Stmt together 
     * with the other Stmt(s). (Instead of placing the Block-Stmts into the new Block-Stmt) */
    def +++(other: Stmt): Block = combineStmts(this, other)
  }

  /** 
   * Scala representation of a B3 RawAst VarDecl-Stmt node. (introduces a local variable)
   * 
   * Has the 'variable' variable, to access the corresponding Variable node.
   * 
   * @param name The name of the Variable
   * @param body The variable is ONLY in scope in the given body! (Overshadows 'parent' VarDecl's with same name).
   * @param typ must either be "bool", "int", or "tag" OR a type defined by a TypeDecl
   * @param isMutable var (true) vs val (false)
   * @param optInitValue optionally provide the initial value here (in form of an Expression, i.e. Option[Expr])
   */
  case class VarDecl(name: String, body: Stmt, typ: Type, isMutable: Boolean = true, optInitValue: Option[Expr] = None) extends Stmt {
    val variable = Variable(name, typ, isMutable)
    override def b3fy: RawAst.Stmt = new RawAst.Stmt_VarDecl(variable.b3fy, daf(optInitValue map {_.b3fy}) , body.b3fy) // Option_None ==> do not initiate variables (which we never want to do) 
  }


  /** 
   * Scala representation of a B3 RawAst VarDecl-Stmt node. 'lhs := rhs'
   * @param lhr must be the name of a variable in scope (= must be in body of a corresponding VarDecl) 
   */
  case class Assign(lhs: String, rhs: Expr) extends Stmt {
    override def b3fy: RawAst.Stmt_Assign = new RawAst.Stmt_Assign(daf(lhs), rhs.b3fy)
  }

  /** Scala representation of a B3 RawAst Reinit-Stmt node. (= havoc) */
  case class Reinit(vars: Seq[String]) extends Stmt {
    override def b3fy: RawAst.Stmt = new RawAst.Stmt_Reinit(daf(vars))
  }

  /** Scala representation of a B3 RawAst Block-Stmt node. (= Seqn) */
  case class Block(stmts: Seq[Stmt]) extends Stmt {
    override def b3fy: RawAst.Stmt = new RawAst.Stmt_Block(daf(stmts map {_.b3fy}))
  }

  /* (Calls a Procedure)*/
  // case class Call(name: String, args: Seq[CallArgument]) extends Stmt



  //Assertions
  /** Scala representation of a B3 RawAst Check-Stmt node. (Check = Assert, then forget; see B3 manual)
   * @param error currently not supported by B3; we still require it for when that changes */
  case class Check(expr: Expr, error: String) extends Stmt {
    override def b3fy: RawAst.Stmt = new RawAst.Stmt_Check(expr.b3fy)
  }

  /** Scala representation of a B3 RawAst Assume-Stmt node. */
  case class Assume(expr: Expr) extends Stmt {
    override def b3fy: RawAst.Stmt = new RawAst.Stmt_Assume(expr.b3fy)
  }

  /** Advanced Feature (checks if any valid trace exists that reaches that position) */
  // case class Reach extends Stmt

  /** Scala representation of a B3 RawAst Assert-Stmt node. (Assert = "Check + Assume")
   * @param error Currently not supported by B3, but we require it for when that changes */
  case class Assert(expr: Expr, error: String) extends Stmt {
    override def b3fy: RawAst.Stmt = new RawAst.Stmt_Assert(expr.b3fy)
  }

  /** (not documented enough to use) */
  // case class AForall(name: String, typ: Type, body: Stmt) extends Stmt


  //Control flow
  /** Scala representation of a B3 RawAst Choose-Stmt node. (This is basically an "If(*) {} else if (*) {} ... " - Stmt)*/
  case class Choose(branches: Seq[Stmt]) extends Stmt {
    override def b3fy: RawAst.Stmt = new RawAst.Stmt_Choose(daf(branches map {_.b3fy}))
  }
  
  /** Scala representation of a B3 RawAst If-Stmt node. */
  case class If(cond: Expr, thn: Stmt, els: Stmt) extends Stmt {
    override def b3fy: RawAst.Stmt = new RawAst.Stmt_If(cond.b3fy, thn.b3fy, els.b3fy)
  }

  /** Unused (~"if case b1 stmt1, case b2 stmt2, ...") */
  // case class IfCase(node: RawAst.Stmt_IfCase) extends B3Stmt
  
  /** Scala representation of a B3 RawAst Loop-Stmt node. (= do forever...until Exit-stmt) 
   * TODO: check how to implement sil.While using this (instead of the current approach where
   * no while is used) */
  case class Loop(invariants: Seq[AExpr], body: Stmt) extends Stmt {
    override def b3fy: RawAst.Stmt = new RawAst.Stmt_Loop(daf(invariants map {_.b3fy}), body.b3fy)
  }

  /** Scala representation of a B3 RawAst Loop-Stmt node. 
   * Labels are not allowed to shadow other labels. TODO: decribe allowed names */
  case class LabeledStmt(lbl: String, body: Stmt) extends Stmt {
    override def b3fy: RawAst.Stmt = new RawAst.Stmt_LabeledStmt(daf(lbl), body.b3fy)
  }

  // case class Exit extends Stmt
  // case class Return extends Stmt

  //Error reporting
  // case class Probe extends Stmt


  // MORE GENERAL NODES
  /* Procedure-Call Argument */
  // case class CallArgument(mode: ParameterMode, arg: Expr) extends Node

  /* An IfCase-Stmt Case */
  // case class Case extends Node


  /* pre-/post-conditions and loop-invariants */
  sealed trait AExpr extends Node {
    def b3fy: RawAst.AExpr
  }
  /** Scala representation of a B3 RawAst AExpression-AExpr node. 
   * TODO: rules for this (see manual)!
   * (pre-/post-conditions and loop-invariants) */
  case class AExpression(expr: Expr) extends AExpr {
    override def b3fy: RawAst.AExpr = new RawAst.AExpr_AExpr(expr.b3fy)
  }
  /** Scala representation of a B3 RawAst AAssertion-AExpr node. 
   * TODO: rules for this (see manual)!
   * (pre-/post-conditions and loop-invariants) */
  case class AAssertion(stmt: Stmt) extends AExpr {
    override def b3fy: RawAst.AExpr = new RawAst.AExpr_AAssertion(stmt.b3fy)
  }



  // EXPRESSION NODES

  /** Corresponds to the "'TODO_Expr_bool_info1': true" (labeled) bool-Expr. 
   * Use these if a bool expr is required, but you dont want to implement it yet. */
  def TODO_Expr_bool(info1: String = "", info2: String = ""): Expr = {
    B3Development.addTODO(info1, info2)
    LabeledExpr(s"TODO_Expr_bool_$info1", TrueLit())
  }
  /** Corresponds to the "'TODO_Expr_int_info1': 666" (labeled) int-Expr. 
   * Use these if a int expr is required, but you dont want to implement it yet. */
  def TODO_Expr_int(info1: String = "", info2: String = ""): Expr = {
    B3Development.addTODO(info1, info2)
    LabeledExpr(s"TODO_Expr_bool_$info1", IntLit(666))
  }
  /** Corresponds to the "'LATER_Expr_bool_info1': true" (labeled) bool-Expr. 
   * Use these if a bool expr is required, but you dont want to implement it yet. */
  def LATER_Expr_bool(info1: String = "", info2: String = ""): Expr = {
    B3Development.addLATER(info1, info2)
    LabeledExpr(s"LATER_Expr_bool_$info1", TrueLit())
  }
  /** Corresponds to the "'LATER_Expr_int_info1': 666" (labeled) int-Expr. 
   * Use these if a int expr is required, but you dont want to implement it yet. */
  def LATER_Expr_int(info1: String = "", info2: String = ""): Expr = {
    B3Development.addLATER(info1, info2)
    LabeledExpr(s"LATER_Expr_bool_$info1", IntLit(666))
  }
  /** Corresponds to the "'ADVANCED_Expr_bool_info1': true" (labeled) bool-Expr. 
   * Use these if a bool expr is required, but you dont want to implement it yet. */
  def ADVANCED_Expr_bool(info1: String = "", info2: String = ""): Expr = {
    B3Development.addADVANCED(info1, info2)
    LabeledExpr(s"ADVANCED_Expr_bool_$info1", TrueLit())
  }
  /** Corresponds to the "'ADVANCED_Expr_int_info1': 666" (labeled) int-Expr. 
   * Use these if a int expr is required, but you dont want to implement it yet. */
  def ADVANCED_Expr_int(info1: String = "", info2: String = ""): Expr = {
    B3Development.addADVANCED(info1, info2)
    LabeledExpr(s"ADVANCED_Expr_bool_$info1", IntLit(666))
  }


  sealed trait Expr extends Node {
    def b3fy: RawAst.Expr
    
    def ===(other: Expr) = OperatorExpr(EqCmp, Seq(this, other))
    def !==(other: Expr) = OperatorExpr(NeCmp, Seq(this, other))
    def :=(rhs: Expr) = this match {
      case fst: IdExpr => Assign(fst.name, rhs)
      case fst => sys.error("FAIL: Using Expr.':=' operator, which expects lhs to be an IdExpr, but it was " + fst.getClass.getName)
    }
    def +=(rhs: Expr) = this match {
      case fst: IdExpr => Assign(fst.name, fst + rhs)
      case fst => sys.error("FAIL: Using '+=' operator, which expects lhs to be an IdExpr, but it was " + fst.getClass.getName)
    }
    def -=(rhs: Expr) = this match {
      case fst: IdExpr => Assign(fst.name, fst - rhs)
      case fst => sys.error("FAIL: Using '-=' operator, which expects lhs to be an IdExpr, but it was " + fst.getClass.getName)
    }
    def +(other: Expr) = OperatorExpr(Add, Seq(this, other))
    def -(other: Expr) = OperatorExpr(Sub, Seq(this, other))
    def *(other: Expr) = OperatorExpr(Mul, Seq(this, other))
    def /(other: Expr) = OperatorExpr(Div, Seq(this, other))
    def div(other: Expr) = OperatorExpr(IntDiv, Seq(this, other))
    def %(other: Expr) = OperatorExpr(Mod, Seq(this, other))
    def <(other: Expr) = OperatorExpr(LtCmp, Seq(this, other))
    def >(other: Expr) = OperatorExpr(LtCmp, Seq(other, this)) // this > other => other < this
    def <=(other: Expr) = OperatorExpr(LeCmp, Seq(this, other))
    def >=(other: Expr) = OperatorExpr(LeCmp, Seq(other, this)) // this >= other => other <= this
    def neg = OperatorExpr(Minus, Seq(this))
    def &&(other: Expr) = OperatorExpr(And, Seq(this, other))
    def ||(other: Expr) = OperatorExpr(Or, Seq(this, other))
    def ==>(other: Expr) = OperatorExpr(Implies, Seq(this, other))
    def <==>(other: Expr) = OperatorExpr(Equiv, Seq(this, other))
    def forall(vars: Seq[Binding], triggers: Seq[Pattern]) =
      Forall(vars, triggers, this)
    def exists(vars: Seq[Binding], triggers: Seq[Pattern]) =
      Exists(vars, triggers, this)
    def not = OperatorExpr(Not, Seq(this))
    def thn(thn: Expr) = new PartialCondExpr(this, thn)

    // def transform(f: PartialFunction[Expr, Option[Expr]]) = Nodes.transform(this, f)
    
    class PartialCondExpr(cond: Expr, thn: Expr) {
      def els(els: Expr) = OperatorExpr(CondExp, Seq(cond, thn, els))
    }
  }

  /**
   * "adds" ':=' operator method to String by automatically packaging it inside 
   * this class and using the methods of this class whenever that is called
   * on a String. 
   */
  implicit class B3AssignOpWrapper(val first: String) {
    def :=(rhs: Expr) = Assign(first, rhs)
  }

  /** Scala representation of a B3 RawAst BLiteral-Expr node. (Boolean values) */
  sealed abstract class BoolLit(val b: Boolean) extends Expr {
    override def b3fy: RawAst.Expr = new RawAst.Expr_BLiteral(b)
  }
  object BoolLit {
  def unapply(b: BoolLit) = Some(b.b)
    def apply(b: Boolean) = if (b) TrueLit() else FalseLit()
    // use .b if you want to know the boolean value
  }
  /** Scala representation of a B3 RawAst BLiteral(true)-Expr node. */
  case class TrueLit() extends BoolLit(true)
  /** Scala representation of a B3 RawAst ILiteral(false)-Expr node. */
  case class FalseLit() extends BoolLit(false)

  /** Scala representation of a B3 RawAst ILiteral-Expr node. (Integer values) */
  case class IntLit(x: BigInt) extends Expr {
    override def b3fy: RawAst.Expr = new RawAst.Expr_ILiteral(x.bigInteger)
  }

  /* CustomLiteral  =^=  "|" LiteralIdentifier ":" Type "|"  =>  not what we want; we use nonary functions if we have to. */
  // case class CustomLiteral(name: String, typ: Type) extends Expr

  /** (≃ LocalVar) Scala representation of a B3 RawAst IdExpr-Expr node. 
   * @param typ is only there for internal type tracking and is not used by B3
  */
  case class IdExpr(name: String, typ: Type, isOld: Boolean = false) extends Expr {
    override def b3fy: RawAst.Expr = new RawAst.Expr_IdExpr(daf(name), isOld)
  }

  /** Scala representation of a B3 RawAst OperatorExpr-Expr node. 
   * @param op RawAst Operators are provided as values by the current Object (use ObjectName.{Op-name})
   */
  case class OperatorExpr(op: RawAst.Operator, exprs: Seq[Expr]) extends Expr {
    override def b3fy: RawAst.Expr = new RawAst.Expr_OperatorExpr(op, daf(exprs map {_.b3fy}))
  }
  /* For easy use of B3 operators: (same names as in Silver) */
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

  /** Scala representation of a B3 RawAst FunctionCallExpr-Expr node. 
   * The function must be defined in the current Program with matching number of args.
   * @param typ is only for internal purposes */
  case class FunctionCallExpr(name: String, args: Seq[Expr], typ: Type) extends Expr {
    override def b3fy: RawAst.Expr = new RawAst.Expr_FunctionCallExpr(daf(name), daf(args map {_.b3fy}))
  }

  /** Scala representation of a B3 RawAst IdExpr-Expr node.
   * 
   * Notes on Labeled expression: = "{label}: {expr}", where label can be used to provide information, 
   * but has otherwise no functional purpose. Here, the label can be anything (can have spaces and use
   * any character; in written B3 code this is probably not the case, so B3-printout of the code can 
   * not be reused if these labels are chosen arbitrarily. Although, maybe only ":" leads to problems)
   */
  case class LabeledExpr(label: String, expr: Expr) extends Expr {
    override def b3fy: RawAst.Expr = new RawAst.Expr_LabeledExpr(daf(label), expr.b3fy)
  }


  sealed trait QuantifiedExpr extends Expr {
    def vars: Seq[Binding]
    def expr: Expr
  }
  /**
    * Scala representation of a B3 RawAst Quantifier(Forall)-Expr node.
    *
    * @param vars Seq of B3 Bindings (bound variables (w/ name + type))
    * @param patterns Seq of patterns for pattern-matching. (same as Silver Trigger's)
    * @param expr The body. Can use any variables in scope, including the ones defined by 'bindings'
    */
  case class Forall(vars: Seq[Binding], patterns: Seq[Pattern], expr: Expr) extends QuantifiedExpr {
    override def b3fy: RawAst.Expr = new RawAst.Expr_QuantifierExpr(true, daf(vars map {_.b3fy}), daf(patterns map {_.b3fy}), expr.b3fy)
  } 
  /**
    * Scala representation of a B3 RawAst Quantifier(Exists)-Expr node.
    *
    * @param vars Seq of B3 Bindings (bound variables (w/ name + type))
    * @param patterns Seq of patterns for pattern-matching. (same as Silver Trigger's)
    * @param expr The body. Can use any variables in scope, including the ones defined by 'bindings'
    */
  case class Exists(vars: Seq[Binding], patterns: Seq[Pattern], expr: Expr) extends QuantifiedExpr {
    override def b3fy: RawAst.Expr = new RawAst.Expr_QuantifierExpr(false, daf(vars map {_.b3fy}), daf(patterns map {_.b3fy}), expr.b3fy)
  } 




  // MORE GENERAL NODES
  /** Scala representation of a B3 RawAst Binding node. ("VarDecl" for bound variables in exist/forall expressions). */
  case class Binding(name: String, typ: Type) extends Node {
    def b3fy: RawAst.Binding = new RawAst.Binding(daf(name), daf(typ.b3fy))
  }

  /** (= Trigger) Scala representation of a B3 RawAst Pattern node. (equivalent to a sil.Trigger; for pattern-matching in forall/exists). */
  case class Pattern(exprs: Seq[Expr]) extends Node {
    def b3fy: DafnySequence[RawAst.Expr] = daf(exprs map {_.b3fy}) // We dont use "new RawAst.Pattern(daf(exprs map {_.b3fy}))" because QuantifierExpr expects DafnySequence[_ <: RawAst.Expr] instead of RawAst.Pattern"
  }

  // case class ClosureBinding extends Node
  // case class ClosureProperty extends Node


  /** 
   * Combines the two statements into a single statement block. The order of the stmts is preserved.
   * 
   * If (at least) one of them is a statement block, then the statements in it
   * are placed into a new statement block together with the other statement(s). 
   * (Instead of placing the statement block - stmt in the new statement block)
   */
  def combineStmts(e1: Stmt, e2: Stmt): Block = {
    (e1, e2) match {
      case (Block(stmts1), Block(stmts2)) => Block(stmts1 ++ stmts2)
      case (Block(stmts1), stmt2) => Block(stmts1 :+ stmt2)
      case (stmt1, Block(stmts2)) => Block(stmt1 +: stmts2)
      case (stmt1, stmt2) => Block(Seq(stmt1, stmt2))
    }
  }
}

/** For development purposes */
object B3Development {
  val todos = mutable.Set.empty[(String, String)]
  val laters = mutable.Set.empty[(String, String)]
  val advanced = mutable.Set.empty[(String, String)]
  def reset(): Unit = {
    todos.clear()
    laters.clear()
    advanced.clear()
  }
  def addTODO(info1: String = "", info2: String = ""): Unit = {
    if (info1 != "") {
      todos.add((info1, info2))
    }
  }
  def addLATER(info1: String = "", info2: String = ""): Unit = {
    if (info1 != "") {
      laters.add((info1, info2))
    }
  }
  def addADVANCED(info1: String = "", info2: String = ""): Unit = {
    if (info1 != "") {
      advanced.add((info1, info2))
    }
  }
  def printTODO(): Unit = {
    val grouped = todos.groupMap(_._1)(_._2)
    println("vvv TODO INFO vvv")
    grouped.foreach { case (main, detailSet) =>
      println(s"=> $main:\n  ${detailSet.mkString(", ")}")
    }
    println("^^^ TODO INFO ^^^")
  }
  def printLATER(): Unit = {
    val grouped = laters.groupMap(_._1)(_._2)
    println("vvv LATER INFO vvv")
    grouped.foreach { case (main, detailSet) =>
      println(s"=> $main:\n  ${detailSet.mkString(", ")}")
    }
    println("^^^ LATER INFO ^^^")
  }
  def printADVANCED(): Unit = {
    val grouped = advanced.groupMap(_._1)(_._2)
    println("vvv ADVANCED INFO vvv")
    grouped.foreach { case (main, detailSet) =>
      println(s"=> $main:\n  ${detailSet.mkString(", ")}")
    }
    println("^^^ ADVANCED INFO ^^^")
  }
  def printALL(): Unit = {
    printTODO()
    printLATER()
    printADVANCED()
  }
}

/**
 * A collection of implicits for working with the B3 AST.
 */
object B3Implicits {
  import viper.carbon.b3.B3Nodes._
  import _root_.scala.language.implicitConversions

  // DAFNY HELPERS
  implicit def dseqToSeq[T](dseq: DafnySequence[_ <: T]): Seq[T] = {
    dseq.asScala.toSeq
  }
  
  implicit def dafStringToString(dstr: DafnySequence[_ <: CodePoint]): String = {
    val sb = new java.lang.StringBuilder
    dstr.asScala.foreach(cp => sb.appendCodePoint(cp.value()))
    sb.toString
  }

  // GENERAL Seq HELPERS
  implicit def lift[T](t: T): Seq[T] = Seq(t)
  implicit def liftStmt(ss: Seq[Stmt]): Block = Block(ss)
  implicit def liftSeq(ss: Seq[Expr]): BoolSeq = new BoolSeq(ss)

  /**
   * Adds methods to turn a sequence of Expressions into their conjunction or disjunction.
   */
  class BoolSeq(xs: Seq[Expr]) {
    /** Returns the conjunction of all Expressions, or 'true' if the sequence is empty'. */
    def all: Expr = all(xs).getOrElse(TrueLit())
    /** Returns the conjunction of all Expressions, or 'None' if the sequence is empty'. */
    def allOption: Option[Expr] = all(xs)

    /** Returns the disjunction of all Expressions, or 'false' if the sequence is empty'. */
    def any: Expr = any(xs).getOrElse(TrueLit())
    /** Returns the disjunction of all Expressions, or 'None' if the sequence is empty'. */
    def anyOption: Option[Expr] = any(xs)

    private def any(xss: Seq[Expr]): Option[Expr] = {
      xss match {
        case Nil => None
        case Seq(x) => Some(x)
        case Seq(x, y) => Some(OperatorExpr(Or, Seq(x, y)))
        case x +: xs => Some(OperatorExpr(Or, Seq(x, all(xs).get)))
      }
    }

    private def all(xss: Seq[Expr]): Option[Expr] = {
      xss match {
        case Nil => None
        case Seq(x) => Some(x)
        case Seq(x, y) => Some(OperatorExpr(And, Seq(x, y)))
        case x +: xs => Some(OperatorExpr(And, Seq(x, all(xs).get)))
      }
    }
  }




  // IDENTIFIER UNIQUENESS (and other properties; stolen from PrettyPrinter; 'ident2doc' -> 'idName')
  /** The current mapping from identifier to names. */
  private val idnMap = collection.mutable.HashMap[Identifier, String]()

  /** B3NameGenerator instance. */ 
  private val names = new B3NameGenerator()
  /**
    * The current mapping from unique B3 names to the original identifiers (inverse mapping of idnMap,
    * where the names of the identifiers are used directly).
    */
  val backMap = collection.mutable.HashMap[String, Identifier]()

  /** Map an identifier to a string, making it unique first if necessary. */
  implicit def idName(i: Identifier): String = {
    idnMap.get(i) match {
      case Some(s) => s
      case None =>
        val s = names.createUniqueIdentifier(i.preferredName)
        idnMap.put(i, s)
        backMap.update(s, i)
        s
    }
  }
}


/**
 * An identifier of a Boogie program.  Creators of identifiers must make sure that
 * names from the same category are unique in any given program (otherwise, the two
 * identifiers refer to the same thing), but the pretty-printer then tries to use
 * the name `preferredName` if possible.
 */
trait Identifier {
  def name: String
  def namespace: Namespace
  def preferredName = name
  override def equals(o: Any) = {
    o match {
      case Identifier(n, ns) => n == name && ns == namespace
      case _ => false
    }
  }
  override def hashCode = List(name, namespace).hashCode
}
case object Identifier {
  def apply(n: String)(implicit ns: Namespace): Identifier =
    new Identifier {
      val name = n
      val namespace = ns
    }
  def unapply(i: Identifier) = Some(i.name, i.namespace)
}
/**
 * A namespace to make it easier to avoid duplicated entities in the Boogie output.
  *
  * @param name The name of the namespace; only used for debugging purposes.
 * @param id The ID of this namespace; used to identify the namespace.
 */
case class Namespace(name: String, id: Int)

object ErrorMemberMapping {
  // The "weak" hash map is necessary to avoid leaking memory.
  // See issue https://github.com/viperproject/carbon/issues/444
  val mapping = mutable.WeakHashMap[VerificationError, Member]()
  var currentMember : Member = null
}