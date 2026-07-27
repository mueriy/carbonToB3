package viper.carbon.b3
import dafny._
import scala.jdk.CollectionConverters._
import scala.reflect.ClassTag
import scala.collection.mutable
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
  import viper.carbon.b3.B3Naming._

  /** 
   * The root of the Scala-B3 AST.
   * 
   * All nodes must have the b3fy method, which returns the corresponding RawAst "node".
   * This includes transforming all subnodes correctly (can use their own b3fy method for that).
   */
  sealed trait Node {
    //(Taken from boogie.scala -> Node) vvv
    /**
     * Returns a list of all direct sub-nodes of this node.
     */
    lazy val subnodes = Nodes.subnodes(this)

    /**
     * Optimize a program or expression
     */
    lazy val optimized: Node = Optimizer.optimize(this)

    /**
     * Applies the function `f` to the node and the results of the subnodes.
     */
    def reduce[T](f: (Node, Seq[T]) => T) = Visitor.reduce[T](this)(f)

    /**
     * More powerful version of reduce that also carries a context argument through the tree.
     */
    def reduce[C, R](context: C, enter: (Node, C) => C, combine: (Node, C, Seq[R]) => R) = {
      Visitor.reduce[C, R](this)(context, enter, combine)
    }
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

  /** Decl includes: [[Domain]], [[TypeDecl]], [[Tagger]], [[Function]], [[Axiom]], and [[Procedure]]. (B3's 'TypeName' is String here) */
  sealed trait Decl extends Node 
  /** LocalVarDecl includes: [[Binding]], [[PParameter]], [[FParameter]], and [[VarDecl]](which also extends Stmt) */
  sealed trait LocalVarDecl extends Node {
    /* B3 INFO: splitting the LocalVarDecl into the different "B3 versions" makes sense because they have/need different 
    parameters (see in the following). However, Boogie-Carbon naturally always used LocalVarDecl, and in some cases
    we need two different variants. Currently this is solved by adding def F/P/Q to the case-classes, which returns the 
    corresponding FParameter/PParameter/(Quantifier-)Binding, respectively. */

    // Binding(name: Identifier, typ: Type)
    // PParameter(name: Identifier, typ: Type, mode: RawAst.ParameterMode = IN)
    // FParameter(name: Identifier, typ: Type, isInjective: Boolean = false)
    // VarDecl(name: Identifier, body: Stmt, typ: Type, isMutable: Boolean = true, optInitValue: Option[Expr] = None)
    
    // Enable use of .name and .typ even if we don't know which specific case-variant of LocalVarDecl we have: 
    def name: Identifier
    def typ: Type
    /* returns the identifier expression of this locally declared variable */
    def l = IdExpr(name, typ)
  } 

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
  /** Temporarily represented by int. B3 TODO: change this to real as soon as that is possible */
  case object Real extends BuiltInType { override def b3fy: String = "int" }
  /** This is (currently) the same as Real, but please use this for permissions. */
  case object Perm extends BuiltInType { override def b3fy: String = Real.b3fy }
  /** This is only for temporary use. Using it in the final Program version will almost certainly fail. */
  case class TypeVar(name: String) extends Type {
    override def freeTypeVars: Seq[TypeVar] = Seq(this)
    override def b3fy: String = name
  }
  case class NamedType(n: String, typVars: Seq[Type] = Nil) extends Type {
    override def freeTypeVars: Seq[TypeVar] = typVars flatMap (_.freeTypeVars)
    // must be lazy because during setup heapTypVarsToIdx is set up AFTER creating the first
    // 'NamedType', but would already be needed at that point
    lazy val name = (n, typVars) match {
      case ("Perm", _) => "int" // B3 TODO: change "int" to "real" as soon as that is possible
      case (_, Seq()) => n
      case _ => getNameFromParamTypeConstel(n, typVars)
    } 
    override def b3fy: String = name
  }

  private val typeTracker = collection.mutable.MultiDict.empty[String, Seq[Type]]
  def addType(name: String, typVars: Seq[Type]): Unit = {
    typeTracker += ((name, typVars))
  }

  /** 
   * Returns the B3-transformed type name for a parametrized type given its name and the names of (concrete) parameters.
   * Also adds them to 
   * 
   * @param name name of the type (without parameters) (e.g. "Field")
   * @param typVarNames parameter names as Seq of Strings. (e.g. ["Int", "Bool"])
   * @return The same name as would be used in boogie, but spaces are replaced by %% (=> e.g. "Field%%Int%%Bool")
   */
  def getNameFromParamTypeConstel(name: String, typVars: Seq[Type]): String = {
    addType(name, typVars)
    if (name.startsWith("%") || name == "Field") {
      name + "%%" + heapTypVarsToIdx(typVars)
    } else name + "%%" + (typVars map {_.b3fy}).mkString("%%")
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
 
  class Domain extends NOT_SUPPORTED with Decl
  class DomainInstantiation extends NOT_SUPPORTED

  /**
   * Scala representation of a B3 RawAst TypeDecl node.
   * 
   * @param name Name of the type
   * @param domain Optional domain instantiation (CURRENTLY NOT SUPPORTED)
   * @return a B3 RawAst TypeDecl node (If domain is used, this corresponds to "type [name] = [domain(parameters)]". Otherwise simply "type [name]")
   */
  case class TypeDecl(name: Type, domain: Option[DomainInstantiation] = None) extends Decl {
    def b3fy: RawAst.TypeDecl = {
      val dom = None
      // val dom = domain match {
      //   case Some(domInst) => Some(domInst.b3fy)
      //   case None => None
      // }
      new RawAst.TypeDecl(StringToDString(name.b3fy), daf(dom))
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
   * @param args A Seq of the function parameters
   * @param resultType The function's return type 
   * @param tag The optional name of a tag (functions with the same tag return disjoint values).
   * @param body 
   */
  case class Function(name: Identifier, args: Seq[FParameter], typ: Type, tag: Option[String] = None, body: Option[FunctionDef] = None) extends Decl {
    // Automatically register this function
    registerFunction(this)
    
    def b3fy: RawAst.Function = {
      new RawAst.Function(daf(functionName), 
                          daf(args map {_.b3fy}),
                          daf(typ.b3fy),
                          daf(tag map {daf(_)}),
                          daf(body map {_.b3fy}))
    }
    def functionName: String = funcName(this)
    def isPure: Boolean = typ.freeTypeVars.size == 0
  }

  /** Scala representation of a B3 RawAst FParameter node (a function parameter) */
  case class FParameter(name: Identifier, typ: Type, isInjective: Boolean = false) extends LocalVarDecl {
    def b3fy: RawAst.FParameter = new RawAst.FParameter(daf(name), isInjective, daf(typ.b3fy))
    def toQ: Binding = Binding(name, typ)
    def toP(mode: RawAst.ParameterMode = IN): PParameter = PParameter(name, typ, mode)
  }

  case class FunctionDef(body: Expr, when: Seq[Expr] = Seq()) extends Node {
    def b3fy: RawAst.FunctionDefinition = new RawAst.FunctionDefinition(daf(when map {_.b3fy}), body.b3fy)
  }

  /**
   * This represents a single, constant value. In reality this just creates a nonary Function Node, but
   * using this increases Carbon's readability by differencing between "real" functions and values.
   * Note that these Functions never need to be manually registered (because they don't *need* to be registered).
   *
   * @param name The constant/function name.
   * @param resultType The constant/function's return type 
   * @param tag The optional name of a tag (constants/functions with the same tag return disjoint values).
   * The values returned by different functions with the same tag are disjoint.
   */
  object ConstDecl {
    def apply(name: Identifier, typ: Type, tag: Option[String] = None, body: Option[FunctionDef] = None): Function =
      Function(name, Seq(), typ, tag, body)
  }

  
  /** 
   * Scala representation of a B3 RawAst Axiom node. 
   * @param explains If an explains is provided, that is used later. If none is provided
   * it will be (tried to be) created when transforming it to its RawAst form (TODO). 
   * Technically, every 'explains' that can be manually created should also be able to be
   * created automatically, so in the future there should be no need to declare it beforehand.
   * (Although, for preamble-axioms we might know that we use one axiom only when we use another, 
   * so we might be able to add some "explanations" of the required axiom to the other one, 
   * which could help if the other axiom has many different pattern variants, making it hard or
   * impossible to define an 'explains' that works.)
   * Defining it beforehand can make it more complicated for "parametric" axioms and other 
   * cases where we will (or might) modify the axiom. Therefore, it might even make sense
   * to remove this parameter at some point.
   */
  case class Axiom(expr: Expr, explains: Seq[Identifier] = Seq()) extends Decl {
    // Automatic creation of 'explains' is only done at the very end (when b3fy is called),
    // to ensure that we can generate all function names correctly.
    def b3fy: RawAst.Axiom = explains match {
      case Seq() => new RawAst.Axiom(daf(axiomExplanations(this)), expr.b3fy)
      case seq => new RawAst.Axiom(daf(seq map idName), expr.b3fy)
    }

    // def isPure: Boolean = typ.freeTypeVars.size == 0

    /** 
     * Auto-generates "explains"-identifiers for axioms
     * TODO: support more complicated cases.
     */
    def axiomExplanations(ax: Axiom): Seq[String] = {
      ax.expr match {
        case Forall(vars, patterns, expr, _, _) => patterns match {
          case Seq(Pattern(pExprs)) => pExprs map {expr => funcName(expr.asInstanceOf[FunctionCallExpr])}
          case _ => B3Development.addLATER("axiomExplanations", "multiple (alternative) patterns currently not supported"); Seq()
        }
        case _ => B3Development.addLATER("axiomExplanations", "axioms containing not just a forall currently not supported"); Seq()
      }
    }
  }



  /** Scala representation of a B3 RawAst Procedure node */
  case class Procedure(name: Identifier,
                      parameters: Seq[PParameter],
                      body: Option[Stmt],
                      pre: Seq[AExpr] = Seq(),
                      post: Seq[AExpr] = Seq()) extends Decl {
    def b3fy: RawAst.Procedure = {
      new RawAst.Procedure(daf(name),
                          daf(parameters map {_.b3fy}),
                          daf(pre map {_.b3fy}),
                          daf(post map {_.b3fy}),
                          daf(body map {_.b3fy}))
    }
  }

  /** 
   * Scala representation of a B3 RawAst Variable (B3 uses this to define the created variable
   * in VarDecl, but we can also use it to store Var info in general, although LocalVarDecl
   * is often a P/F-Parameter)
   * 
   * @param name The name of the Variable
   * @param typ must either be "bool", "int", or "tag" OR a type defined by a TypeDecl
   * @param isMutable 'var' (true) vs 'val' (false)
   */
  case class Variable(name: Identifier, typ: Type, isMutable: Boolean = true) extends Node {
    def l: IdExpr = IdExpr(name, typ)
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
  case class PParameter(name: Identifier, typ: Type, mode: RawAst.ParameterMode = IN) extends LocalVarDecl {
    def b3fy: RawAst.PParameter = new RawAst.PParameter(daf(name), mode, daf(typ.b3fy), Option_None)
    def toQ: Binding = Binding(name, typ)
    def toF(isInjective: Boolean = false): FParameter = FParameter(name, typ, isInjective) 
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


  import viper.silver.verifier.errors.AssertFailed
  import viper.silver.verifier.reasons.FeatureUnsupported
  import viper.silver.ast.{Assert=>SilAssert, TrueLit=>SilTrueLit}
  def fakeError(msg: String) = AssertFailed(SilAssert(SilTrueLit()())(), FeatureUnsupported(SilAssert(SilTrueLit()())(), msg)) 
  // STATEMENT NODES
  /** Corresponds to the Stmt: "'TODO_Stmt_info1': {}" ({} = empty Block-stmt). Use this if a Stmt is required, but you dont want to implement it yet. */
  def TODO_Stmt(info1: String = "", info2: String = ""): Stmt = {
    B3Development.addTODO(info1, info2)
    val info2inlc = if(INLCUDE_SECOND_MSG){s", \"$info2\""} else {""}
    LabeledStmt(s"TODO_Stmt(\"$info1\"$info2inlc)", Block(Seq()))
  }
  /** Corresponds to the Stmt: "'LATER_Stmt_info1': {}" ({} = empty Block-stmt). Use this if a Stmt is required, but it is actually an advanced feature. */
  def LATER_Stmt(info1: String = "", info2: String = ""): Stmt = {
    B3Development.addLATER(info1, info2)
    val info2inlc = if(INLCUDE_SECOND_MSG){s", \"$info2\""} else {""}
    LabeledStmt(s"LATER_Stmt(\"$info1\"$info2inlc)", Block(Seq()))
  }
  /** Corresponds to the Stmt: "'ADVANCED_Stmt_info1': {}" ({} = empty Block-stmt). Use this if a Stmt is required, but it is actually an advanced feature. */
  def ADVANCED_Stmt(info1: String = "", info2: String = ""): Stmt = {
    B3Development.addADVANCED(info1, info2)
    val info2inlc = if(INLCUDE_SECOND_MSG){s", \"$info2\""} else {""}
    LabeledStmt(s"ADVANCED_Stmt(\"$info1\"$info2inlc)", Block(Seq()))
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
    /**
     * Returns a list of all actual statements contained in this statement.  That
     * is, all statements except `Block`, including statements in the body of loops, etc.
     */
    def children = Statements.children(this)
    /**
     * Returns a list of all undeclared local variables contained in this statement and
     * throws an exception if the same variable is used with different types.
     */
    def undeclLocalVars: Seq[IdExpr] = Statements.undeclLocalVars(this)
  }

  /** 
   * Scala representation of a B3 RawAst VarDecl-Stmt node. (introduces a local variable)
   * 
   * Has the field [[variable]], which contains the corresponding [[Variable]] Node.
   * 
   * @param name The name of the variable
   * @param body The variable is ONLY in scope in the given body! (Overshadows 'parent' VarDecl's with same name).
   * @param typ must either be "bool", "int", or "tag" OR a type defined by a [[TypeDecl]]
   * @param isMutable var (true) vs val (false)
   * @param optInitValue optionally provide the initial value here (in form of an Expression, i.e. Option[Expr])
   */
  case class VarDecl(name: Identifier, body: Stmt, typ: Type, isMutable: Boolean = true, optInitValue: Option[Expr] = None) extends Stmt with LocalVarDecl {
    val variable = Variable(name, typ, isMutable)
    override def b3fy: RawAst.Stmt = new RawAst.Stmt_VarDecl(variable.b3fy, daf(optInitValue map {_.b3fy}) , body.b3fy) // Option_None ==> do not initiate variables (which we never want to do) 
  }


  /** 
   * Scala representation of a B3 RawAst Assign-Stmt node. 'lhs := rhs'
   * @param lhr must be the name of a variable in scope (= must be in body of a corresponding VarDecl) 
   */
  case class Assign(lhs: IdExpr, rhs: Expr) extends Stmt {
    override def b3fy: RawAst.Stmt_Assign = new RawAst.Stmt_Assign(daf(lhs.name), rhs.b3fy)
  }

  /** Scala representation of a B3 RawAst Reinit-Stmt node. (= Havoc) */
  case class Reinit(vars: Seq[IdExpr]) extends Stmt {
    override def b3fy: RawAst.Stmt = new RawAst.Stmt_Reinit(daf(vars map {v => idName(v.name)}))
  }

  /** Scala representation of a B3 RawAst Block-Stmt node. (= Seqn) */
  case class Block(stmts: Seq[Stmt]) extends Stmt {
    override def b3fy: RawAst.Stmt = new RawAst.Stmt_Block(daf(stmts map {_.b3fy}))
  }

  /* (Calls a Procedure)*/
  // case class Call(name: Identifier, args: Seq[CallArgument]) extends Stmt



  //Assertions
  /** Scala representation of a B3 RawAst Check-Stmt node. (Check = Assert, then forget; see B3 manual)
   * @param error currently not supported by B3; we still require it for when that changes */
  case class Check(expr: Expr, error: VerificationError) extends Stmt {
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
  case class Assert(expr: Expr, error: VerificationError) extends Stmt {
    override def b3fy: RawAst.Stmt = new RawAst.Stmt_Assert(expr.b3fy)
  }

  /** (not documented enough to use) */
  // case class AForall(name: String?, typ: Type, body: Stmt) extends Stmt


  //Control flow
  /** 
   * Scala representation of a B3 RawAst Choose-Stmt node. (This is basically an "If(\*) {} else if (\*) {} ... "/NondetIf - Stmt).
   * 
   * Careful! Implicit liftSeq (Seq -> Block) does NOT work here (since we expect Seq, not Block!)
   * 
   * @param branches If only a single Stmt is given, an empty branch is automatically added as alternative branch-option. 
   *  (If you want to only use one option, then you have no need for Choose!) 
   */
  case class Choose(branches: Seq[Stmt]) extends Stmt {
    private def addElse = if (branches.length == 1) {Seq(EmptyStmt.b3fy)} else {Seq()} //If only 1 stmt is given then the idea is ALWAYS
    override def b3fy: RawAst.Stmt = new RawAst.Stmt_Choose(daf((branches map {_.b3fy}) ++ addElse))
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
   * Labels are not allowed to shadow other labels. TODO: decribe allowed names 
   * ADVANCED: To actually support labels (for goto, etc.) we will need Identifier instead of String */
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
  val INLCUDE_SECOND_MSG = true
  /** Corresponds to the "'TODO_Expr_bool_info1': true" (labeled) bool-Expr. 
   * Use these if a bool expr is required, but you dont want to implement it yet. */
  def TODO_Expr_bool(info1: String = "", info2: String = ""): Expr = {
    B3Development.addTODO(info1, info2)
    val info2inlc = if(INLCUDE_SECOND_MSG){s", \"$info2\""} else {""} 
    LabeledExpr(s"TODO_Expr_bool(\"$info1\"$info2inlc)", TrueLit())
  }
  /** Corresponds to the "'TODO_Expr_int_info1': 666" (labeled) int-Expr. 
   * Use these if a int expr is required, but you dont want to implement it yet. */
  def TODO_Expr_int(info1: String = "", info2: String = ""): Expr = {
    B3Development.addTODO(info1, info2)
    val info2inlc = if(INLCUDE_SECOND_MSG){s", \"$info2\""} else {""} 
    LabeledExpr(s"TODO_Expr_int(\"$info1\"$info2inlc)", IntLit(666))
  }
  /** Corresponds to the "'LATER_Expr_bool_info1': true" (labeled) bool-Expr. 
   * Use these if a bool expr is required, but you dont want to implement it yet. */
  def LATER_Expr_bool(info1: String = "", info2: String = ""): Expr = {
    B3Development.addLATER(info1, info2)
    val info2inlc = if(INLCUDE_SECOND_MSG){s", \"$info2\""} else {""} 
    LabeledExpr(s"LATER_Expr_bool(\"$info1\"$info2inlc)", TrueLit())
  }
  /** Corresponds to the "'LATER_Expr_int_info1': 666" (labeled) int-Expr. 
   * Use these if a int expr is required, but you dont want to implement it yet. */
  def LATER_Expr_int(info1: String = "", info2: String = ""): Expr = {
    B3Development.addLATER(info1, info2)
    val info2inlc = if(INLCUDE_SECOND_MSG){s", \"$info2\""} else {""} 
    LabeledExpr(s"LATER_Expr_int(\"$info1\"$info2inlc)", IntLit(666))
  }
  /** Corresponds to the "'ADVANCED_Expr_bool_info1': true" (labeled) bool-Expr. 
   * Use these if a bool expr is required, but you dont want to implement it yet. */
  def ADVANCED_Expr_bool(info1: String = "", info2: String = ""): Expr = {
    B3Development.addADVANCED(info1, info2)
    val info2inlc = if(INLCUDE_SECOND_MSG){s", \"$info2\""} else {""} 
    LabeledExpr(s"ADVANCED_Expr_bool(\"$info1\"$info2inlc)", TrueLit())
  }
  /** Corresponds to the "'ADVANCED_Expr_int_info1': 666" (labeled) int-Expr. 
   * Use these if a int expr is required, but you dont want to implement it yet. */
  def ADVANCED_Expr_int(info1: String = "", info2: String = ""): Expr = {
    B3Development.addADVANCED(info1, info2)
    val info2inlc = if(INLCUDE_SECOND_MSG){s", \"$info2\""} else {""} 
    LabeledExpr(s"ADVANCED_Expr_int(\"$info1\"$info2inlc)", IntLit(666))
  }


  sealed trait Expr extends Node {
    def b3fy: RawAst.Expr
    def typ: Type
    
    def ===(other: Expr) = OpExpr(EqCmp, Seq(this, other))
    def !==(other: Expr) = OpExpr(NeCmp, Seq(this, other))
    def :=(rhs: Expr) = this match {
      case fst: IdExpr => Assign(fst, rhs)
      case fst => sys.error("FAIL: Using Expr.':=' operator, which expects lhs to be an IdExpr, but it was " + fst.getClass.getName)
    }
    def +=(rhs: Expr) = this match {
      case fst: IdExpr => Assign(fst, fst + rhs)
      case fst => sys.error("FAIL: Using '+=' operator, which expects lhs to be an IdExpr, but it was " + fst.getClass.getName)
    }
    def -=(rhs: Expr) = this match {
      case fst: IdExpr => Assign(fst, fst - rhs)
      case fst => sys.error("FAIL: Using '-=' operator, which expects lhs to be an IdExpr, but it was " + fst.getClass.getName)
    }
    def +(other: Expr) = OpExpr(Add, Seq(this, other))
    def -(other: Expr) = OpExpr(Sub, Seq(this, other))
    def *(other: Expr) = OpExpr(Mul, Seq(this, other))
    def /(other: Expr) = OpExpr(Div, Seq(this, other))
    def div(other: Expr) = OpExpr(IntDiv, Seq(this, other))
    def %(other: Expr) = OpExpr(Mod, Seq(this, other))
    def <(other: Expr) = OpExpr(LtCmp, Seq(this, other))
    def >(other: Expr) = OpExpr(LtCmp, Seq(other, this)) // this > other => other < this
    def <=(other: Expr) = OpExpr(LeCmp, Seq(this, other))
    def >=(other: Expr) = OpExpr(LeCmp, Seq(other, this)) // this >= other => other <= this
    def neg = OpExpr(Minus, Seq(this))
    def &&(other: Expr) = OpExpr(And, Seq(this, other))
    def ||(other: Expr) = OpExpr(Or, Seq(this, other))
    def ==>(other: Expr) = OpExpr(Implies, Seq(this, other))
    def <==>(other: Expr) = OpExpr(Equiv, Seq(this, other))
    def forall(vars: Seq[Binding], triggers: Seq[Pattern]) =
      Forall(vars, triggers, this)
    def exists(vars: Seq[Binding], triggers: Seq[Pattern]) =
      Exists(vars, triggers, this)
    def not = OpExpr(Not, Seq(this))
    def thn(thn: Expr) = new PartialCondExpr(this, thn)

    def transform(f: PartialFunction[Expr, Option[Expr]]) = Nodes.transform(this, f)
    
    class PartialCondExpr(cond: Expr, thn: Expr) {
      def els(els: Expr) = CondExp(cond, thn, els)
    }
  }
  /***/
  def andAll(exprs: Seq[Expr]): Expr = {
    exprs match {
      case Seq() => sys.error("Empty Seq[Expr] that should not be empty")
      case Seq(single) => single
      case seq => seq(0) && andAll(seq.tail)
    }
  }


  /** Scala representation of a B3 RawAst BLiteral-Expr node. (Boolean values) */
  sealed abstract class BoolLit(val b: Boolean) extends Expr {
    override def b3fy: RawAst.Expr = new RawAst.Expr_BLiteral(b)
    override def typ: Type = Bool
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
    override def typ: Type = Int
  }

  /** 
   * Real is currently not supported by B3. Therefore, RealLit works currently 
   * the same as IntLit, except that it is not the same class, making the switch 
   * to supporting reals easier, when B3 finally supports them.
   * 
   * B3 LATER (real) change to: "Scala representation of a B3 RawAst RLiteral-Expr node. (Real values)"
   */
  case class RealLit(x: BigInt) extends Expr {
    override def b3fy: RawAst.Expr = new RawAst.Expr_ILiteral(x.bigInteger)
    override def typ: Type = Real
  }

  /* CustomLiteral  =^=  "|" LiteralIdentifier ":" Type "|"  =>  not what we want; we use nonary functions if we have to. */
  // case class CustomLiteral(name: Identifier, typ: Type) extends Expr

  /** 
   * (≃ LocalVar) Scala representation of a B3 RawAst IdExpr-Expr node. 
   * Can also be used as placeholder for an undefined LocalVarDecl for basic variables.
   * Use methods P, F, or Q to create the LocalVarDecl in form of a PParameter, FParameter, or (Quantifier) Binding, respectively.
   * 
   * @param typ is only there for internal type tracking and is not used by B3
  */
  case class IdExpr(name: Identifier, typ: Type, isOld: Boolean = false) extends Expr {
    override def b3fy: RawAst.Expr = new RawAst.Expr_IdExpr(daf(name), isOld)
    /** returns the corresponding PParameter */
    def P(mode: RawAst.ParameterMode = IN) = PParameter(name, typ, mode)
    /** returns the corresponding FParameter */
    def F(isInjective: Boolean = false) = FParameter(name, typ, isInjective)
    /** returns the corresponding (Quantifier) Binding */
    def Q = Binding(name, typ)
  }

  /** Scala representation of a B3 RawAst OpExpr-Expr node. (Replaces BinExp and UnExp; CondExp has its own class)
   * @param op RawAst Operators are provided as values by the current Object (use ObjectName.{Op-name})
   * @param exprs The number of expressions must match to what makes sense for the given Operator.
   * (NOTE: maybe it would make sense to split this into a Binary and Unary Operator internally, with one or two 
   * Expr as parameters instead of one Seq[Expr], even though it is combined in B3)
   */
  case class OpExpr(op: RawAst.Operator, exprs: Seq[Expr]) extends Expr {
    override def b3fy: RawAst.Expr = new RawAst.Expr_OperatorExpr(op, daf(exprs map {_.b3fy}))
    override def typ: Type = op match {
      case LtCmp|LeCmp|EqCmp|NeCmp => Bool
      case And|Equiv|Implies|Or|Not => Bool
      case Div => Real
      case IntDiv => Int
      case Add|Sub|Mul|Mod|Minus => Int //B3 LATER (real): could also be Real, I think?
    }
  }
  /** Scala representation of a B3 RawAst OpExpr-Expr node in CondExp-mode. 
   * @param op RawAst Operators are provided as values by the current Object (use ObjectName.{Op-name})
   */
  case class CondExp(cond: Expr, thn: Expr, els: Expr) extends Expr {
    override def b3fy: RawAst.Expr = new RawAst.Expr_OperatorExpr(new RawAst.Operator_IfThenElse, daf(Seq(cond, thn, els) map {_.b3fy}))
    override def typ: Type = thn.typ
  }

  /* For easy use of B3 operators: (same names as in Silver) */
  // val CondExp = new RawAst.Operator_IfThenElse (Made into case class CondExp for easier use)
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
  case class FunctionCallExpr(name: Identifier, args: Seq[Expr], typ: Type) extends Expr {
    override def b3fy: RawAst.Expr = new RawAst.Expr_FunctionCallExpr(daf(functionName), daf(args map {_.b3fy}))
    def functionName: String = funcName(name, args, typ)
  }

  /**
   * This represents a single, constant value. In reality this just creates a nonary FunctionCallExpr Node, but
   * using this increases Carbon's readability by differencing between "real" functions and values.
   *
   * @param typ is only for internal purposes 
   */
  object Const {
    def apply(name: Identifier, typ: Type): FunctionCallExpr = FunctionCallExpr(name, Seq(), typ)
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
    override def typ: Type = expr.typ
  }


  sealed trait QuantifiedExpr extends Expr {
    def vars: Seq[Binding]
    def expr: Expr
    override def typ: Type = Bool
  }
  /**
    * Scala representation of a B3 RawAst Quantifier(Forall)-Expr node.
    *
    * @param vars Seq of B3 Bindings (bound variables (w/ name + type))
    * @param patterns Seq of patterns for pattern-matching. (same as Silver Trigger's)
    * @param expr The body. Can use any variables in scope, including the ones defined by 'bindings'
    * @param typeVars Type vars are not supported by B3! However, this parameter makes it more practical to handle Domains and many parts
    *  of the preambles, as we can first create a parametric version and then instanciate that for all required TypeVar-combinations 
    * @param weight Currently not supported by B3, but if B3 ever supports this we are ready. This info comes from Silver and can be used
    *  for "specifying the weight of a quantifier in the SMT encoding".
    */
  case class Forall(vars: Seq[Binding], patterns: Seq[Pattern], expr: Expr, typeVars: Seq[TypeVar] = Nil, weight: Option[Int] = None) extends QuantifiedExpr {
    override def b3fy: RawAst.Expr = new RawAst.Expr_QuantifierExpr(true, daf(vars map {_.b3fy}), daf(patterns map {_.b3fyPattern}), expr.b3fy)
  } 
  /**
    * Scala representation of a B3 RawAst Quantifier(Exists)-Expr node.
    *
    * @param vars Seq of B3 Bindings (bound variables (w/ name + type))
    * @param patterns Seq of patterns for pattern-matching. (same as Silver Trigger's)
    * @param expr The body. Can use any variables in scope, including the ones defined by 'bindings'
    * @param weight Currently not supported by B3, but if B3 ever supports this we are ready. This info comes from Silver and can be used
    *  for "specifying the weight of a quantifier in the SMT encoding".
    */
  case class Exists(vars: Seq[Binding], patterns: Seq[Pattern], expr: Expr, weight: Option[Int] = None) extends QuantifiedExpr {
    override def b3fy: RawAst.Expr = new RawAst.Expr_QuantifierExpr(false, daf(vars map {_.b3fy}), daf(patterns map {_.b3fyPattern}), expr.b3fy)
  } 




  // MORE GENERAL NODES
  /** Scala representation of a B3 RawAst Binding node. ("VarDecl" for bound variables in exist/forall expressions). */
  case class Binding(name: Identifier, typ: Type) extends LocalVarDecl {
    def b3fy: RawAst.Binding = new RawAst.Binding(daf(name), daf(typ.b3fy))
    def toP(mode: RawAst.ParameterMode = IN): PParameter = PParameter(name, typ, mode) 
    def toF(isInjective: Boolean = false): FParameter = FParameter(name, typ, isInjective)
  }

  /** (= Trigger) Scala representation of a B3 RawAst Pattern node. (equivalent to a sil.Trigger; for pattern-matching in forall/exists). 
   * (Actually, Pattern nodes only exist in standard B3, but not in B3.jar - there it is "Seq[Seq[Expr]]". But using this nonetheless makes certain transformations easier.) */
  case class Pattern(exprs: Seq[Expr]) extends Expr {
    /** DO NOT USE THIS! (use b3fyPattern instead) */
    def b3fy: RawAst.Expr = sys.error("You are not allowed to call 'b3fy' on a Pattern node! Use 'b3fyPattern' instead")
    def b3fyPattern: DafnySequence[RawAst.Expr] = daf(exprs map {_.b3fy}) // We dont use "new RawAst.Pattern(daf(exprs map {_.b3fy}))" because QuantifierExpr expects DafnySequence[_ <: RawAst.Expr] instead of RawAst.Pattern" in B3's java version
    override def typ: Type = sys.error("You are not allowed to call 'typ' on a Pattern node!")
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
  val infos = mutable.Set.empty[(String, String)]
  val todos = mutable.Set.empty[(String, String)]
  val laters = mutable.Set.empty[(String, String)]
  val advanced = mutable.Set.empty[(String, String)]
  def reset(): Unit = {
    todos.clear()
    laters.clear()
    advanced.clear()
  }
  def info(info1: String = "", info2: String = ""): Unit = {
    if (info1 != "") {
      infos.add((info1, info2))
    }
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
  def printInfo(): Unit = {
    val grouped = infos.groupMap(_._1)(_._2)
    println("=== OTHER INFOS ===")
    grouped.foreach { case (main, detailSet) =>
      println(s"==> $main:\n  - ${detailSet.mkString("\n  - ")}")
    }
    println("=================================")
  }
  def printTODO(): Unit = {
    val grouped = todos.groupMap(_._1)(_._2)
    println("=========== TODO INFO ===========")
    grouped.foreach { case (main, detailSet) =>
      println(s"==> $main:\n  - ${detailSet.mkString("\n  - ")}")
    }
    println("=================================")
  }
  def printLATER(): Unit = {
    val grouped = laters.groupMap(_._1)(_._2)
    println("========== LATER INFO ===========")
    grouped.foreach { case (main, detailSet) =>
      println(s"==> $main:\n  - ${detailSet.mkString("\n  - ")}")
    }
    println("=================================")
  }
  def printADVANCED(): Unit = {
    val grouped = advanced.groupMap(_._1)(_._2)
    println("========= ADVANCED INFO =========")
    grouped.foreach { case (main, detailSet) =>
      println(s"==> $main:\n  - ${detailSet.mkString("\n  - ")}")
    }
    println("=================================")
  }
  def printALL(): Unit = {
    printTODO()
    printLATER()
    printADVANCED()
    printInfo()
  }
}

/**
 * A collection of implicits for working with the B3 AST.
 */
object B3Implicits {
  import viper.carbon.b3.B3Nodes._
  import language.implicitConversions

  // DAFNY HELPERS
  implicit def dseqToSeq[T](dseq: DafnySequence[_ <: T]): Seq[T] = {
    dseq.asScala.toSeq
  }
  
  implicit def dafStringToString(dstr: DafnySequence[_ <: CodePoint]): String = {
    val sb = new java.lang.StringBuilder
    dstr.asScala.foreach(cp => sb.appendCodePoint(cp.value()))
    sb.toString
  }

  // "LocalVarDecl"-CONVERSION HELPERS
  implicit def fToQ(fSeq: Seq[FParameter]): Seq[Binding] = fSeq map {_.toQ}
  implicit def fToExpr(fSeq: Seq[FParameter]): Seq[IdExpr] = fSeq map {_.l}


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
        case Seq(x, y) => Some(OpExpr(Or, Seq(x, y)))
        case x +: xs => Some(OpExpr(Or, Seq(x, all(xs).get)))
      }
    }

    private def all(xss: Seq[Expr]): Option[Expr] = {
      xss match {
        case Nil => None
        case Seq(x) => Some(x)
        case Seq(x, y) => Some(OpExpr(And, Seq(x, y)))
        case x +: xs => Some(OpExpr(And, Seq(x, all(xs).get)))
      }
    }
  }
}


object B3Naming {
  // Currently, only ONE Program is allowed to be translated, since there is no reset yet.
  import viper.carbon.b3.B3Development._
  import viper.carbon.b3.B3Nodes._

  // --- Namespace ---
  /**
   * A namespace to make it easier to avoid duplicated Identities.
   *
   * @param name The name of the namespace; only used for debugging purposes.
   * @param id The ID of this namespace; used to identify the namespace.
   */
  case class Namespace(name: String, id: Int)
  
  
  // --- Identifier ---
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
    override def toString: String = name
  }
  case object Identifier {
    def apply(n: String)(implicit ns: Namespace): Identifier =
      new Identifier {
        val name = n
        val namespace = ns
      }
    def unapply(i: Identifier) = Some(i.name, i.namespace)
  }

  import language.implicitConversions
  /** B3NameGenerator instance. */
  private val nameGen = new B3NameGenerator()
  /** The current mapping from identifier to names. */
  private val idnMap = collection.mutable.HashMap[Identifier, String]()
  /** The current mapping from unique B3 names to the original identifiers (inverse mapping of idnMap,
    * where the names of the identifiers are used directly). */
  val backMap = collection.mutable.HashMap[String, String]()
  /** Map an identifier to a string, making it unique first if necessary. */
  implicit def idName(i: Identifier): String = {
    idnMap.get(i) match {
      case Some(s) => s
      case None =>
        val s = nameGen.createUniqueIdentifier(i.preferredName)
        idnMap.put(i, s)
        backMap.update(s, i.name)
        s
    }
  }


  // --- Function names ---
  // Function names can only be created after registering the function, which happens when its Function(-Decl)
  // is created. Therefore, any operation that needs the name from a function call expression must ensure that 
  // the corresponding function has already been declared. For parametric functions this means the parametric 
  // version!

  /** 
   * Stores all in/output combos of functions that were used somewhere and have not yet been declared.
   * These all need to be declared! Don't add any already declared function here (see declaredFuncs).
   * A combo for some functionName is of the form: Seq(inTyp1Name,...,inTypNName, outTypName) 
   */
  private val usedButUndeclaredFuncs = collection.mutable.MultiDict.empty[String, Seq[String]]
  /** Collection of all functions that have been declared. */
  private val declaredFuncs = collection.mutable.Set[(String, Seq[String])]()

  /** 
   * Maps Identifiers to an Int sequence. Each value x in the sequence says: "the x'th parameter's type should be used 
   * for naming" (the output type counts as the "last parameter", so if there are n parameters, then value n means the 
   * output type should be used for naming (since we start at index 0)). This should only be updated by 'registerFunction'
   */
  private val paramFuctionMap = collection.mutable.HashMap[Identifier, Seq[Int]]()

  /** 
   * Returns the correct name to use for the given Identifier, parameterTypeNames and outputTypeName. 
   * For non-parametric functions, this corresponds to the name defined by the Identifier.
   */
  def funcName(name: Identifier, parameterTypeNames: Seq[String], outputTypeName: String): String = {
    paramFuctionMap.get(name) match {
      case Some(paramFuctionHandler) => 
        name+"%F"+paramFuctionHandler.collect(parameterTypeNames ++ Seq(outputTypeName)).mkString("%%")
      case None => name
    } 
  }
  // TODO: find less ugly way to do this whole "nicer-name thing".
  var specialFunctionReadHeapName = Identifier("x")(Namespace("wrong", -666))
  var specialFunctionUpdateHeapName = Identifier("y")(Namespace("wrong", -666))
  var heapTypVarsToIdx: Map[Seq[B3Nodes.Type],Int] = Map()
  def printTypVarMapping = heapTypVarsToIdx map {case (typvars, idx) => println(s"${idx} <-> ${(typvars map {_.b3fy}).mkString(" ")}")}
  /** 
   * Returns the correct name to use for the given Identifier, args, and output Type. 
   * For non-parametric functions, this corresponds to the name defined by the Identifier.
   */
  def funcName(name: Identifier, args: Seq[Expr], typ: Type): String = {
    // TODO: find less ugly way to do this whole "nicer-name thing".
    if (name == specialFunctionReadHeapName || name == specialFunctionUpdateHeapName) {
      return name+"%F"+heapTypVarsToIdx(args(0).typ.asInstanceOf[NamedType].typVars)
    }

    val argTypeNames = args map {_.typ.b3fy}
    val outputTypeName = typ.b3fy
    funcName(name, argTypeNames, outputTypeName)
  }
  /** 
   * Returns the correct name to use for the given FunctionCallExpr. 
   * For non-parametric functions, this corresponds to the name defined by the Identifier.
   */
  def funcName(fc: FunctionCallExpr): String = {
    funcName(fc.name, fc.args, fc.typ)
  }

  /** 
   * Returns the correct name to use for the given Function. 
   * For non-parametric functions, this corresponds to the name defined by the Identifier.
   * Do not call this on parametric Function's
   */
  def funcName(f: Function): String = {
    // TODO: find less ugly way to do this whole "nicer-name thing".
    if (f.name == specialFunctionReadHeapName || f.name == specialFunctionUpdateHeapName) {
      return f.name+"%F"+heapTypVarsToIdx(f.args(0).typ.asInstanceOf[NamedType].typVars)
    }
    val argTypeNames = f.args map {_.typ.b3fy}
    val outputTypeName = f.typ.b3fy
    funcName(f.name, argTypeNames, outputTypeName)
  }

  /** 
   * (This function is automatically called when creating a new Function instance, no need to call it anywhere else.) 
   * 
   * Registers the given Function. This creates an entry in paramFuctionMap for this function (or rather, its Identifier). 
   * The entry corresponds to the sequence of indexes of all parameters(*) whose type contains any free type vars.
   * (If there are no free type vars we can later use the normal name, otherwise we will have to modify it.)
   * If the function is already registered (this happens e.g. when parametric functions are replaced by their concrete 
   * versions), then 'registerFunction' does nothing.
   * 
   * (*) the function output is treated as if it was the last function parameter
   */
  def registerFunction(func: Function): Unit = {
    val inAndOutTypsWithIndexes = ((func.args map (_.typ)) ++ Seq(func.typ)).zipWithIndex
    val freeTypVarIndexes = inAndOutTypsWithIndexes.collect({
      case (arg, idx) if !arg.freeTypeVars.isEmpty => idx
    })
    if (!freeTypVarIndexes.isEmpty) {
      paramFuctionMap.getOrElseUpdate(func.name, freeTypVarIndexes)
    }
  }
  /** 
   * This function can be used to register a function without declaring its parametric version.
   * If the function is already registered, then an error is thrown. (Since this "manual" version is
   * likely what we actually want.)
   * 
   * @param funcName The Identifier of the function to register.
   * @param idxs A Seq containing the indexes (as integers) of the parameters that should be used 
   * for the name. (Starts at index 0, ends at index |parameters|, which corresponds to the output)
   */
  def registerFunction(funcName: Identifier, idxs: Seq[Int]): Unit = {
    if (paramFuctionMap.contains(funcName)) {
      sys.error("The manual version of registerFunction is must only be called on a yet unregistered" +
        "function. This did not hold right now, so the order of operations is wrong somewhere.")
    }
    if (!idxs.isEmpty) {
      paramFuctionMap.getOrElseUpdate(funcName, idxs)
    }
  }
}

object ErrorMemberMapping {
  // The "weak" hash map is necessary to avoid leaking memory.
  // See issue https://github.com/viperproject/carbon/issues/444
  val mapping = mutable.WeakHashMap[VerificationError, Member]()
  var currentMember : Member = null
}