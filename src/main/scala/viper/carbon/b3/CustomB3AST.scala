package viper.carbon.b3
import viper.carbon.b3.{DafnyHelper => Daf, B3Helper => B3}


object CustomB3AST {
  def B3_test(): Unit = {
    println("Testing custom B3 AST...")
    // import scala.collection.JavaConverters._ //(Needed to translate Scala List into Java List ("List(...).asJava"))
    import scala.jdk.CollectionConverters._

    // val typeDescrPParam = dafny.TypeDescriptor.reference(classOf[Ast.PParameter])
    // val noProcedureParameters = dafny.DafnySequence.empty(typeDescrPParam)
    val noProcedureParameters = Daf.SeqT_empty[Ast.PParameter]

    // Better way of creating empty DafnySequence of some type
    val typeDescrAExpr = dafny.TypeDescriptor.reference(classOf[Ast.AExpr])
    val emptyAExprSeq = dafny.DafnySequence.empty(typeDescrAExpr)


    // BODY (assert false / assert true)
    val label = new Ast.Label();
    label.__ctor(Resolver.__default.ReturnLabelName());
    // val var11 = (Ast.Stmt)var10.Extract(Ast.Stmt._typeDescriptor(), DafnySequence._typeDescriptor(TypeDescriptor.UNICODE_CHAR));
    val assertLocation = new Ast.Location()
    assertLocation.__ctor(dafny.DafnySequence.asUnicodeString("assertLoc1"))
    val var11 = new Ast.Stmt_Check(new Ast.Expr_BLiteral(true), assertLocation)
    val var12 = new Ast.Stmt_Assert(new Ast.Expr_BLiteral(true), assertLocation)

    //can now replace this vvv
    // val javaBlockStmtList = List(var11, var12).map(x => x: Ast.Stmt).asJava
    // val typeDescrBlockStmts = dafny.TypeDescriptor.reference(Ast.Stmt.class)
    // val dafnyBlockStmtArr = dafny.Array.fromList(dafny.TypeDescriptor.reference(classOf[Ast.Stmt]), javaBlockStmtList)
    // val stmtBlock = new Ast.Stmt_Block(dafny.DafnySequence.fromArray(dafny.TypeDescriptor.reference(classOf[Ast.Stmt]), dafnyBlockStmtArr))
    //can replace this ^^^
    //with this vvv
    val dafnyBlockStmtArr = Daf.SeqT_fromSeq(Seq(var11, var12).map(x => x: Ast.Stmt))
    val stmtBlock = new Ast.Stmt_Block(dafnyBlockStmtArr)
    //with this ^^^
    val procedureBody = Std.Wrappers.Option.create_Some(dafny.TypeDescriptor.reference(classOf[Ast.Stmt]), Ast.Stmt.create_LabeledStmt(label, stmtBlock));

    // PROCEDURE
    val name = dafny.DafnySequence.asUnicodeString("procedureName") // "= CodePoint"
    val procedure = new Ast.Procedure()
    procedure.__ctor(name, noProcedureParameters, emptyAExprSeq, emptyAExprSeq)
    val javaProcedureList = List(procedure).asJava
    val typeDescrProcedure = dafny.TypeDescriptor.reference(classOf[Ast.Procedure])
    val dafnyProcedureArr = dafny.Array.fromList(typeDescrProcedure, javaProcedureList)
    val procedure_seq = dafny.DafnySequence.fromArray(typeDescrProcedure, dafnyProcedureArr)
    procedure.Body = procedureBody

    // PROGRAM
    val prog = new Ast.Program(dafny.DafnySequence.empty(dafny.TypeDescriptor.reference(classOf[Ast.TypeDecl])), 
                                dafny.DafnySequence.empty(dafny.TypeDescriptor.reference(classOf[Ast.Function])), 
                                dafny.DafnySequence.empty(dafny.TypeDescriptor.reference(classOf[Ast.Axiom])), 
                                procedure_seq)


    print("-----------------\n")
    B3.printAst(prog)
    print("-----------------\n")
    Verifier.__default.Verify(prog, new dafny.DafnyMap())
    print("-----------------\n")

    println("Testing custom B3 AST finished!")

    println("=======================")


    println("Testing custom B3 RawAST...")
    val rawProcedure = Daf.SeqT_fromSeq[RawAst.Procedure](Seq()) 
    val rawb3 = new RawAst.Program(Daf.SeqT_empty[dafny.DafnySequence[dafny.CodePoint]],
                                    Daf.SeqT_empty[RawAst.Tagger],
                                    Daf.SeqT_empty[RawAst.Function],
                                    Daf.SeqT_empty[RawAst.Axiom],
                                    rawProcedure)

    val options = Seq("--print", "print", "test")

    B3.runB3(rawb3, options)
    println("Testing custom B3 RawAST finished!")
    // var syntax := new B3CliSyntax();


  }
}





object UncleanCustomB3AST {
  def B3_test_unclean(): Unit = {
    println("Testing custom B3 AST...")
    // import scala.collection.JavaConverters._ //(Needed to translate Scala List into Java List)
    import scala.jdk.CollectionConverters._
    // import Ast._
    // import Std.Wrappers._

    // Procedure Parameters (does not work since array is not truly empty; the PParameter is not defined => (: bool) instead of ())
    // val emptyPParam = new Ast.PParameter() // create empty one. Otherwise use: .__ctor(DafnySequence<? extends CodePoint> var1, ParameterMode var2, Type var3, Option<Variable> var4)
    // val javaPParamList = List(emptyPParam).asJava
    // val typeDescrPParam = dafny.TypeDescriptor.reference(classOf[Ast.PParameter])
    // val dafnyPParamArr = dafny.Array.fromList(typeDescrPParam, javaPParamList)
    // val noProcedureParameters = dafny.DafnySequence.fromArray(typeDescrPParam, dafnyPParamArr)

    val typeDescrPParam = dafny.TypeDescriptor.reference(classOf[Ast.PParameter])
    val noProcedureParameters = dafny.DafnySequence.empty(typeDescrPParam)

    // Better way of creating empty DafnySequence of some type
    val typeDescrAExpr = dafny.TypeDescriptor.reference(classOf[Ast.AExpr])
    val emptyAExprSeq = dafny.DafnySequence.empty(typeDescrAExpr)


    // val newTypeHello = new Ast.TypeDecl()
    // newTypeHello.__ctor(dafny.DafnySequence.asUnicodeString("hello"))
    // val intVal2 = new Ast.Expr_ILiteral(java.math.BigInteger.valueOf(2))


    // Procedure BODY
    // Label var7 = new Label();
    // var7.__ctor(ReturnLabelName());
    // LocalResolverState var8 = LocalResolverState.create(var5, DafnyMap.fromElements(new Tuple2[0]), Option.create_None((TypeDescriptor)TypeDescriptor.reference(Label.class)), Option.create_Some((TypeDescriptor)TypeDescriptor.reference(Label.class), var7));
    // Result var9 = (Result)null;
    // Result var10 = StmtResolver.__default.ResolveStmt((Stmt)var0.dtor_body().dtor_value(), var2, var8);
    // if (var10.IsFailure(Ast.Stmt._typeDescriptor(), DafnySequence._typeDescriptor(TypeDescriptor.UNICODE_CHAR))) {
    //   var3 = var10.PropagateFailure(Ast.Stmt._typeDescriptor(), DafnySequence._typeDescriptor(TypeDescriptor.UNICODE_CHAR), Tuple0._typeDescriptor());
    //   return var3;
    // } else {
    //   Ast.Stmt var11 = (Ast.Stmt)var10.Extract(Ast.Stmt._typeDescriptor(), DafnySequence._typeDescriptor(TypeDescriptor.UNICODE_CHAR));
    //   var1.Body = Option.create_Some(Ast.Stmt._typeDescriptor(), Ast.Stmt.create_LabeledStmt(var7, var11));
    //   var3 = Result.create_Success(Tuple0._typeDescriptor(), DafnySequence._typeDescriptor(TypeDescriptor.UNICODE_CHAR), Tuple0.create());
    //   return var3;
    // }


    val label = new Ast.Label();
    label.__ctor(Resolver.__default.ReturnLabelName());
    // val var11 = (Ast.Stmt)var10.Extract(Ast.Stmt._typeDescriptor(), DafnySequence._typeDescriptor(TypeDescriptor.UNICODE_CHAR));
    val assertLocation = new Ast.Location()
    assertLocation.__ctor(dafny.DafnySequence.asUnicodeString("assertLoc1"))
    val var11 = new Ast.Stmt_Assert(new Ast.Expr_BLiteral(false), assertLocation)
    val procedureBody = Std.Wrappers.Option.create_Some(Ast.Stmt._typeDescriptor(), Ast.Stmt.create_LabeledStmt(label, var11));

    // PROCEDURE
    // __ctor(DafnySequence<? extends CodePoint> var1,   (procedure name)
    //        DafnySequence<? extends PParameter> var2,  (procedure parameters)
    //        DafnySequence<? extends AExpr> var3,       (pre-conditions)
    //        DafnySequence<? extends AExpr> var4) {     (post-conditions)
    // + Need to define Option<Stmt> Body separately
    val name = dafny.DafnySequence.asUnicodeString("procedureName") // "= CodePoint"
    val procedure = new Ast.Procedure()
    procedure.__ctor(name,
                      noProcedureParameters,
                      emptyAExprSeq,
                      emptyAExprSeq)
    val javaProcedureList = List(procedure).asJava
    val typeDescrProcedure = dafny.TypeDescriptor.reference(classOf[Ast.Procedure])
    val dafnyProcedureArr = dafny.Array.fromList(typeDescrProcedure, javaProcedureList)
    val procedure_seq = dafny.DafnySequence.fromArray(typeDescrProcedure, dafnyProcedureArr)
    procedure.Body = procedureBody

    // val seq = dafny.DafnySequence.fromArray(dafny.TypeDescriptor.reference(classOf[Ast.PParameter]), new dafny.Array(emptyPParameter))
    // var x = new Ast.Expr_ILiteral(2)



    // var returnLabel := new Label(ReturnLabelName);
    // var ls := LocalResolverState(varMap, map[], None, Some(returnLabel));

    // var body :- ResolveStmt(proc.body.value, prs, ls);

    // rproc.Body := Some(LabeledStmt(returnLabel, body));


    // PROGRAM
    // datatype Program = Program(types: seq<TypeDecl>,         DafnySequence<? extends TypeDecl>
    //                            functions: seq<Function>,     DafnySequence<? extends Function>
    //                            axioms: seq<Axiom>,           DafnySequence<? extends Axiom>
    //                            procedures: seq<Procedure>)   DafnySequence<? extends Procedure>  
    val prog = new Ast.Program(dafny.DafnySequence.empty(dafny.TypeDescriptor.reference(classOf[Ast.TypeDecl])), 
                                dafny.DafnySequence.empty(dafny.TypeDescriptor.reference(classOf[Ast.Function])), 
                                dafny.DafnySequence.empty(dafny.TypeDescriptor.reference(classOf[Ast.Axiom])), 
                                procedure_seq)





  // //   var input :- FileIO.ReadUTF8FromFile(filename);
  // //   var parseResult := SB.Apply(Parser.TopLevel, input);
  // //   var b3 :- match parseResult {
  // //     case ParseSuccess(value, remaining) => Success(value)
  // //     case ParseFailure(_, _) => Failure(SB.FailureToString(input, parseResult))
  // //   };
  // //  return Success(b3);

  // var rawb3 = new Std.Wrappers.Result_Success(dafny.TypeDescriptor.reference(classOf[Ast.Program]), 
  //                                             dafny.TypeDescriptor.reference(classOf[String]),
  //                                             prog)
// Std.Wrappers.Result_Success(type of succes val, type of error val, actual input [i.e. input from Success(input); Result_Error(SucType, ErrType, input) exacty same for Error(input)])


  
  // var x = 
    // var syntax = new B3.B3CliSyntax();
    // var cliResult = CommandLineOptions.__default.Parse(B3.Verb._typeDescriptor(), syntax, args);
    // // if (cliResult.Failure) {
    // //   print (cliResult.error, "\n");
    // //   return;
    // // }
    // var cli = cliResult.value;

    // var r = B3.__default.ReadAndParseProgram(cli.files[0]);
    // if (r.IsFailure()) {
    //   print (r.error, "\n");
    //   return;
    // }
    // var rawb3 = r.value;

    print("-----------------\n")
    ResolvedPrinter.__default.Program(prog);
    // System.out.flush()
    print("-----------------\n")
    Verifier.__default.Verify(prog, new dafny.DafnyMap())
    // Verifier.__default.Verify(Program var0, 
    //                           DafnyMap<? extends DafnySequence<? extends CodePoint>, 
    //                                   ? extends DafnySequence<? extends DafnySequence<? extends CodePoint>>> var1) {
    print("-----------------\n")

    println("Testing custom B3 AST finished!")
    

    // var r = B3.ReadAndParseProgram(cli.files[0]);
    // B3.ResolveAndTypeCheck()
  }
}

// object ASTInjector {
//   var program: Option[B3.Program] = None
// }

// ASTInjector.program = Some(myProgram)
// b3.main(Array("verify", "dummy.b3"))

// ASTInjector.program.getOrElse(Parser.parse(file))
