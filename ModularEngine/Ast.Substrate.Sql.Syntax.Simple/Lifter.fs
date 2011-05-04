namespace Microsoft.Research.Dkal.Ast.Substrate.Sql.Syntax.Simple
  
  open System.Collections.Generic

  open Microsoft.Research.Dkal.Interfaces
  open Microsoft.Research.Dkal.Globals
  open Microsoft.Research.Dkal.Ast.Tree
  open Microsoft.Research.Dkal.Ast.Infon
  open Microsoft.Research.Dkal.Ast
  open Microsoft.Research.Dkal.Ast.Substrate.Sql.Syntax.Simple.SimpleAst
  open Microsoft.Research.Dkal.Substrate
  open Microsoft.Research.Dkal.Substrate.Sql
  open Microsoft.Research.Dkal.Substrate.Factories
  
  /// A Lifter is responsible for lifting untyped SimpleMetaTerms into typed
  /// MetaTerms. 
  type Lifter(substrate: SqlSubstrate, context: IParsingContext, ns: string) =

    /// Given a SimpleMetaTerm smt and a Type t, it returns its the 
    /// corresponding MetaTerm, if smt encodes a MetaTerm of Type t. All 
    /// macros are solved and its conditions are added as an extra AsInfon
    /// expression in the end.
    member ctx.LiftSimpleMetaTerm (smt: SimpleMetaTerm) : ISubstrateTerm =
      let queryWithMacros smt = 
        let mainTerm, solvedMacros = ctx.Traverse smt Type.Boolean
        let termWithMacros = AndBool((Seq.toList (List.map AsBoolean solvedMacros)) @ [mainTerm])
        termWithMacros.Normalize()

      match smt with
      | SimpleModify(query, cols) -> 
        let colsMapping = new Dictionary<string, ITerm>()
        for (tableCol, smt) in cols do
          let table, column = ctx.SplitTableCol tableCol
          let t, (macros: ISubstrateQueryTerm list) = ctx.Traverse smt (Type.Substrate(substrate.GetColumnType table column))
          if not(macros.IsEmpty) then
            failwith "Unresolved macros inside SQL modify statement"
          colsMapping.[tableCol] <- t
        SqlSubstrateModifyTerm(ns, queryWithMacros query, colsMapping) :> ISubstrateTerm
      | _ ->
        new DummySubstrateQueryTerm(queryWithMacros smt, ns) :> ISubstrateTerm

    member private ctx.LookaheadType (smt: SimpleMetaTerm) =
      match smt with
      | SimpleApp(f, smts) -> 
        let t, _ = ctx.LiftSimpleApplication f smts false
        t.Type
      | SimpleConst(c) ->
        ctx.LiftSimpleConstant(c).Type
      | SimpleVar(v) ->
        ctx.LiftSimpleVariable(v).Type
      | SimpleSubstrate(ns, exp) ->
        Type.Boolean
      | SimpleModify(_) -> 
        Type.SubstrateUpdate
  
    member private ctx.Traverse (smt: SimpleMetaTerm) (typ: IType) =
      let t, solvedMacros = 
        match smt with
        | SimpleApp(f, smts) -> 
          ctx.LiftSimpleApplication f smts true
        | SimpleConst(c) ->
          ctx.LiftSimpleConstant(c), []
        | SimpleVar(v) ->
          ctx.LiftSimpleVariable(v), []
        | SimpleSubstrate(ns, exp) ->
          ctx.LiftSimpleSubstrate ns exp, []
        | SimpleModify(query, cols) ->
          failwith "Found modification statement inside substrate expression"
      if t.Type = typ then 
        t, solvedMacros
      else 
        failwithf "Expecting a %O MetaTerm, found %A" typ smt

    member private ctx.LiftSimpleApplication (f: SimpleFunction) (smts: SimpleMetaTerm list) (goRecursively: bool) 
                    : ITerm * ISubstrateQueryTerm list =
      // check if it is a table.column operator
      if f.Contains "." then
        let table, column = ctx.SplitTableCol f
        let typ = Type.Substrate(substrate.GetColumnType table column)
        App({Name=f; RetType=typ; ArgsType=[]; Identity=None}, []), []
      // check if it is a macro
      elif context.HasMacro f then
        let args = context.GetMacroArgs f
        if goRecursively then
          let mutable accumSolvedMacros = []
          let mutable concreteArgs = []
          for smt, arg in List.zip smts args do
            let mt, solvedMacros = ctx.Traverse smt arg.Type
            accumSolvedMacros <- accumSolvedMacros @ solvedMacros
            concreteArgs <- concreteArgs @ [mt]
          let ret, solvedMacro = context.ApplyMacro(f, concreteArgs)
          ret, accumSolvedMacros @ [solvedMacro]
        else
          Var {Name="MacroResult"; Type=context.GetMacroRetType f}, []
      // check if it is an overloaded operator
      elif not(smts.IsEmpty) then
        let t = ctx.LookaheadType smts.[0]
        match SqlPrimitives.SolveOverloadOperator f t with
        | Some func -> 
          if goRecursively then
            let mts, solvedMacrosMany = List.unzip <| List.map2 (fun smt t -> ctx.Traverse smt t) smts func.ArgsType
            App(func, mts), List.concat solvedMacrosMany
          else
            Var {Name="FunctionResult"; Type=func.RetType}, []
        | None -> 
          failwith <| "Undefined identifier: " + f 
      else
        failwith <| "Undefined identifier: " + f 

    member private ctx.LiftSimpleConstant (c : SimpleConstant) : ITerm =
      match c with
      | BoolSimpleConstant b -> Const(Constant b)
      | Int32SimpleConstant i -> Const(Constant i)
      | DoubleSimpleConstant f -> Const(Constant f)
      | StringSimpleConstant s -> Const(Constant s)
      | PrincipalSimpleConstant p -> Const(PrincipalConstant p)

    member private ctx.LiftSimpleVariable (v: SimpleVariable) : ITerm =
      if v = "Me" then
        Const <| PrincipalConstant(context.Me)
      else
        Var({ Name = v; Type = context.VariableType v })
    
    member private ctx.LiftSimpleSubstrate (ns: string) (exp: string) : ITerm =
      let substrate = SubstrateMap.GetSubstrate ns
      let parser = SubstrateParserFactory.SubstrateParser substrate "simple" ns (Some context)
      parser.ParseTerm exp :> ITerm

    member private ctx.SplitTableCol (tableCol: string) =
      match tableCol.Split [|'.'|] with
      | [| table; column |] -> 
        table, column
      | _ -> failwithf "Incorrect table.column operator usage in %O" tableCol