namespace Microsoft.Research.Dkal.Ast.Substrate.Sql.Syntax.Simple
  
  open System.Collections.Generic

  open Microsoft.Research.Dkal.Interfaces
  open Microsoft.Research.Dkal.Globals
  open Microsoft.Research.Dkal.Ast.Tree
  open Microsoft.Research.Dkal.Ast.Infon
  open Microsoft.Research.Dkal.Ast
  open Microsoft.Research.Dkal.Ast.Substrate.Sql.Syntax.Simple.SimpleAst
  open Microsoft.Research.Dkal.Substrate.Sql
  open Microsoft.Research.Dkal.Substrate.Factories
  
  /// A Context is responsible for lifting untyped SimpleMetaTerms into typed
  /// MetaTerms. 
  type Context(substrate: SqlSubstrate, types: Dictionary<SimpleVariable, IType>, macros: Dictionary<string, IType * ISubstrateTerm * IVar list>, tmpId: int) =

    /// Holds fresh variable ids that are used when solving macros
    let mutable freshVarId = tmpId

    /// Returns a fresh Variable of the given type
    let freshVar t = 
      freshVarId <- freshVarId + 1
      { Name = "Tmp" + freshVarId.ToString(); 
        Type = t }

    /// Given a SimpleMetaTerm smt and a Type t, it returns its the 
    /// corresponding MetaTerm, if smt encodes a MetaTerm of Type t. All 
    /// macros are solved and its conditions are added as an extra AsInfon
    /// expression in the end.
    member ctx.LiftSimpleMetaTerm (smt: SimpleMetaTerm) (typ: IType) : ITerm =
      let mainTerm, solvedMacros = ctx.Traverse smt typ
      let termWithMacros = AndBool((Seq.toList solvedMacros) @ [mainTerm])
      termWithMacros.Normalize()

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
  
    member private ctx.Traverse (smt: SimpleMetaTerm) (typ: IType) =
      let t, solvedMacros = 
        match smt with
        | SimpleApp(f, smts) -> 
          ctx.LiftSimpleApplication f smts true
          // TODO apply solved macros
        | SimpleConst(c) ->
          ctx.LiftSimpleConstant(c), []
        | SimpleVar(v) ->
          ctx.LiftSimpleVariable(v), []
        | SimpleSubstrate(ns, exp) ->
          ctx.LiftSimpleSubstrate ns exp, []
      if t.Type = typ then 
        t, solvedMacros
      else 
        failwithf "Expecting a %O MetaTerm, found %A" typ smt

    member private ctx.LiftSimpleApplication (f: SimpleFunction) (smts: SimpleMetaTerm list) (goRecursively: bool) 
                    : ITerm * ITerm list =
      // check if it is a table.column operator
      if f.Contains "." then
        match f.Split [|'.'|] with
        | [| table; column |] -> 
          let typ = Type.Substrate(substrate.GetColumnType ("dbo."+table) column)
          App({Name=f; RetType=typ; ArgsType=[]; Identity=None}, []), []
        | _ -> failwithf "Incorrect table.column operator usage in %O" f
      // check if it is a macro
      elif macros.ContainsKey f then
        let retTyp, body, args = macros.[f]
        if goRecursively then
          let mutable subst = Substitution.Id
          let mutable accumSolvedMacros = []
          for smt, arg in List.zip smts args do
            let mt, solvedMacros = ctx.Traverse smt arg.Type
            accumSolvedMacros <- accumSolvedMacros @ solvedMacros
            subst <- subst.Extend ({Name = arg.Name; Type = mt.Type}, mt)
          let newRet = Var(freshVar retTyp)
          subst <- subst.Extend ({Name = "Ret"; Type = retTyp}, newRet)
          accumSolvedMacros <- accumSolvedMacros @ [(body :> ITerm).Apply subst]
          newRet, accumSolvedMacros
        else
          Var {Name="MacroResult"; Type=retTyp}, []
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
      | BoolSimpleConstant b -> Const(SubstrateConstant b)
      | Int32SimpleConstant i -> Const(SubstrateConstant i)
      | DoubleSimpleConstant f -> Const(SubstrateConstant f)
      | StringSimpleConstant s -> Const(SubstrateConstant s)
      | PrincipalSimpleConstant p -> Const(PrincipalConstant p)

    member private ctx.LiftSimpleVariable (v: SimpleVariable) : ITerm =
      let found, typ' = types.TryGetValue v
      if found then
        Var({ Name = v; Type = typ' })
      else
        failwith <| "Undeclared variable: " + v
    
    member private ctx.LiftSimpleSubstrate (ns: string) (exp: string) : ITerm =
      let substrate = SubstrateMap.GetSubstrate ns
      let parser = SubstrateParserFactory.SubstrateParser substrate "simple" ns freshVarId types macros
      parser.ParseTerm exp :> ITerm



