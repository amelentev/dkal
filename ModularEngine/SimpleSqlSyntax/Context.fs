namespace Microsoft.Research.Dkal.Substrate.SimpleSqlSyntax
  
  open System.Collections.Generic

  open Microsoft.Research.Dkal.Substrate.SimpleSqlSyntax.SimpleAst
  open Microsoft.Research.Dkal.SqlSubstrate
  open Microsoft.Research.Dkal.Interfaces
  open Microsoft.Research.Dkal.Ast.Tree
  open Microsoft.Research.Dkal.Ast.Infon
  open Microsoft.Research.Dkal.Ast
  
  /// A Context is responsible for lifting untyped SimpleMetaTerms into typed
  /// MetaTerms. 
  type Context(substrate: SqlSubstrate, types: Dictionary<SimpleVariable, IType>) =

    /// Given a SimpleMetaTerm smt and a Type t, it returns its the 
    /// corresponding MetaTerm, if smt encodes a MetaTerm of Type t. All 
    /// macros are solved and its conditions are added as an extra AsInfon
    /// expression in the end.
    member ctx.LiftSimpleMetaTerm (smt: SimpleMetaTerm) (typ: IType option) : ITerm =
      let complies (typ: IType) (typ': IType option) =
        match typ' with
        | Some typ' when typ' = typ -> true
        | None -> true
        | _ -> false
      let failDueToType (smt: SimpleMetaTerm) (typ: IType option) = 
        failwith <| "Expecting a " + typ.Value.Name + "MetaTerm, found: " + (sprintf "%A" smt)
      let solvedMacros = new List<ITerm>()
      let rec traverse (smt: SimpleMetaTerm) (typ: IType option) : ITerm = 
        match smt with
        | SimpleApp(f, smts) ->
          // check if it is a table.column operator
          if f.Contains "." then
            match f.Split [|'.'|] with
            | [| table; column |] -> 
              let typ = Type.Substrate(substrate.GetColumnType ("dbo."+table) column)
              App({Name=f; RetType=typ; ArgsType=[]}, [])
            | _ -> failwithf "Incorrect table.column operator usage in %O" f
          else
            // check if it is an overloaded operator
            if not(smts.IsEmpty) then
              let mt0 = traverse smts.[0] None
              match SqlPrimitives.SolveOverloadOperator f mt0.Type with
              | Some func -> 
                if not(complies func.RetType typ) then failDueToType smt typ
                let mts = List.map2 (fun smt t -> traverse smt (Some t)) smts func.ArgsType
                App(func, mts)
              | None -> 
                failwith <| "Undefined identifier: " + f + " on " + (sprintf "%A" smt)
            else
              failwith <| "Undefined identifier: " + f + " on " + (sprintf "%A" smt)
        | SimpleConst(c) ->
          match c with
          | BoolSimpleConstant b when complies Type.Boolean typ -> Const(SubstrateConstant b)
          | Int32SimpleConstant i when complies Type.Int32 typ -> Const(SubstrateConstant i)
          | DoubleSimpleConstant f when complies Type.Double typ -> Const(SubstrateConstant f)
          | StringSimpleConstant s when complies Type.String typ -> Const(SubstrateConstant s)
          | PrincipalSimpleConstant p when complies Type.Principal typ -> Const(PrincipalConstant p)
          | _ -> failDueToType smt typ
        | SimpleVar(v) ->
          let found, typ' = types.TryGetValue v
          if found then
            if complies typ' typ then
              Var({ Name = v; Type = typ' })
            else
              failDueToType smt typ
          else
            failwith <| "Undeclared variable: " + v
      let mainTerm = traverse smt typ
//      let normalTerm = normalize mainTerm
//      normalTerm
      // TODO: normalize
      mainTerm

    



