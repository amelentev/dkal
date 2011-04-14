namespace Microsoft.Research.Dkal.SimpleSyntax
  
  open System.Collections.Generic

  open Microsoft.Research.Dkal.SimpleSyntax.SimpleAst
  open Microsoft.Research.Dkal.SimpleSyntax.TypeErrors
  open Microsoft.Research.Dkal.Ast
  open Microsoft.Research.Dkal.Ast.Normalizer

  type Context() =
    let types = new TypeInfo()
    let identifiers = new Dictionary<string, Function>()
    let macros = new Dictionary<string, Function * MetaTerm * string list>()

    let mutable freshVarId = 0
    let freshVar typ = 
      freshVarId <- freshVarId + 1
      { Name = "Tmp" + freshVarId.ToString(); 
        Typ = typ }

    do
      // primitive identifiers
      for nameFunc in primitives do
        identifiers.Add(nameFunc.Key, nameFunc.Value)

    member ctx.Identifiers = identifiers
    member ctx.Macros = macros

    member private ctx.LiftArgs (args: SimpleArg list) : Variable list = 
      List.map (fun (name, typ) -> { Name = name; Typ = types.LiftType typ }) args

    member private ctx.HasIdentifier name =
      ctx.Identifiers.ContainsKey name || ctx.Macros.ContainsKey name 

    member private ctx.AddIdentifier func =
      if ctx.HasIdentifier func.Name then
        failwith <| "Identifier " + func.Name + " defined twice"
      ctx.Identifiers.[func.Name] <- func

    member private ctx.AddMacro func body argsNames =
      if ctx.HasIdentifier func.Name then
        failwith <| "Identifier " + func.Name + " defined twice"
      ctx.Macros.[func.Name] <- (func, body, argsNames)

    member private ctx.AddType (std: SimpleTypeDeclaration) =
      types.AddTypeRename std.newTyp std.targetTyp

    member private ctx.AddTable (std: SimpleTableDeclaration) =
      for colName, colTyp in std.Cols do
        ctx.AddIdentifier { Name = std.Name + "." + colName
                            RetTyp = types.LiftType colTyp; 
                            ArgsTyp = [] }

    member private ctx.AddRelation (srd: SimpleRelationDeclaration) =
      let _, argsTyp = List.unzip srd.Args
      let realArgsTyp = List.map (fun st -> types.LiftType st) argsTyp
      ctx.AddIdentifier { Name = srd.Name; 
                          RetTyp = Infon; 
                          ArgsTyp = realArgsTyp }

    member private ctx.AddFunction (sfd: SimpleFunctionDeclaration) =
      let argsTyp = List.map (fun (_, st) -> types.LiftType st) sfd.Args
      let argsNames = List.map (fun (arg, _) -> arg) sfd.Args
      types.AddLevel sfd.Args
      types.AddToCurrentLevel ("Ret", sfd.RetTyp)
      let retTyp = types.LiftType sfd.RetTyp
      let body = ctx.LiftSimpleMetaTerm(sfd.Body)
      types.PopLevel()
      if body.CheckTyp() <> Type.Bool then
        failwith <| "Function " + sfd.Name + " body has type " + body.Typ().ToString() 
                    + " but Bool was expected" 
      ctx.AddMacro  { Name = sfd.Name; 
                      RetTyp = retTyp; 
                      ArgsTyp = argsTyp } body argsNames

    member private ctx.MacroConditions (solvedMacros: List<MetaTerm>) =
      if solvedMacros.Count > 0 then
        let ret = Primitives.andBool <| Seq.toList solvedMacros
        solvedMacros.Clear()
        ret
      else
        Const(BoolConstant(true))

    member ctx.LiftSimpleMetaTerm (smt: SimpleMetaTerm) : MetaTerm =
      let solvedMacros = new List<MetaTerm>()
      let rec traverse : SimpleMetaTerm -> MetaTerm = 
        fun smt ->
        match smt with
        | SimpleApp(f, smts) -> 
          // check if it is a rule
          if f = "rule" then
            match smts with
            | cs :: cw :: a :: vars -> 
              let args = List.map (fun var -> match var with 
                                              | SimpleVar(v) -> 
                                                let parts = v.Split ':'
                                                (parts.[0], parts.[1])
                                              | _ -> failwith "expecting var in rule") vars
              types.AddLevel args
              let cs', cw', a' = traverse cs, traverse cw, traverse a
              types.PopLevel()
              let conds = ctx.MacroConditions solvedMacros
              let cs'' = App(ctx.Identifiers.["andInfon"], [cs'; App(ctx.Identifiers.["asInfon"], [conds])])
              App(ctx.Identifiers.["rule"], [cs''; cw'; a'])
            | _ -> failwith "Too few arguments in rule"
          else
            let mts = List.map traverse smts
            // check if it is a macro
            let found, value = ctx.Macros.TryGetValue f
            if found then
              let func, body, argsNames = value
              let subst = new Substitution()
              for mt, argName in List.zip mts argsNames do
                subst.[{ Name = argName; Typ = mt.Typ() }] <- mt
              let newRet = Var(freshVar func.RetTyp)
              subst.[{ Name = "Ret"; Typ = func.RetTyp }] <- newRet
              solvedMacros.Add(body.ApplySubstitution subst) |> ignore
              newRet
            else
              // check if it is a primitive operator
              let found, func = ctx.Identifiers.TryGetValue f
              if found then
                App(func, mts)
              elif f = "eq" || f = "neq" || f = "lt" || f = "lte" || f = "gt" || f = "gte" || 
                    f = "plus" || f = "times" || f = "minus" || f = "uminus" || f = "div" || f = "and" then
                let simpleTyp = mts.[0].Typ().ToString()
                match ctx.SolveOverloadOperator f simpleTyp with
                | Some func -> App(func, mts)
                | None -> failwith <| "There is no " + f + " operator for " + simpleTyp
              else
                failwith <| "Undefined identifier: " + f + " on " + (sprintf "%A" smt)
        | SimpleConst(c) ->
          match c with
          | BoolSimpleConstant b -> Const(BoolConstant b)
          | IntSimpleConstant i -> Const(SubstrateConstant i)
          | FloatSimpleConstant f -> Const(SubstrateConstant f)
          | StringSimpleConstant s -> Const(SubstrateConstant s)
          | PrincipalSimpleConstant p -> Const(PrincipalConstant p)
        | SimpleVar(v) ->
          match types.VariableType v with
          | None -> failwith <| "Undeclared variable: " + v
          | Some typ -> Var({ Name = v; Typ = typ })
      let mainTerm = traverse smt
      let conditions = ctx.MacroConditions solvedMacros
      let finalTerm = if mainTerm.Typ() = Bool then
                        App(primitives.["andBool"], [conditions; mainTerm])
                      elif mainTerm.Typ() = Infon then
                        App(primitives.["andInfon"], [App(primitives.["asInfon"], [conditions]); mainTerm])
                      else
                        if solvedMacros.Count > 0 then
                          failwith <| sprintf "Pending macro conditions when lifting term: %A" smt
                        else
                          mainTerm
      mainTerm.CheckTyp() |> ignore
      let normalTerm = normalize mainTerm
      normalTerm

    member private ctx.SolveOverloadOperator (f: string) (simpleTyp: string) =
      let found, func = ctx.Identifiers.TryGetValue (f + simpleTyp)
      if found then
        Some func
      else
        None

    member ctx.LiftSimplePolicy (sp: SimplePolicy) =
      let rules = List.map (fun rule -> typeCheck (ctx.LiftSimpleMetaTerm rule) Rule) (Seq.toList sp.Rules)
      { Rules =  rules }

    member ctx.LoadSimpleSignature (ss: SimpleSignature) =
      Seq.iter ctx.AddType ss.TypeDeclarations 
      Seq.iter ctx.AddTable ss.TableDeclarations
      Seq.iter ctx.AddRelation ss.RelationDeclarations
      Seq.iter ctx.AddFunction ss.FunctionDeclarations

    member ctx.LiftSimpleSignature (ss: SimpleSignature) =
      let tds = List.map 
                  (fun (std: SimpleTableDeclaration) -> 
                    { TableDeclaration.Name = std.Name; Cols = ctx.LiftArgs std.Cols}) 
                  (Seq.toList ss.TableDeclarations)
      let rds = List.map 
                  (fun (srd: SimpleRelationDeclaration) -> 
                    { RelationDeclaration.Name = srd.Name; Args = ctx.LiftArgs srd.Args}) 
                  (Seq.toList ss.RelationDeclarations)
      { Tables = tds; Relations = rds }
      
    member ctx.LiftSimpleAssembly (sa: SimpleAssembly) =
      ctx.LoadSimpleSignature sa.Signature
      { Signature = ctx.LiftSimpleSignature sa.Signature; 
        Policy = ctx.LiftSimplePolicy sa.Policy }

