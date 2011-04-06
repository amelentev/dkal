namespace Microsoft.Research.Dkal.SimpleSyntax
  
  open System.Collections.Generic

  open Microsoft.Research.Dkal.SimpleSyntax.SimpleAst
  open Microsoft.Research.Dkal.Ast
  open Microsoft.Research.Dkal.Ast.Primitives
  open Microsoft.Research.Dkal.Ast.Normalizer

  type Context() =
    let types = new Dictionary<string, Type>()
    let identifiers = new Dictionary<string, Function>()
    let macros = new Dictionary<string, Function * MetaTerm * string list>()

    let mutable freshVarId = 0
    let freshVar typ = 
      freshVarId <- freshVarId + 1
      { Name = "Tmp" + freshVarId.ToString(); 
        Typ = typ }

    do
      // primitive types
      types.Add("bool", Type.Bool)
      types.Add("int", Type.Int)
      types.Add("float", Type.Float)
      types.Add("string", Type.String)
      types.Add("principal", Type.Principal)
      types.Add("infon", Type.Infon)

      // primitive identifiers
      for nameFunc in primitives do
        identifiers.Add(nameFunc.Key, nameFunc.Value)

    member ctx.Types = types
    member ctx.Identifiers = identifiers
    member ctx.Macros = macros

    member ctx.LoadSimpleSignature (ss: SimpleSignature) =
      Seq.iter ctx.AddType ss.TypeDeclarations 
      Seq.iter ctx.AddTable ss.TableDeclarations
      Seq.iter ctx.AddRelation ss.RelationDeclarations
      Seq.iter ctx.AddFunction ss.FunctionDeclarations

    member ctx.LiftSimpleAssembly (sa: SimpleAssembly) =
      ctx.LoadSimpleSignature sa.Signature
      { Signature = ctx.LiftSimpleSignature sa.Signature; 
        Policy = ctx.LiftSimplePolicy sa.Policy }

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

    member private ctx.LiftArgs (args: SimpleArg list) : Variable list = 
      List.map (fun (name, typ) -> { Name = name; Typ = ctx.Types.[typ] }) args

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
      ctx.Types.[std.newTyp] <- ctx.Types.[std.targetTyp]

    member private ctx.AddTable (std: SimpleTableDeclaration) =
      for colName, colTyp in std.Cols do
        ctx.AddIdentifier { Name = std.Name + "." + colName
                            RetTyp = ctx.Types.[colTyp]; 
                            ArgsTyp = [] }

    member private ctx.AddRelation (srd: SimpleRelationDeclaration) =
      let _, argsTyp = List.unzip srd.Args
      let realArgsTyp = List.map (fun st -> ctx.Types.[st]) argsTyp
      ctx.AddIdentifier { Name = srd.Name; 
                          RetTyp = Infon; 
                          ArgsTyp = realArgsTyp }

    member private ctx.AddFunction (sfd: SimpleFunctionDeclaration) =
      let argsTyp = List.map (fun (_, st) -> ctx.Types.[st]) sfd.Args
      let argsNames = List.map (fun (arg, _) -> arg) sfd.Args
      let localVars = ctx.CreateLocalVars sfd.Args
      localVars.["Ret"] <- ctx.Types.[sfd.RetTyp]
      let retTyp = ctx.Types.[sfd.RetTyp]
      let body = ctx.LiftSimpleMetaTerm(sfd.Body, localVars)
      if body.CheckTyp() <> Type.Bool then
        failwith <| "Function " + sfd.Name + " body has type " + body.Typ().ToString() 
                    + " but Bool was expected" 
      ctx.AddMacro  { Name = sfd.Name; 
                      RetTyp = retTyp; 
                      ArgsTyp = argsTyp } body argsNames

    member ctx.LiftSimpleMetaTerm (smt: SimpleMetaTerm, localVars: Dictionary<SimpleVariable, Type>) : MetaTerm =
      let solvedMacros = new List<MetaTerm>()
      let rec traverse : SimpleMetaTerm -> MetaTerm = 
        fun smt ->
        match smt with
        | SimpleApp(f, smts) -> 
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
                  f = "plus" || f = "times" || f = "minus" || f = "uminus" || f = "div" then
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
          let found, typ = localVars.TryGetValue v
          if found then
            Var({ Name = v; Typ = typ })
          else
            failwith <| "Undeclared variable: " + v
      let mainTerm = traverse smt
      let finalTerm = if solvedMacros.Count > 0 then
                        let macroConds = Primitives.boolAnd (Seq.toList solvedMacros)
                        if mainTerm.Typ() = Bool then
                          App(primitives.["boolAnd"], [macroConds; mainTerm])
                        else
                          App(primitives.["infonAnd"], [App(primitives.["asInfon"], [macroConds]); mainTerm])
                      else
                        mainTerm
      normalize finalTerm

    member ctx.LiftSimpleMetaTerm (smt: SimpleMetaTerm) : MetaTerm =
      ctx.LiftSimpleMetaTerm(smt, new Dictionary<SimpleVariable, Type>())

    member private ctx.SolveOverloadOperator (f: string) (simpleTyp: string) =
      let found, func = ctx.Identifiers.TryGetValue (f + simpleTyp)
      if found then
        Some func
      else
        None

    member private ctx.CreateLocalVars (args: SimpleArg list) : Dictionary<SimpleVariable, Type> = 
      let localVars = new Dictionary<SimpleVariable, Type>()
      for argName, argTyp in args do
        localVars.[argName] <- ctx.Types.[argTyp]
      localVars

    member ctx.LiftSimpleAssertion (sa: SimpleAssertion) =
      match sa with
      | SimpleKnow(sk) -> 
        Know({ Fact = ctx.LiftSimpleMetaTerm(sk.Fact, (ctx.CreateLocalVars sk.Args)) })
      | SimpleCommRule(scr) ->
        let localVars = ctx.CreateLocalVars scr.Args
        CommRule({  Trigger = ctx.LiftSimpleMetaTerm(scr.Trigger, localVars); 
                    Target = ctx.LiftSimpleMetaTerm(scr.Target, localVars);
                    Content = ctx.LiftSimpleMetaTerm(scr.Content, localVars) })

    member ctx.LiftSimplePolicy (sp: SimplePolicy) =
      { Assertions = List.map ctx.LiftSimpleAssertion (Seq.toList sp.Assertions) }

