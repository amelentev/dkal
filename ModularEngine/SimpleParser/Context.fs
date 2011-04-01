namespace Microsoft.Research.Dkal.SimpleParser
  
  open System.Collections.Generic

  open Microsoft.Research.Dkal.SimpleParser.SimpleAst
  open Microsoft.Research.Dkal.Ast
  open Microsoft.Research.Dkal.Ast.Primitives

  type Context() =
    let types = new Dictionary<string, Type>()
    let identifiers = new Dictionary<string, Function>()

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

    member ctx.LoadSimplePolicy (sp: SimplePolicy) =
      for std in sp.TypeDeclarations do
        ctx.AddType std
      for std in sp.TableDeclarations do
        ctx.AddTable std
      for srd in sp.RelationDeclarations do
        ctx.AddRelation srd
      for sfd in sp.FunctionDeclarations do
        ctx.AddFunction sfd

    member private ctx.AddIdentifier func =
      if ctx.Identifiers.ContainsKey func.Name then
        failwith <| "Identifier " + func.Name + " defined twice"
      ctx.Identifiers.[func.Name] <- func

    member ctx.AddType (std: SimpleTypeDeclaration) =
      ctx.Types.[std.newTyp] <- ctx.Types.[std.targetTyp]

    member ctx.AddTable (std: SimpleTableDeclaration) =
      for colName, colTyp in std.Cols do
        ctx.AddIdentifier { Name = std.Name + "." + colName
                            RetTyp = ctx.Types.[colTyp]; 
                            ArgsTyp = [];
                            Body = None }

    member ctx.AddRelation (srd: SimpleRelationDeclaration) =
      let realArgsTyp = List.map (fun st -> ctx.Types.[st]) srd.ArgsTyp
      ctx.AddIdentifier { Name = srd.Name; 
                          RetTyp = Infon; 
                          ArgsTyp = realArgsTyp; 
                          Body = None }

    member ctx.AddFunction (sfd: SimpleFunctionDeclaration) =
      let argsTyp = List.map (fun (_, st) -> ctx.Types.[st]) sfd.Args
      let localVars = new Dictionary<SimpleVariable, Type>()
      localVars.["Ret"] <- ctx.Types.[sfd.RetTyp]
      for argName, argTyp in sfd.Args do
        localVars.[argName] <- ctx.Types.[argTyp]
      let retTyp = ctx.Types.[sfd.RetTyp]
      let body =  match sfd.Body with
                  | None -> None
                  | Some body -> 
                    let body = ctx.LiftSimpleMetaTerm(body, localVars)
                    let typ = body.Typ
                    Some body
      ctx.AddIdentifier { Name = sfd.Name; 
                          RetTyp = retTyp; 
                          ArgsTyp = argsTyp; 
                          Body = body }

    member ctx.LiftSimpleMetaTerm (smt: SimpleMetaTerm, localVars: Dictionary<SimpleVariable, Type>) : MetaTerm =
      match smt with
      | SimpleApp(f, smts) -> 
        let mts = List.map (fun smt -> ctx.LiftSimpleMetaTerm(smt, localVars)) smts
        let func = 
          let found, func = ctx.Identifiers.TryGetValue f
          if found then
            func
          elif f = "eq" || f = "neq" || f = "lt" || f = "lte" || f = "gt" || f = "gte" || 
                f = "plus" || f = "times" || f = "minus" || f = "uminus" || f = "div" then
            let simpleTyp = sprintf "%A" mts.[0].Typ
            match ctx.SolveOverloadOperator f simpleTyp with
            | Some func -> func
            | None -> failwith <| "There is no " + f + " operator for " + simpleTyp
          else
            failwith <| "Undefined identifier: " + f
        App(func, mts)
      | SimpleConst(c) ->
        match c with
        | BoolSimpleConstant b -> Const(BoolConstant b)
        | IntSimpleConstant i -> Const(IntConstant i)
        | FloatSimpleConstant f -> Const(FloatConstant f)
        | StringSimpleConstant s -> Const(StringConstant s)
      | SimpleVar(v) ->
        let found, typ = localVars.TryGetValue v
        if found then
          Var({ Name = v; Typ = typ })
        else
          failwith <| "Undeclared variable: " + v

    member ctx.LiftSimpleMetaTerm (smt: SimpleMetaTerm) : MetaTerm =
      ctx.LiftSimpleMetaTerm(smt, new Dictionary<SimpleVariable, Type>())

    member private ctx.SolveOverloadOperator (f: string) (simpleTyp: string) =
      let found, func = ctx.Identifiers.TryGetValue (f + simpleTyp)
      if found then
        Some func
      else
        None
