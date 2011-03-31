namespace Microsoft.Research.Dkal.SimpleParser

  open System.Collections.Generic

  open Microsoft.Research.Dkal.Ast
  open Microsoft.Research.Dkal.SimpleParser.SimpleAst

  type SimpleMetaTermLifter(ctx: Context) =
    let nameToRelationDeclaration = new Dictionary<string, SimpleRelationDeclaration>()
    let nameToFunctionDeclaration = new Dictionary<string, SimpleFunctionDeclaration>()
    do
      for srd in ctx.Relations.Keys do
        nameToRelationDeclaration.[srd.Name] <- srd
      for sfd in ctx.Functions.Keys do
        nameToFunctionDeclaration.[sfd.Name] <- sfd

    member smtl.LiftSimpleMetaTerm (smt: SimpleMetaTerm) =
      match smt with
      | SimpleApp(f, smts) -> 
        let mts = List.map (fun smt -> smtl.LiftSimpleMetaTerm smt) smts
        let found, srd = nameToRelationDeclaration.TryGetValue f
        if found then
          App(ctx.Relations.[srd], mts)
        else
          let found, sfd = nameToFunctionDeclaration.TryGetValue f
          if found then
            App(ctx.Functions.[sfd], mts)
          else
            failwith <| "Undeclared identifier: " + f
      | SimpleConst(c) ->
        match c with
        | BoolSimpleConstant b -> Const(BoolConstant b)
        | IntSimpleConstant i -> Const(IntConstant i)
        | FloatSimpleConstant f -> Const(FloatConstant f)
        | StringSimpleConstant s -> Const(StringConstant s)
      | SimpleVar(v) ->
        failwith "not supported yet"