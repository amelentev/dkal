namespace Microsoft.Research.Dkal.SimpleParser
  
  open System.Collections.Generic

  open Microsoft.Research.Dkal.SimpleParser.SimpleAst
  open Microsoft.Research.Dkal.Ast

  type Context() =
    let types = new Dictionary<SimpleType, Type>()
    let relations = new Dictionary<SimpleRelationDeclaration, Function>()
    let functions = new Dictionary<SimpleFunctionDeclaration, Function>()

    do
      // primitive types
      types.Add("infon", Type.Infon)
      types.Add("int", Type.Int)
      types.Add("principal", Type.Principal)
      types.Add("string", Type.String)

      // primitive relations
      relations.Add(SimpleRelationDeclaration.infonAnd, Function.infonAnd)
      relations.Add(SimpleRelationDeclaration.infonImplies, Function.infonImplies)
      relations.Add(SimpleRelationDeclaration.infonSaid, Function.infonSaid)
      relations.Add(SimpleRelationDeclaration.asInfon, Function.asInfon)

      // primitive functions
      // ...

    member ctx.Types = types
    member ctx.Relations = relations
    member ctx.Functions = functions

    member ctx.AddRelation (srd: SimpleRelationDeclaration) =
      let realArgsTyp = List.map (fun st -> ctx.Types.[st]) srd.ArgsTyp
      ctx.Relations.Add(srd, { Name = srd.Name; RetTyp = Infon; ArgsTyp = realArgsTyp })

    member ctx.AddFunction (sfd: SimpleFunctionDeclaration) =
      let argsTyp = List.map (fun (_, st) -> ctx.Types.[st]) sfd.Args
      let retTyp = ctx.Types.[sfd.RetTyp]
      ctx.Functions.Add(sfd, { Name = sfd.Name; RetTyp = retTyp; ArgsTyp = argsTyp})