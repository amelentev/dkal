namespace Microsoft.Research.Dkal.Ast.Infon.Syntax.Simple
  
  open System.Collections.Generic

  open Microsoft.Research.Dkal.Ast.Infon.Syntax.Simple.SimpleAst
  open Microsoft.Research.Dkal.Interfaces
  open Microsoft.Research.Dkal.Globals
  open Microsoft.Research.Dkal.Ast
  open Microsoft.Research.Dkal.Ast.Tree
  open Microsoft.Research.Dkal.Ast.Infon
  open Microsoft.Research.Dkal.Ast.Syntax.ParsingContext
  open Microsoft.Research.Dkal.Substrate.Factories

  /// A Lifter is responsible for lifting untyped SimpleMetaTerms into typed
  /// MetaTerms. It solves the macros that appear in the SimpleMetaTerms and 
  /// uses the type information from relation declarations, type renames, etc.
  type Lifter(context: IParsingContext) =
    let substrates = new HashSet<ISubstrate>()
    
    /// For each declared relation it holds the appropiate Function element to 
    /// be used in App nodes
    let relations = new Dictionary<string, Function>()

    /// Given a SimpleAssembly it returns its corresponding Assembly
    member ctx.LiftSimpleAssembly (sa: SimpleAssembly) =
      ctx.LoadSimpleSignature sa.Signature
      { Signature = ctx.LiftSimpleSignature sa.Signature; 
        Policy = ctx.LiftSimplePolicy sa.Policy }

    /// Given a SimplePolicy it returns its corresponding Policy
    member ctx.LiftSimplePolicy (sp: SimplePolicy) =
      let rules = List.map (fun rule -> ctx.LiftSimpleMetaTerm rule <| Some Type.Rule) (Seq.toList sp.Rules)
      { Rules =  rules }

    /// Given a SimpleSignature it returns its corresponding Signature
    member ctx.LiftSimpleSignature (ss: SimpleSignature) =
      let sds = substrates |> Seq.toList
      let rds = List.map 
                  (fun (srd: SimpleRelationDeclaration) -> 
                    { RelationDeclaration.Name = srd.Name; Args = ctx.LiftArgs srd.Args}) 
                  <| Seq.toList ss.RelationDeclarations
      { Substrates = sds; Relations = rds }

    /// It loads a SimpleSignature into the Context, saving the relation 
    /// declarations, macro definitions, etc.
    member ctx.LoadSimpleSignature (ss: SimpleSignature) =
      Seq.iter ctx.AddSubstrate ss.SubstrateDeclarations
      Seq.iter ctx.AddType ss.TypeDeclarations 
      Seq.iter ctx.AddRelation ss.RelationDeclarations
      Seq.iter ctx.AddMacro ss.MacroDeclarations

    member private ctx.HasIdentifier (name: string) =
      context.HasMacro name || relations.ContainsKey name

    /// Given a list of SimpleArgs (SimpleVariable * SimpleType) returns a 
    /// list of Variables
    member private ctx.LiftArgs (args: SimpleArg list) : IVar list = 
      List.map (fun (name, t) -> { Name = name; Type = context.TypeFromName t } :> IVar) args

    /// Adds a substrate declaration so that it can be used in AsInfon 
    /// SimpleMetaTerms. This information is used when lifting these
    member private ctx.AddSubstrate (ssd: SimpleSubstrateDeclaration) = 
      let s = SubstrateFactory.Substrate ssd.Kind ssd.Args ssd.Namespaces
      SubstrateMap.AddSubstrate s

    /// Adds a new type rename
    member private ctx.AddType (std: SimpleTypeDeclaration) =
      context.AddTypeRename(std.NewTyp, std.TargetTyp)

    /// Adds a relation declaration and saves it so that now SimpleMetaTerms
    /// can use it.
    member private ctx.AddRelation (srd: SimpleRelationDeclaration) =
      if ctx.HasIdentifier srd.Name then
        failwith <| "Identifier " + srd.Name + " defined twice"
      let _, argsTyp = List.unzip srd.Args
      let realArgsTyp = List.map (fun st -> context.TypeFromName st) argsTyp
      relations.[srd.Name] <- { Name = srd.Name; 
                                RetType = Type.Infon; 
                                ArgsType = realArgsTyp;
                                Identity = None }

    /// Adds a macro declaration, lifting its body into a MetaTerm and saving
    /// it so that now SimpleMetaTerms can use it. All referenced macros in the
    /// body must have been added before this one (e.g., no recursion allowed).
    member private ctx.AddMacro (smd: SimpleMacroDeclaration) =
      if ctx.HasIdentifier smd.Name then
        failwith <| "Identifier " + smd.Name + " defined twice"
      let retTyp = context.TypeFromName smd.RetTyp
      let args = List.map (fun (arg, st) -> {Name = arg; Type = context.TypeFromName st} :> IVar) smd.Args
      let localContext = new LocalParsingContext(args @ [{Name="Ret"; Type=retTyp}], context)
      let substrate = SubstrateMap.GetSubstrate smd.Namespace
      let parser = SubstrateParserFactory.SubstrateParser substrate "simple" smd.Namespace (Some <| (localContext :> IParsingContext))
      let body = parser.ParseTerm smd.Body
      match body with
      | :? ISubstrateQueryTerm as body when body.Type = Type.SubstrateQuery ->
        context.AddMacro(smd.Name, retTyp, body, args)
      | _ ->
        failwithf "Macro %O body must be substrate query expression, regardless of return type" smd.Name

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
      let solvedMacros = new List<ISubstrateQueryTerm>()
      let rec traverse (smt: SimpleMetaTerm) (typ: IType option) (context: IParsingContext) = 
        let term = 
          match smt with
          | SimpleVarDeclaration(args, smt) ->
            let localParsingContext = new LocalParsingContext(ctx.LiftArgs args, context)
            let mt = traverse smt (Some Type.Rule) localParsingContext 
            mt
  //        | SimpleApp([], f, []) when f = "nil" ->
  //          match typ with
  //          | None -> failwith "Failed to infer sequence type from context"
  //          | Some (Sequence typ') -> Nil typ'
  //          | Some typ' -> failwith <| "Expecting a sequence type, found " + typ'.ToString() + " on " + (sprintf "%A" smt)
  //        | SimpleApp([], f, [e; list]) when f = "cons" ->
  //          let e', list' = 
  //            match typ with
  //            | Some (Sequence typ') -> traverse e (Some typ'), traverse list (Some <| Sequence typ')
  //            | None -> 
  //              let e' = traverse e None
  //              e', traverse list (Some <| Sequence(e'.Type :?> Type))
  //            | Some typ' -> failwith <| "Expecting a sequence type, found " + typ'.ToString() + " on " + (sprintf "%A" smt)
  //          Cons e' list'
          | SimpleApp(f, smts) ->
              // check if it is a macro
              if context.HasMacro f then
                let args = context.GetMacroArgs f
                let mutable concreteArgs = []
                for smt, arg in List.zip smts args do
                  let mt = traverse smt (Some arg.Type) context
                  concreteArgs <- concreteArgs @ [mt]
                let ret, solvedMacro = context.ApplyMacro(f, concreteArgs)
                solvedMacros.Add solvedMacro |> ignore
                ret
              else
                // check if it is a relation
                let found, func = relations.TryGetValue f
                if found then
                  let mts = List.map2 (fun smt t -> traverse smt (Some t) context) smts func.ArgsType
                  App(func, mts)
                else
                  // check if it is a primitive operator
                  match Primitives.SolveFunction f with
                  | Some func -> 
                    if not(complies func.RetType typ) then failDueToType smt typ
                    if smts.Length <> func.ArgsType.Length then
                      failwithf "Incorrect amount of arguments on %O: %A" f smts
                    let mts = List.map2 (fun smt t -> traverse smt (Some t) context) smts func.ArgsType
                    App(func, mts)
                  | None ->
                    failwith <| "Undefined identifier: " + f + " on " + (sprintf "%A" smt)
          | SimpleConst(c) ->
            match c with
            | BoolSimpleConstant b when complies Type.Boolean typ -> Const(Constant b)
            | Int32SimpleConstant i when complies Type.Int32 typ -> Const(Constant i)
            | DoubleSimpleConstant f when complies Type.Double typ -> Const(Constant f)
            | StringSimpleConstant s when complies Type.String typ -> Const(Constant s)
            | PrincipalSimpleConstant p when complies Type.Principal typ -> Const(PrincipalConstant p)
            | _ -> failDueToType smt typ
          | SimpleVar(v) ->
            let ret = 
              if v = "Me" then
                Const <| PrincipalConstant(context.Me)
              else
                Var({ Name = v; Type = context.VariableType v })
            if complies ret.Type typ then 
              ret
            else
              failDueToType smt typ
          | SimpleSubstrate(ns, exp) ->
            let substrate = SubstrateMap.GetSubstrate ns
            let parser = SubstrateParserFactory.SubstrateParser substrate "simple" ns (Some context)
            let t = parser.ParseTerm exp :> ITerm
            if complies t.Type typ then
              t
            else
              failDueToType smt typ
        if term.Type = Type.Infon then
          let termWithSolvedMacros = AndInfon <| List.map AsInfon (Seq.toList solvedMacros) @ [term]
          solvedMacros.Clear()
          termWithSolvedMacros
        else
          term
        
      let mainTerm = traverse smt typ context
      if solvedMacros.Count > 0 then
        failwithf "Unresolved macros in %O" mainTerm
      mainTerm.Normalize()

    



