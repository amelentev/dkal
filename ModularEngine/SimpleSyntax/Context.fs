namespace Microsoft.Research.Dkal.Ast.SimpleSyntax
  
  open System.Collections.Generic

  open Microsoft.Research.Dkal.Ast.SimpleSyntax.SimpleAst
  open Microsoft.Research.Dkal.Interfaces
  open Microsoft.Research.Dkal.Globals
  open Microsoft.Research.Dkal.Ast
  open Microsoft.Research.Dkal.Ast.Tree
  open Microsoft.Research.Dkal.Ast.Infon
  open Microsoft.Research.Dkal.Ast.Infon.Normalizer
  open Microsoft.Research.Dkal.Substrate.Factories

  /// A Context is responsible for lifting untyped SimpleMetaTerms into typed
  /// MetaTerms. It solves the macros that appear in the SimpleMetaTerms and 
  /// uses the type information from relation declarations, type renames, etc.
  type Context() =
    let substrates = new HashSet<ISubstrate>()
    
    /// Holds type information (SimpleType to Type mapping, and SimpleVariable 
    /// type info)
    let types = new TypeInfo()

    /// For each declared identifier (relations, tables, etc.) it holds the 
    /// appropiate Function element to be used in MetaTerm App nodes
    let identifiers = new Dictionary<string, Function>()

    /// For each declared macro it holds its return Type, its body and its args
    /// (arguments Type and names)
    let macros = new Dictionary<string, IType * ITerm * SimpleArg list>()

    /// Holds fresh variable ids that are used when solving macros
    let mutable freshVarId = 0

    /// Returns a fresh Variable of the given type
    let freshVar t = 
      freshVarId <- freshVarId + 1
      { Name = "Tmp" + freshVarId.ToString(); 
        Type = t }

    /// Given a SimpleType it returns its corresponding Type
    member ctx.LiftSimpleType (st: SimpleType) =
      types.LiftType st

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

    /// Given a list of SimpleArgs (SimpleVariable * SimpleType) returns a 
    /// list of Variables
    member private ctx.LiftArgs (args: SimpleArg list) : IVar list = 
      List.map (fun (name, t) -> { Name = name; Type = types.LiftType t } :> IVar) args

    /// Returns true if the given name is already associated with an identifier
    /// (either primitive function, table, relation, macro, etc.)
    member private ctx.HasIdentifier name =
      identifiers.ContainsKey name || macros.ContainsKey name 

    /// Adds a the given function as a new identifier in the Context and saves
    /// it so that it can be used to lift SimpleMetaTerms
    member private ctx.AddIdentifier (func: Function) =
      if ctx.HasIdentifier func.Name then
        failwith <| "Identifier " + func.Name + " defined twice"
      identifiers.[func.Name] <- func

    /// Adds a substrate declaration so that it can be used in AsInfon 
    /// SimpleMetaTerms. This information is used when lifting these
    member private ctx.AddSubstrate (ssd: SimpleSubstrateDeclaration) = 
      let s = SubstrateFactory.Substrate ssd.Kind ssd.Args ssd.Namespaces
      SubstrateMap.AddSubstrate s

    /// Adds a new type rename
    member private ctx.AddType (std: SimpleTypeDeclaration) =
      types.AddTypeRename std.NewTyp std.TargetTyp

    /// Adds a relation declaration and saves it so that now SimpleMetaTerms
    /// can use it.
    member private ctx.AddRelation (srd: SimpleRelationDeclaration) =
      let _, argsTyp = List.unzip srd.Args
      let realArgsTyp = List.map (fun st -> types.LiftType st) argsTyp
      ctx.AddIdentifier { Name = srd.Name; 
                          RetType = Type.Infon; 
                          ArgsType = realArgsTyp }

    /// Adds a macro declaration, lifting its body into a MetaTerm and saving
    /// it so that now SimpleMetaTerms can use it. All referenced macros in the
    /// body must have been added before this one (e.g., no recursion allowed).
    member private ctx.AddMacro (smd: SimpleMacroDeclaration) =
      if ctx.HasIdentifier smd.Name then
        failwith <| "Identifier " + smd.Name + " defined twice"
      let macroTypes = new Dictionary<string, IType>()
      for arg, st in smd.Args do
        macroTypes.[arg] <- types.LiftType st
      macroTypes.["Ret"] <- types.LiftType smd.RetTyp

      let retTyp = types.LiftType smd.RetTyp
      let substrate = SubstrateMap.GetSubstrate smd.Namespace
      let parser = SubstrateParserFactory.SubstrateParser substrate "simple" smd.Namespace macroTypes
      let body = parser.ParseTerm smd.Body :> ITerm
      if body.Type = Type.Boolean then
        macros.[smd.Name] <- (retTyp, body, smd.Args)
      else
        failwithf "Macro %O body must be a boolean expression, regardless of return type" smd.Name

    /// Given a list of MetaTerm boolean conditions given by solvedMacros, it 
    /// returns a single boolean MetaTerm that encodes them all (conjunction).
//    member private ctx.MacroConditions (solvedMacros: List<ITerm>) =
//      if solvedMacros.Count > 0 then
//        let ret = AndBool <| Seq.toList solvedMacros
//        solvedMacros.Clear()
//        ret
//      else
//        True

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
      let rec traverse (smt: SimpleMetaTerm) (typ: IType option) = 
        match smt with
        | SimpleApp(args, f, [cs; cw; a]) when f = "rule" ->
            if not(complies Type.Rule typ) then failDueToType smt typ
            types.AddLevel args
            let cs', cw', a' = traverse cs (Some Type.Infon), traverse cw (Some Type.Infon), traverse a (Some Type.Action)
            types.PopLevel()
//            let conds = ctx.MacroConditions solvedMacros
//            let cs'' = Normalizer.normalize <| AndInfon([cs'; AsInfon(ctx.createSubstrateTerm(conds, ""))])
//            RuleRule(cs'', cw', a')
            RuleRule(cs', cw', a')
//        | SimpleApp([], f, [query; SimpleVar(ns)]) when f = "asInfon" ->
//            if not(complies Type.Infon typ) then failDueToType smt typ
//            let query' = traverse query (Some Type.Bool)
//            AsInfon(ctx.createSubstrateTerm(query', ns))
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
        | SimpleApp([], f, smts) ->
            // check if it is a macro
            let found, value = macros.TryGetValue f
            if found then
              let retTyp, body, args = value
              if not(complies retTyp typ) then failDueToType smt typ
              let mutable subst = Substitution.Id
              for smt, (argName, argTyp) in List.zip smts args do
                let mt = traverse smt (Some <| types.LiftType argTyp)
                subst <- subst.Extend ({Name = argName; Type = mt.Type}, mt)
              let newRet = Var(freshVar retTyp)
              subst <- subst.Extend ({Name = "Ret"; Type = retTyp}, newRet)
              solvedMacros.Add(body.Apply subst) |> ignore
              newRet
            else
              // check if it is a user defined relation/table/etc.
              let found, func = identifiers.TryGetValue f
              if found then
                let mts = List.map2 (fun smt t -> traverse smt (Some t)) smts func.ArgsType
                App(func, mts)
              else
                // check if it is a primitive operator
                match Primitives.SolveFunction f with
                | Some func -> 
                  if not(complies func.RetType typ) then failDueToType smt typ
                  let mts = List.map2 (fun smt t -> traverse smt (Some t)) smts func.ArgsType
                  App(func, mts)
                | None ->
                  // check if it is an overloaded operator
//                  if not(smts.IsEmpty) then
//                    let mt0 = traverse smts.[0] None
//                    match Primitives.SolveOverloadOperator f mt0.Type with
//                    | Some func -> 
//                      if not(complies func.RetType typ) then failDueToType smt typ
//                      let mts = List.map2 (fun smt t -> traverse smt (Some t)) smts func.ArgsType
//                      App(func, mts)
//                    | None -> 
//                      failwith <| "Undefined identifier: " + f + " on " + (sprintf "%A" smt)
//                  else
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
          match types.VariableType v with
          | None -> failwith <| "Undeclared variable: " + v
          | Some typ' when complies typ' typ -> Var({ Name = v; Type = typ' })
          | _ -> failDueToType smt typ
        | SimpleSubstrate(ns, exp) ->
          let substrate = SubstrateMap.GetSubstrate ns
          let parser = SubstrateParserFactory.SubstrateParser substrate "simple" ns (types.AllLevels())
          let t = parser.ParseTerm exp :> ITerm
          if complies t.Type typ then
            t
          else
            failDueToType smt typ
        | _ -> failwith <| "Malformed SimpleMetaTerm"
      let mainTerm = traverse smt typ
//      let conditions = ctx.MacroConditions solvedMacros
//      let finalTerm = if mainTerm.Type = Type.Bool then
//                        AndBool([conditions; mainTerm])
//                      elif mainTerm.Type = Type.Infon then
//                        AndInfon([AsInfon(ctx.createSubstrateTerm(conditions, "")); mainTerm])
//                      else
//                        if solvedMacros.Count > 0 then
//                          failwith <| sprintf "Pending macro conditions when lifting term: %A" smt
//                        else
//                          mainTerm
//      let normalTerm = normalize finalTerm
      let normalTerm = normalize mainTerm
      normalTerm

    



