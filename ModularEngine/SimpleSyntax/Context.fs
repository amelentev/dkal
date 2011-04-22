namespace Microsoft.Research.Dkal.SimpleSyntax
  
  open System.Collections.Generic

  open Microsoft.Research.Dkal.SimpleSyntax.SimpleAst
  open Microsoft.Research.Dkal.Ast
  open Microsoft.Research.Dkal.Ast.Normalizer

  /// A Context is responsible for lifting untyped SimpleMetaTerms into typed
  /// MetaTerms. It solves the macros that appear in the SimpleMetaTerms and 
  /// uses the type information from relation declarations, type renames, etc.
  type Context() =
    /// For each substrate name it keeps the Substrate declaration MetaTerm
    let substrates = new Dictionary<string, MetaTerm>()

    /// Holds type information (SimpleType to Type mapping, and SimpleVariable 
    /// type info)
    let types = new TypeInfo()

    /// For each declared identifier (relations, tables, etc.) it holds the 
    /// appropiate Function element to be used in MetaTerm App nodes
    let identifiers = new Dictionary<string, Function>()

    /// For each declared macro it holds its return Type, its body and its args
    /// (arguments Type and names)
    let macros = new Dictionary<string, Type * MetaTerm * SimpleArg list>()

    /// Holds fresh variable ids that are used when solving macros
    let mutable freshVarId = 0

    /// Returns a fresh Variable of the given type
    let freshVar typ = 
      freshVarId <- freshVarId + 1
      { Name = "Tmp" + freshVarId.ToString(); 
        Typ = typ }

    /// Given a SimpleAssembly it returns its corresponding Assembly
    member ctx.LiftSimpleAssembly (sa: SimpleAssembly) =
      ctx.LoadSimpleSignature sa.Signature
      { Signature = ctx.LiftSimpleSignature sa.Signature; 
        Policy = ctx.LiftSimplePolicy sa.Policy }

    /// Given a SimplePolicy it returns its corresponding Policy
    member ctx.LiftSimplePolicy (sp: SimplePolicy) =
      let rules = List.map (fun rule -> ctx.LiftSimpleMetaTerm rule Rule) (Seq.toList sp.Rules)
      { Rules =  rules }

    /// Given a SimpleSignature it returns its corresponding Signature
    member ctx.LiftSimpleSignature (ss: SimpleSignature) =
      let sds = List.map
                  (fun (ssd: SimpleSubstrateDeclaration) ->
                    { Name = ssd.Name; Decl = substrates.[ssd.Name]})
                  <| Seq.toList ss.SubstrateDeclarations
      let tds = List.map 
                  (fun (std: SimpleTableDeclaration) -> 
                    { TableDeclaration.Name = std.Name; Cols = ctx.LiftArgs std.Cols}) 
                  <| Seq.toList ss.TableDeclarations
      let rds = List.map 
                  (fun (srd: SimpleRelationDeclaration) -> 
                    { RelationDeclaration.Name = srd.Name; Args = ctx.LiftArgs srd.Args}) 
                  <| Seq.toList ss.RelationDeclarations
      { Substrates = sds; Tables = tds; Relations = rds }

    /// It loads a SimpleSignature into the Context, saving the relation 
    /// declarations, macro definitions, etc.
    member ctx.LoadSimpleSignature (ss: SimpleSignature) =
      Seq.iter ctx.AddSubstrate ss.SubstrateDeclarations
      Seq.iter ctx.AddType ss.TypeDeclarations 
      Seq.iter ctx.AddTable ss.TableDeclarations
      Seq.iter ctx.AddRelation ss.RelationDeclarations
      Seq.iter ctx.AddMacro ss.MacroDeclarations

    /// Given a list of SimpleArgs (SimpleVariable * SimpleType) returns a 
    /// list of Variables
    member private ctx.LiftArgs (args: SimpleArg list) : Variable list = 
      List.map (fun (name, typ) -> { Name = name; Typ = types.LiftType typ }) args

    /// Returns true if the given name is already associated with an identifier
    /// (either primitive function, table, relation, macro, etc.)
    member private ctx.HasIdentifier name =
      identifiers.ContainsKey name || macros.ContainsKey name 

    /// Adds a the given function as a new identifier in the Context and saves
    /// it so that it can be used to lift SimpleMetaTerms
    member private ctx.AddIdentifier func =
      if ctx.HasIdentifier func.Name then
        failwith <| "Identifier " + func.Name + " defined twice"
      identifiers.[func.Name] <- func

    /// Adds a substrate declaration so that it can be used in AsInfon 
    /// SimpleMetaTerms. This information is used when lifting these
    member private ctx.AddSubstrate (ssd: SimpleSubstrateDeclaration) = 
      substrates.[ssd.Name] <- 
        match ssd.Kind, ssd.Args with
        | "sql", [StringSimpleConstant(cs)] -> Sql(Const(SubstrateElemConstant(cs)))
        | "xml", [StringSimpleConstant(file)] -> Xml(Const(SubstrateElemConstant(file)))
        | _ -> failwith "Unrecognized substrate declaration"

    /// Adds a new type rename
    member private ctx.AddType (std: SimpleTypeDeclaration) =
      types.AddTypeRename std.NewTyp std.TargetTyp

    /// Adds a table declaration and saves it so that now SimpleMetaTerms
    /// can use it.
    member private ctx.AddTable (std: SimpleTableDeclaration) =
      for colName, colTyp in std.Cols do
        ctx.AddIdentifier { Name = std.Name + "." + colName
                            RetTyp = types.LiftType colTyp; 
                            ArgsTyp = [] }

    /// Adds a relation declaration and saves it so that now SimpleMetaTerms
    /// can use it.
    member private ctx.AddRelation (srd: SimpleRelationDeclaration) =
      let _, argsTyp = List.unzip srd.Args
      let realArgsTyp = List.map (fun st -> types.LiftType st) argsTyp
      ctx.AddIdentifier { Name = srd.Name; 
                          RetTyp = Infon; 
                          ArgsTyp = realArgsTyp }

    /// Adds a macro declaration, lifting its body into a MetaTerm and saving
    /// it so that now SimpleMetaTerms can use it. All referenced macros in the
    /// body must have been added before this one (e.g., no recursion allowed).
    member private ctx.AddMacro (smd: SimpleMacroDeclaration) =
      if ctx.HasIdentifier smd.Name then
        failwith <| "Identifier " + smd.Name + " defined twice"
      types.AddLevel smd.Args
      types.AddToCurrentLevel ("Ret", smd.RetTyp)
      let retTyp = types.LiftType smd.RetTyp
      let body = ctx.LiftSimpleMetaTerm smd.Body Type.Bool
      types.PopLevel()
      macros.[smd.Name] <- (retTyp, body, smd.Args)

    /// Given a list of MetaTerm boolean conditions given by solvedMacros, it 
    /// returns a single boolean MetaTerm that encodes them all (conjunction).
    member private ctx.MacroConditions (solvedMacros: List<MetaTerm>) =
      if solvedMacros.Count > 0 then
        let ret = AndBool <| Seq.toList solvedMacros
        solvedMacros.Clear()
        ret
      else
        Const(BoolConstant(true))

    /// Given a SimpleMetaTerm smt and a Type t, it returns its the 
    /// corresponding MetaTerm, if smt encodes a MetaTerm of Type t. All 
    /// macros are solved and its conditions are added as an extra AsInfon
    /// expression in the end.
    member ctx.LiftSimpleMetaTerm (smt: SimpleMetaTerm) (typ: Type) : MetaTerm =
      let complies (typ: Type) (typ': Type option) =
        match typ' with
        | Some typ' when typ' = typ -> true
        | None -> true
        | _ -> false
      let failDueToType (smt: SimpleMetaTerm) (typ: Type option) = 
        failwith <| "Expecting a " + typ.Value.ToString() + "MetaTerm, found: " + (sprintf "%A" smt)
      let solvedMacros = new List<MetaTerm>()
      let rec traverse (smt: SimpleMetaTerm) (typ: Type option) = 
        match smt with
        | SimpleApp(args, f, [cs; cw; a]) when f = "rule" ->
            if not(complies Rule typ) then failDueToType smt typ
            types.AddLevel args
            let cs', cw', a' = traverse cs (Some Infon), traverse cw (Some Infon), traverse a (Some Action)
            types.PopLevel()
            let conds = ctx.MacroConditions solvedMacros
            let cs'' = Normalizer.normalize <| AndInfon([cs'; AsInfon(conds, substrates.["Default"])])
            RuleRule(cs'', cw', a')
        | SimpleApp([], f, [query; SimpleVar(substrate)]) when f = "asInfon" ->
            if not(complies Infon typ) then failDueToType smt typ
            let query' = traverse query (Some Bool)
            AsInfon(query', substrates.[substrate])
        | SimpleApp([], f, []) when f = "nil" ->
          match typ with
          | None -> failwith "Failed to infer sequence type from context"
          | Some (Sequence typ') -> Nil typ'
          | Some typ' -> failwith <| "Expecting a sequence type, found " + typ'.ToString() + " on " + (sprintf "%A" smt)
        | SimpleApp([], f, [e; list]) when f = "cons" ->
          let e', list' = 
            match typ with
            | Some (Sequence typ') -> traverse e (Some typ'), traverse list (Some <| Sequence typ')
            | None -> 
              let e' = traverse e None
              e', traverse list (Some <| Sequence(e'.Typ()))
            | Some typ' -> failwith <| "Expecting a sequence type, found " + typ'.ToString() + " on " + (sprintf "%A" smt)
          Cons e' list'
        | SimpleApp([], f, smts) ->
            // check if it is a macro
            let found, value = macros.TryGetValue f
            if found then
              let retTyp, body, args = value
              if not(complies retTyp typ) then failDueToType smt typ
              let mutable subst = Substitution.Id
              for smt, (argName, argTyp) in List.zip smts args do
                let mt = traverse smt (Some <| types.LiftType argTyp)
                subst <- subst.Extend ({Name = argName; Typ = mt.Typ()}, mt)
              let newRet = Var(freshVar retTyp)
              subst <- subst.Extend ({Name = "Ret"; Typ = retTyp}, newRet)
              solvedMacros.Add(subst.Apply body) |> ignore
              newRet
            else
              // check if it is a user defined relation/table/etc.
              let found, func = identifiers.TryGetValue f
              if found then
                let mts = List.map2 (fun smt t -> traverse smt (Some t)) smts func.ArgsTyp
                App(func, mts)
              else
                // check if it is a primitive operator
                match Primitives.SolveFunction f with
                | Some func -> 
                  if not(complies func.RetTyp typ) then failDueToType smt typ
                  let mts = List.map2 (fun smt t -> traverse smt (Some t)) smts func.ArgsTyp
                  App(func, mts)
                | None ->
                  // check if it is an overloaded operator
                  if not(smts.IsEmpty) then
                    let mt0 = traverse smts.[0] None
                    match Primitives.SolveOverloadOperator f (mt0.Typ()) with
                    | Some func -> 
                      if not(complies func.RetTyp typ) then failDueToType smt typ
                      let mts = List.map2 (fun smt t -> traverse smt (Some t)) smts func.ArgsTyp
                      App(func, mts)
                    | None -> 
                      failwith <| "Undefined identifier: " + f + " on " + (sprintf "%A" smt)
                  else
                    failwith <| "Undefined identifier: " + f + " on " + (sprintf "%A" smt)
        | SimpleConst(c) ->
          match c with
          | BoolSimpleConstant b when complies Type.Bool typ -> Const(BoolConstant b)
          | IntSimpleConstant i when complies Type.Int typ -> Const(SubstrateElemConstant i)
          | FloatSimpleConstant f when complies Type.Float typ -> Const(SubstrateElemConstant f)
          | StringSimpleConstant s when complies Type.String typ -> Const(SubstrateElemConstant s)
          | PrincipalSimpleConstant p when complies Type.Principal typ -> Const(PrincipalConstant p)
          | _ -> failDueToType smt typ
        | SimpleVar(v) ->
          match types.VariableType v with
          | None -> failwith <| "Undeclared variable: " + v
          | Some typ' when complies typ' typ -> Var({ Name = v; Typ = typ' })
          | _ -> failDueToType smt typ
        | _ -> failwith <| "Malformed SimpleMetaTerm"
      let mainTerm = traverse smt (Some typ)
      let conditions = ctx.MacroConditions solvedMacros
      let finalTerm = if mainTerm.Typ() = Bool then
                        AndBool([conditions; mainTerm])
                      elif mainTerm.Typ() = Infon then
                        AndInfon([AsInfon(conditions, substrates.["Default"]); mainTerm]) 
                      else
                        if solvedMacros.Count > 0 then
                          failwith <| sprintf "Pending macro conditions when lifting term: %A" smt
                        else
                          mainTerm
      mainTerm.CheckTyp() |> ignore // TODO: is it necessary?
      let normalTerm = normalize mainTerm
      normalTerm

    



