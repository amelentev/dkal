namespace Microsoft.Research.Dkal.SimpleSyntax
  
  open System.Collections.Generic

  open Microsoft.Research.Dkal.SimpleSyntax.SimpleAst
  open Microsoft.Research.Dkal.SimpleSyntax.TypeErrors
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

    do
      // Load primitive identifiers
      for nameFunc in primitives do
        identifiers.[nameFunc.Key] <- nameFunc.Value

    /// Given a SimpleAssembly it returns its corresponding Assembly
    member ctx.LiftSimpleAssembly (sa: SimpleAssembly) =
      ctx.LoadSimpleSignature sa.Signature
      { Signature = ctx.LiftSimpleSignature sa.Signature; 
        Policy = ctx.LiftSimplePolicy sa.Policy }

    /// Given a SimplePolicy it returns its corresponding Policy
    member ctx.LiftSimplePolicy (sp: SimplePolicy) =
      let rules = List.map (fun rule -> typeCheck (ctx.LiftSimpleMetaTerm rule) Rule) (Seq.toList sp.Rules)
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
      let body = ctx.LiftSimpleMetaTerm(smd.Body)
      types.PopLevel()
      if body.CheckTyp() <> Type.Bool then
        failwith <| "Function " + smd.Name + " body has type " + body.Typ().ToString() 
                    + " but Bool was expected" 
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

    /// Given a SimpleMetaTerm, it returns its corresponding MetaTerm. All 
    /// macros are solved and its conditions are added as an extra AsInfon
    /// expression in the end.
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
              let cs'' = Normalizer.normalize <| 
                          App(identifiers.["andInfon"], [cs'; App(identifiers.["asInfon"], [conds; substrates.["Default"]])])
              App(identifiers.["rule"], [cs''; cw'; a'])
            | _ -> failwith "Wrong arguments in rule"
          elif f = "asInfon" then
            match smts with
            | [query; SimpleVar(substrate)] -> 
              let query' = traverse query
              AsInfon(query', substrates.[substrate])
            | _ -> failwith "Wrong arguments in asInfon"
          else
            let mts = List.map traverse smts
            // check if it is a macro
            let found, value = macros.TryGetValue f
            if found then
              let retTyp, body, args = value
              let mutable subst = Substitution.Id
              for mt, (argName, argTyp) in List.zip mts args do
                if types.LiftType argTyp <> mt.Typ() then
                  failwith <| "Expecting " + argTyp + " macro argument, but found " + mt.Typ().ToString()
                subst <- subst.Extend ({Name = argName; Typ = mt.Typ()}, mt)
              let newRet = Var(freshVar retTyp)
              subst <- subst.Extend ({Name = "Ret"; Typ = retTyp}, newRet)
              solvedMacros.Add(subst.Apply body) |> ignore
              newRet
            else
              // check if it is a primitive operator
              let found, func = identifiers.TryGetValue f
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
          | IntSimpleConstant i -> Const(SubstrateElemConstant i)
          | FloatSimpleConstant f -> Const(SubstrateElemConstant f)
          | StringSimpleConstant s -> Const(SubstrateElemConstant s)
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
                        App(primitives.["andInfon"], [App(primitives.["asInfon"], [conditions; substrates.["Default"]]); mainTerm]) 
                      else
                        if solvedMacros.Count > 0 then
                          failwith <| sprintf "Pending macro conditions when lifting term: %A" smt
                        else
                          mainTerm
      mainTerm.CheckTyp() |> ignore
      let normalTerm = normalize mainTerm
      normalTerm

    /// Given an overloaded function name and the type of one of its parameters
    /// it looks the list of identifiers to see if there is a match.
    /// Overloaded functions convention is that the name gets the type of its 
    /// parameter appended in the end. (Only one type can be overloaded, but
    /// this is enough to accomodate sum, conjunction, multiplication, etc.)
    member private ctx.SolveOverloadOperator (f: string) (simpleTyp: string) =
      let found, func = identifiers.TryGetValue (f + simpleTyp)
      if found then
        Some func
      else
        None



