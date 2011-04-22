[<AutoOpen>]
module Microsoft.Research.Dkal.Ast.Builders

  open Microsoft.Research.Dkal.Ast

  // Rule builders
  let RuleRule (cs: MetaTerm, cw: MetaTerm, a: MetaTerm) = 
    if cs.Typ() = Infon && cw.Typ() = Infon && a.Typ() = Action then
      App(Primitives.SolveFunction "rule" |> Option.get, [cs; cw; a])
    else
      failwith "Incorrect parameter type when building rule"

  // Action builders
  let SeqAction (a1: MetaTerm, a2: MetaTerm) = 
    if a1.Typ() = Action && a2.Typ() = Action then
      App(Primitives.SolveFunction "seq" |> Option.get, [a1; a2])
    else
      failwith "Incorrect parameter type when building seq"

  let SendAction (ppal: MetaTerm, i: MetaTerm) = 
    if ppal.Typ() = Principal && i.Typ() = Infon then
      App(Primitives.SolveFunction "send" |> Option.get, [ppal; i])
    else
      failwith "Incorrect parameter type when building send"
    
  let LearnAction (i: MetaTerm) = 
    if i.Typ() = Infon then
      App(Primitives.SolveFunction "learn" |> Option.get, [i])
    else
      failwith "Incorrect parameter type when building learn"
    
  let ForgetAction (i: MetaTerm) = 
    if i.Typ() = Infon then
      App(Primitives.SolveFunction "forget" |> Option.get, [i])
    else
      failwith "Incorrect parameter type when building forget"
    
  let InstallAction (r: MetaTerm) = 
    if r.Typ() = Rule then
      App(Primitives.SolveFunction "install" |> Option.get, [r])
    else
      failwith "Incorrect parameter type when building install"

  let UninstallAction (r: MetaTerm) = 
    if r.Typ() = Rule then
      App(Primitives.SolveFunction "uninstall" |> Option.get, [r])
    else
      failwith "Incorrect parameter type when building uninstall"

  // Substrate builders
  let Sql (cs: MetaTerm) = 
    if cs.Typ() = Type.String then
      App(Primitives.SolveFunction "sql" |> Option.get, [cs])
    else
      failwith "Incorrect parameter type when building sql"

  let Xml (file: MetaTerm) = 
    if file.Typ() = Type.String then
      App(Primitives.SolveFunction "xml" |> Option.get, [file])
    else
      failwith "Incorrect parameter type when building xml"

  // Infon builders
  let EmptyInfon = 
    App(Primitives.SolveFunction "emptyInfon" |> Option.get, [])
    
  let AsInfon (query: MetaTerm, substrate: MetaTerm) = 
    if query.Typ() = Bool && substrate.Typ() = Substrate then
      App(Primitives.SolveFunction "asInfon" |> Option.get, [query; substrate])
    else
      failwith "Incorrect parameter type when building asInfon"
    
  let AndInfon (infons: MetaTerm list) = 
    if List.forall (fun (i: MetaTerm) -> i.Typ() = Infon) infons then
      App({ Name = "and"; 
            RetTyp = Infon; 
            ArgsTyp = List.replicate infons.Length Infon }, infons)
    else
      failwith "Incorrect parameter type when building andInfon"
    
  let ImpliesInfon (i1: MetaTerm, i2: MetaTerm) = 
    if i1.Typ() = Infon && i2.Typ() = Infon then
      App(Primitives.SolveOverloadOperator "implies" Infon |> Option.get, [i1; i2])
    else
      failwith "Incorrect parameter type when building impliesInfon"

  let SaidInfon (ppal: MetaTerm, i: MetaTerm) = 
    if ppal.Typ() = Principal && i.Typ() = Infon then
      App(Primitives.SolveFunction "said" |> Option.get, [ppal; i])
    else
      failwith "Incorrect parameter type when building saidInfon"

  // Bool builders
  let AndBool (bools: MetaTerm list) = 
    if List.forall (fun (b: MetaTerm) -> b.Typ() = Bool) bools then
      App({ Name = "and"; 
            RetTyp = Bool; 
            ArgsTyp = List.replicate bools.Length Bool }, bools)
    else
      failwith "Incorrect parameter type when building andBool"

  let OrBool (bools: MetaTerm list) = 
    if List.forall (fun (b: MetaTerm) -> b.Typ() = Bool) bools then
      App({ Name = "or"; 
            RetTyp = Bool; 
            ArgsTyp = List.replicate bools.Length Bool }, bools)
    else
      failwith "Incorrect parameter type when building orBool"

  // Sequence builders
  let Nil (t: Type) =
    App({ Name = "nil";
          RetTyp = Sequence(t);
          ArgsTyp = [] }, [])

  let Cons (e: MetaTerm) (es: MetaTerm) =
    match e.Typ(), es.Typ() with
    | t, Sequence(t') when t = t' ->
      App({ Name = "cons";
            RetTyp = Sequence(t) ;
            ArgsTyp = [t; Sequence(t)] }, [e; es])
    | _ -> 
      failwith "Incorrect parameter type when building cons"

  // Literal builders
  let Principal (ppal: string) = 
    Const(PrincipalConstant(ppal))

  let True = Const(BoolConstant(true))
  let False = Const(BoolConstant(false))

