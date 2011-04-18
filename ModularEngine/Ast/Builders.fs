[<AutoOpen>]
module Microsoft.Research.Dkal.Ast.Builders

  open Microsoft.Research.Dkal.Ast

  // Rule builders
  let RuleRule (cs: MetaTerm, cw: MetaTerm, a: MetaTerm) = 
    if cs.Typ() = Infon && cw.Typ() = Infon && a.Typ() = Action then
      App(primitives.["rule"], [cs; cw; a])
    else
      failwith "Incorrect parameter type when building rule"

  // Action builders
  let SeqAction (a1: MetaTerm, a2: MetaTerm) = 
    if a1.Typ() = Action && a2.Typ() = Action then
      App(primitives.["seq"], [a1; a2])
    else
      failwith "Incorrect parameter type when building seq"

  let SendAction (ppal: MetaTerm, i: MetaTerm) = 
    if ppal.Typ() = Principal && i.Typ() = Infon then
      App(primitives.["send"], [ppal; i])
    else
      failwith "Incorrect parameter type when building send"
    
  let LearnAction (i: MetaTerm) = 
    if i.Typ() = Infon then
      App(primitives.["learn"], [i])
    else
      failwith "Incorrect parameter type when building learn"
    
  // Substrate builders
  let Sql (cs: MetaTerm) = 
    if cs.Typ() = Type.String then
      App(primitives.["sql"], [cs])
    else
      failwith "Incorrect parameter type when building sql"

  let Xml (file: MetaTerm) = 
    if file.Typ() = Type.String then
      App(primitives.["xml"], [file])
    else
      failwith "Incorrect parameter type when building xml"

  // Infon builders
  let EmptyInfon = 
    App(primitives.["emptyInfon"], [])
    
  let AsInfon (query: MetaTerm, substrate: MetaTerm) = 
    if query.Typ() = Bool && substrate.Typ() = Substrate then
      App(primitives.["asInfon"], [query; substrate])
    else
      failwith "Incorrect parameter type when building asInfon"
    
  let AndInfon (infons: MetaTerm list) = 
    if List.forall (fun (i: MetaTerm) -> i.Typ() = Infon) infons then
      App({ Name = "andInfon"; 
            RetTyp = Bool; 
            ArgsTyp = List.replicate infons.Length Bool }, infons)
    else
      failwith "Incorrect parameter type when building andInfon"
    
  let ImpliesInfon (i1: MetaTerm, i2: MetaTerm) = 
    if i1.Typ() = Infon && i2.Typ() = Infon then
      App(primitives.["impliesInfon"], [i1; i2])
    else
      failwith "Incorrect parameter type when building impliesInfon"

  let SaidInfon (ppal: MetaTerm, i: MetaTerm) = 
    if ppal.Typ() = Principal && i.Typ() = Infon then
      App(primitives.["saidInfon"], [ppal; i])
    else
      failwith "Incorrect parameter type when building saidInfon"

  // Bool builders
  let AndBool (bools: MetaTerm list) = 
    if List.forall (fun (b: MetaTerm) -> b.Typ() = Bool) bools then
      App({ Name = "andBool"; 
            RetTyp = Bool; 
            ArgsTyp = List.replicate bools.Length Bool }, bools)
    else
      failwith "Incorrect parameter type when building andBool"

  let OrBool (bools: MetaTerm list) = 
    if List.forall (fun (b: MetaTerm) -> b.Typ() = Bool) bools then
      App({ Name = "orBool"; 
            RetTyp = Bool; 
            ArgsTyp = List.replicate bools.Length Bool }, bools)
    else
      failwith "Incorrect parameter type when building orBool"

  // Literal builders
  let Principal (ppal: string) = 
    Const(PrincipalConstant(ppal))

  let True = Const(BoolConstant(true))
  let False = Const(BoolConstant(false))

