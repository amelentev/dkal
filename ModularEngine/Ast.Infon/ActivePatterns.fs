[<AutoOpen>]
module Microsoft.Research.Dkal.Ast.Infon.ActivePatterns

  open Microsoft.Research.Dkal.Interfaces
  open Microsoft.Research.Dkal.Ast.Tree
  open Microsoft.Research.Dkal.Ast

  // Rule patterns
  let (|SeqRule|_|) mt =  match mt with
                          | App({Name=Primitives.SeqRule}, rs) -> Some rs
                          | _ -> None
  let (|EmptyRule|_|) mt =  match mt with
                            | App({Name=Primitives.EmptyRule}, []) -> Some ()
                            | _ -> None
  let (|Rule|_|) mt = match mt with
                      | App({Name=Primitives.Rule}, [c; a]) -> Some (c, a)
                      | _ -> None

  // Condition patterns
  let (|SeqCondition|_|) mt = match mt with
                              | App({Name=Primitives.SeqCondition}, cs) -> Some cs
                              | _ -> None
  let (|EmptyCondition|_|) mt = match mt with
                                | App({Name=Primitives.EmptyCondition}, []) -> Some ()
                                | _ -> None
  let (|WireCondition|_|) mt =  match mt with
                                | App({Name=Primitives.WireCondition}, [i]) -> Some i
                                | _ -> None
  let (|KnownCondition|_|) mt = match mt with
                                | App({Name=Primitives.KnownCondition}, [i]) -> Some i
                                | _ -> None

  // Action patterns
  let (|SeqAction|_|) mt =  match mt with
                            | App({Name=Primitives.SeqAction}, actions) -> Some actions
                            | _ -> None
  let (|EmptyAction|_|) mt =  match mt with
                              | App({Name=Primitives.EmptyAction}, []) -> Some ()
                              | _ -> None
  let (|Send|_|) mt = match mt with
                      | App({Name=Primitives.Send}, [ppal; i]) -> Some (ppal, i)
                      | _ -> None
  let (|Say|_|) mt =  match mt with
                      | App({Name=Primitives.Say}, [ppal; i]) -> Some (ppal, i)
                      | _ -> None
  let (|Learn|_|) mt =  match mt with
                        | App({Name=Primitives.Learn}, [i]) -> Some i
                        | _ -> None
  let (|Forget|_|) mt = match mt with
                        | App({Name=Primitives.Forget}, [i]) -> Some i
                        | _ -> None
  let (|Install|_|) mt =  match mt with
                          | App({Name=Primitives.Install}, [r]) -> Some r
                          | _ -> None
  let (|Uninstall|_|) mt =  match mt with
                            | App({Name=Primitives.Uninstall}, [r]) -> Some r
                            | _ -> None
  let (|Apply|_|) mt =  match mt with
                        | App({Name=Primitives.Apply}, [t]) -> 
                          match t with
                            | :? ISubstrateUpdateTerm as su -> Some su
                            | _ -> failwith "Expecting ISubstrateUpdateTerm in AsInfon"
                        | _ -> None
  let (|Drop|_|) mt = match mt with
                      | App({Name=Primitives.Drop}, [i]) -> Some i
                      | _ -> None

  // Infon patterns
  let (|EmptyInfon|_|) mt = match mt with 
                            | App({Name=Primitives.EmptyInfon}, []) -> Some ()
                            | _ -> None
  let (|AsInfon|_|) mt =  match mt with 
                          | App({Name=Primitives.AsInfon}, [exp]) -> 
                            match exp with
                            | :? ISubstrateQueryTerm as exp -> Some exp
                            | _ -> failwith "Expecting ISubstrateQueryTerm in AsInfon"
                          | _ -> None
  let (|AndInfon|_|) mt = match mt with
                          | App({Name=Primitives.And; RetType=Infon}, mts) -> Some mts
                          | _ -> None
  let (|ImpliesInfon|_|) mt = match mt with
                              | App({Name=Primitives.Implies; RetType=Infon}, [mt1; mt2]) -> Some (mt1, mt2)
                              | _ -> None
  let (|SaidInfon|_|) mt = match mt with
                            | App({Name=Primitives.Said}, [ppal; mt']) -> Some (ppal, mt')
                            | _ -> None

