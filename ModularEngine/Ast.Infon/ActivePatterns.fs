[<AutoOpen>]
module Microsoft.Research.Dkal.Ast.Infon.ActivePatterns

  open Microsoft.Research.Dkal.Interfaces
  open Microsoft.Research.Dkal.Ast.Tree
  open Microsoft.Research.Dkal.Ast

  // Rule patterns
  let (|Rule|_|) mt = match mt with
                      | App({Name="rule"}, [cs; cw; a]) -> Some (cs, cw, a)
                      | _ -> None

  // Action patterns
  let (|Seq|_|) mt =  match mt with
                      | App({Name="seq"}, [a1; a2]) -> Some (a1, a2)
                      | _ -> None
  let (|Send|_|) mt = match mt with
                      | App({Name="send"}, [ppal; i]) -> Some (ppal, i)
                      | _ -> None
  let (|Learn|_|) mt =  match mt with
                        | App({Name="learn"}, [i]) -> Some i
                        | _ -> None
  let (|Forget|_|) mt = match mt with
                        | App({Name="forget"}, [i]) -> Some i
                        | _ -> None
  let (|Install|_|) mt =  match mt with
                          | App({Name="install"}, [r]) -> Some r
                          | _ -> None
  let (|Uninstall|_|) mt =  match mt with
                            | App({Name="uninstall"}, [r]) -> Some r
                            | _ -> None

  // Infon patterns
  let (|EmptyInfon|_|) mt = match mt with 
                            | App({Name="emptyInfon"}, []) -> Some ()
                            | _ -> None
  let (|AsInfon|_|) mt =  match mt with 
                          | App({Name="asInfon"}, [exp]) -> 
                            match exp with
                            | :? ISubstrateTerm as exp -> Some exp
                            | _ -> None
                          | _ -> None
  let (|AndInfon|_|) mt = match mt with
                          | App({Name="and"; RetType=Infon}, mts) -> Some mts
                          | _ -> None
  let (|ImpliesInfon|_|) mt = match mt with
                              | App({Name="implies"; RetType=Infon}, [mt1; mt2]) -> Some (mt1, mt2)
                              | _ -> None
  let (|SaidInfon|_|) mt = match mt with
                            | App({Name="said"}, [ppal; mt']) -> Some (ppal, mt')
                            | _ -> None

  // Literal patterns
  let (|SubstrateConstant|_|) mt =  match mt with
                                    | Const(c) ->
                                      match c with
                                      | :? SubstrateConstant as sc -> Some sc.Elem
                                      | _ -> None
                                    | _ -> None
  let (|Principal|_|) mt =  match mt with
                            | Const(c) -> 
                              match c with
                              | :? PrincipalConstant as p -> Some p.Name
                              | _ -> None
                            | _ -> None
  let (|True|_|) mt = match mt with
                      | Const(c) -> 
                        match c with
                        | :? SubstrateConstant as sc when sc.Elem = (true :> obj) -> Some ()
                        | _ -> None
                      | _ -> None
  let (|False|_|) mt =  match mt with
                        | Const(c) -> 
                          match c with
                          | :? SubstrateConstant as sc when sc.Elem = (false :> obj) -> Some ()
                          | _ -> None
                        | _ -> None