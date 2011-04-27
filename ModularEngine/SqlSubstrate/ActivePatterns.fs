[<AutoOpen>]
module Microsoft.Research.Dkal.SqlSubstrate.ActivePatterns

  open Microsoft.Research.Dkal.Interfaces
  open Microsoft.Research.Dkal.Ast.Tree
  open Microsoft.Research.Dkal.Ast

  // Bool patterns
  let (|AndBool|_|) mt =  match mt with
                          | App({Name="and"; RetType=Substrate(b)}, mts) when b = typeof<bool> -> Some mts
                          | _ -> None
  let (|OrBool|_|) mt = match mt with
                        | App({Name="or"; RetType=Substrate(b)}, mts) when b = typeof<bool> -> Some mts
                        | _ -> None
