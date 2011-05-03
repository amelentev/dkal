[<AutoOpen>]
module Microsoft.Research.Dkal.Substrate.Sql.ActivePatterns

  open Microsoft.Research.Dkal.Interfaces
  open Microsoft.Research.Dkal.Ast.Tree
  open Microsoft.Research.Dkal.Ast

  // Bool patterns
  let (|AndBool|_|) mt =  match mt with
                          | App({Name=SqlPrimitives.And; RetType=Substrate(b)}, mts) when b = typeof<bool> -> Some mts
                          | _ -> None
  let (|OrBool|_|) mt = match mt with
                        | App({Name=SqlPrimitives.Or; RetType=Substrate(b)}, mts) when b = typeof<bool> -> Some mts
                        | _ -> None

  // Table patterns
  let (|Column|_|) mt = match mt with
                        | App({Name=fn}, []) when fn.Contains(".") ->
                          let i = fn.IndexOf('.')
                          Some (fn.Substring(0, i), fn.Substring(i+1))
                        | _ -> None