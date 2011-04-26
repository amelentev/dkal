[<AutoOpen>]
module Microsoft.Research.Dkal.Ast.ActivePatterns

  open Microsoft.Research.Dkal.Interfaces

  // Type patterns
  let (|Infon|_|) t = if t = Type.Infon then Some () else None
  let (|Principal|_|) t = if t = Type.Principal then Some () else None
  let (|Action|_|) t = if t = Type.Action then Some () else None
  let (|Rule|_|) t = if t = Type.Rule then Some () else None
  let (|Substrate|_|) (t: IType) =  match t with
                                    | :? Type.Substrate as t -> Some t.Type
                                    | _ -> None
