[<AutoOpen>]
module Microsoft.Research.Dkal.Ast.Tree.ActivePatterns

  open Microsoft.Research.Dkal.Interfaces
  
  let (|App|_|) (t: ITerm) =  match t with
                              | :? Application as a -> Some (a.Function, a.Args)
                              | _ -> None
