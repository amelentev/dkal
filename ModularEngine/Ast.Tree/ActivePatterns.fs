[<AutoOpen>]
module Microsoft.Research.Dkal.Ast.Tree.ActivePatterns

  open Microsoft.Research.Dkal.Interfaces
  
  let (|App|_|) (t: ITerm) =  match t with
                              | :? Application as a -> Some (a.Function, a.Args)
                              | _ -> None

  let (|Var|_|) (t: ITerm) =  match t with
                              | :? Variable as v -> Some v
                              | _ -> None

  let (|Const|_|) (t: ITerm) =  match t with
                                | :? Constant as c -> Some c
                                | _ -> None

