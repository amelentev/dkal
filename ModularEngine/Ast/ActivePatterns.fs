[<AutoOpen>]
module Microsoft.Research.Dkal.Ast.ActivePatterns

  open Microsoft.Research.Dkal.Interfaces

  // Type patterns
  let (|Infon|_|) t = if t = Type.Infon then Some () else None
  let (|Principal|_|) t = if t = Type.Principal then Some () else None
  let (|SubstrateUpdate|_|) t = if t = Type.SubstrateUpdate then Some () else None
  let (|SubstrateQuery|_|) t = if t = Type.SubstrateQuery then Some () else None
  let (|Action|_|) t = if t = Type.Action then Some () else None
  let (|Condition|_|) t = if t = Type.Condition then Some () else None
  let (|Rule|_|) t = if t = Type.Rule then Some () else None
  let (|Substrate|_|) (t: IType) =  match t with
                                    | :? Type.Substrate as t -> Some t.Type
                                    | _ -> None

  // Variables
  let (|Var|_|) (t: ITerm) =  match t with
                              | :? Variable as v -> Some v
                              | _ -> None

  // Constants                            
  let (|Const|_|) (t: ITerm) =  match t with
                                | :? IConst as c -> Some c
                                | _ -> None

  // Literal patterns
  let (|SubstrateConstant|_|) mt =  match mt with
                                    | Const(c) ->
                                      match c with
                                      | :? Constant as c -> 
                                        match (c :> ITerm).Type with 
                                        | Substrate(_) -> Some c.Value
                                        | _ -> None
                                      | _ -> None
                                    | _ -> None
  let (|PrincipalConstant|_|) mt =  match mt with
                                    | Const(c) -> 
                                      match c with
                                      | :? PrincipalConstant as p -> Some p.Name
                                      | _ -> None
                                    | _ -> None
  let (|True|_|) mt = match mt with
                      | Const(c) -> 
                        match c with
                        | :? Constant as bc -> 
                          match bc.Value with
                          | :? bool as b when b = true -> Some ()
                          | _ -> None
                        | _ -> None
                      | _ -> None
  let (|False|_|) mt =  match mt with
                        | Const(c) -> 
                          match c with
                          | :? Constant as bc ->
                            match bc.Value with
                            | :? bool as b when b = false -> Some ()
                            | _ -> None
                          | _ -> None
                        | _ -> None