[<AutoOpen>]
module Microsoft.Research.Dkal.Ast.Tree.Builders

  open Microsoft.Research.Dkal.Interfaces
  
  let App (f: Function, args: ITerm list) =
    if f.ArgsType.Length = args.Length && List.forall2 (fun (t: IType) (a: ITerm) -> t = a.Type) f.ArgsType args then
      { Function = f; Args = args } :> ITerm
    else
      failwithf "Incorrect parameter types when building %O: %O" f.Name args


