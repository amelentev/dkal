namespace Microsoft.Research.Dkal.TypedSyntax

  open Microsoft.Research.Dkal.Interfaces
  open Microsoft.Research.Dkal.Ast
  open Microsoft.Research.Dkal.Utils.PrettyPrinting

  type TypedPrettyPrinter() =
    interface IPrettyPrinter with
      member tpp.PrintType t =
        match t with
        | Substrate(t) when t = typeof<int> -> "int"
        | Substrate(t) when t = typeof<float> -> "float"
        | Substrate(t) when t = typeof<string> -> "string"
        | t -> t.ToString().ToLower()

      member tpp.PrintMetaTerm mt =
        PrettyPrinter.PrettyPrint <| tpp.TokenizeMetaTerm mt

      member tpp.PrintPolicy p =
        PrettyPrinter.PrettyPrint <| tpp.TokenizePolicy p

      member tpp.PrintSignature s =
        PrettyPrinter.PrettyPrint <| []

      member tpp.PrintAssembly a =
        PrettyPrinter.PrettyPrint <| tpp.TokenizePolicy a.Policy

    member private tpp.PrintType t = (tpp :> IPrettyPrinter).PrintType t
    member private tpp.PrintMetaTerm mt = (tpp :> IPrettyPrinter).PrintMetaTerm mt

    member private tpp.TokenizeMetaTerm mt =
      match mt with
      | App(f, mts) -> 
        let args = List.map tpp.TokenizeMetaTerm mts
        let typedF = f.Name + ":" 
                      + String.concat "*" (List.map tpp.PrintType f.ArgsTyp) 
                      + "->" + tpp.PrintType f.RetTyp
        [ TextToken <| typedF + "(" ]
        @ (if args.IsEmpty then [] else List.reduce (fun t1 t2 -> t1 @ [TextToken ", "] @ t2) args)
        @ [ TextToken ")"]
      | Var(v) -> [TextToken <| v.Name + ":" + tpp.PrintType v.Typ]
      | Const(c) -> 
        match c with
        | BoolConstant(b) -> [TextToken(b.ToString().ToLower())]
        | PrincipalConstant(p) -> [TextToken(p.ToString())]
        | SubstrateConstant(o) when o.GetType() = typeof<string> -> [TextToken("\"" + o.ToString() + "\"")]
        | SubstrateConstant(o) -> [TextToken(o.ToString())]
   
    member private tpp.TokenizePolicy (p: Policy) =
      List.collect (fun a -> tpp.TokenizeMetaTerm a @ [ NewLineToken; NewLineToken ]) p.Rules


