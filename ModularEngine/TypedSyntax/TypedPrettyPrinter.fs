namespace Microsoft.Research.Dkal.Ast.TypedSyntax

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Utils.PrettyPrinting

/// The TypedPrettyPrinter prints AST elements into the typed concrete syntax,
/// which carries type annotations in every function application and every 
/// variable
type TypedPrettyPrinter() =
  interface IAstPrettyPrinter with
    member tpp.PrintType (t: IType) =
      match t with 
      | :? Type as t ->
        match t with
        | SubstrateElem(t) when t = typeof<int> -> "int"
        | SubstrateElem(t) when t = typeof<float> -> "float"
        | SubstrateElem(t) when t = typeof<string> -> "string"
        | t -> t.ToString().ToLower()
      | _ -> failwith <| "Unknown type implementation when printing"

    member tpp.PrintTerm mt =
      PrettyPrinter.PrettyPrint <| tpp.TokenizeTerm mt

    member tpp.PrintPolicy p =
      PrettyPrinter.PrettyPrint <| tpp.TokenizePolicy p

    member tpp.PrintSignature s =
      PrettyPrinter.PrettyPrint <| []

    member tpp.PrintAssembly a =
      PrettyPrinter.PrettyPrint <| tpp.TokenizePolicy a.Policy

  member private tpp.PrintType t = (tpp :> IPrettyPrinter).PrintType t
  member private tpp.PrintTerm mt = (tpp :> IPrettyPrinter).PrintTerm mt

  member private tpp.TokenizeTerm mt =
    match mt with
    | App(f, mts) -> 
      let args = List.map tpp.TokenizeTerm mts
      let typedF = f.Name + ":" 
                    + String.concat "*" (List.map tpp.PrintType f.ArgsType) 
                    + "->" + tpp.PrintType f.RetType
      [ TextToken <| typedF + "(" ]
      @ (if args.IsEmpty then [] else List.reduce (fun t1 t2 -> t1 @ [TextToken ", "] @ t2) args)
      @ [ TextToken ")"]
    | Var(v) -> [TextToken <| v.Name + ":" + tpp.PrintType v.Type]
    | Const(c) -> 
      match c with
      | BoolConstant(b) -> [TextToken(b.ToString().ToLower())]
      | PrincipalConstant(p) -> [TextToken(p.ToString())]
      | SubstrateElemConstant(o) when o.GetType() = typeof<string> -> [TextToken("\"" + o.ToString() + "\"")]
      | SubstrateElemConstant(o) -> [TextToken(o.ToString())]
    | _ -> failwith <| "PrettyPrinter does not know how to print ITerm"
   
  member private tpp.TokenizePolicy (p: Policy) =
    List.collect (fun a -> tpp.TokenizeTerm a @ [ NewLineToken; NewLineToken ]) p.Rules


