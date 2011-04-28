namespace Microsoft.Research.Dkal.Ast.Infon.Syntax.Typed

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Globals
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Substrate
open Microsoft.Research.Dkal.Substrate.Factories
open Microsoft.Research.Dkal.Utils.PrettyPrinting

/// The TypedPrettyPrinter prints AST elements into the typed concrete syntax,
/// which carries type annotations in every function application and every 
/// variable
type TypedPrettyPrinter() =

  interface IInfonPrettyPrinter with
    member tpp.PrintType (t: IType) = t.FullName

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
      let identityTokens =  match f.Identity with
                            | None -> []
                            | Some t -> [TextToken ":"] @ tpp.TokenizeTerm t
      [ TextToken <| typedF + "(" ]
        @ (if args.IsEmpty then [] else List.reduce (fun t1 t2 -> t1 @ [TextToken ", "] @ t2) args)
        @ [ TextToken ")"]
        @ identityTokens
    | Var(v) -> [TextToken <| v.Name + ":" + tpp.PrintType v.Type]
    | True -> [TextToken "true"]
    | False -> [TextToken "false"]
    | Principal(p) -> [TextToken(p)]
    | SubstrateConstant(o) when o.GetType() = typeof<string> -> [TextToken("\"" + o.ToString() + "\"")]
    | SubstrateConstant(o) -> [TextToken(o.ToString())]
    | :? ISubstrateTerm as t -> 
      let substrate = SubstrateMap.GetSubstrate t.Namespace
      let pp = SubstratePrettyPrinterFactory.SubstratePrettyPrinter substrate "typed"
      let printedSubstrateTerm = pp.PrintTerm t
      [ TextToken <| "{|\"" + t.Namespace + "\"|" + printedSubstrateTerm + "|}" ]
    | _ -> failwith <| sprintf "PrettyPrinter does not know how to print ITerm %A" mt
   
  member private tpp.TokenizePolicy (p: Policy) =
    List.collect (fun a -> tpp.TokenizeTerm a @ [ NewLineToken; NewLineToken ]) p.Rules


