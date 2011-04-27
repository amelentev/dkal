namespace Microsoft.Research.Dkal.Substrate.TypedSqlSyntax

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Substrate
open Microsoft.Research.Dkal.Utils.PrettyPrinting

/// The TypedSqlPrettyPrinter prints substrate elements into the typed concrete syntax,
/// which carries type annotations in every function application and every 
/// variable
type TypedSqlPrettyPrinter() =

  interface ISubstratePrettyPrinter with
    member tpp.PrintTerm t =
      match t with
      | :? DummySubstrateTerm as t -> 
        PrettyPrinter.PrettyPrint <| tpp.TokenizeTerm t.Query
      | _ -> failwith "Expecting DummySubstrateTerm when printing TypedSqlSyntax"

    member tpp.PrintSubstrate s = ""
      
  member private tpp.PrintTerm mt = (tpp :> ISubstratePrettyPrinter).PrintTerm mt
  member private tpp.PrintType (t: IType) = t.FullName

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
    | True -> [TextToken "true"]
    | False -> [TextToken "false"]
    | Principal(p) -> [TextToken(p)]
    | SubstrateConstant(o) when o.GetType() = typeof<string> -> [TextToken("\"" + o.ToString() + "\"")]
    | SubstrateConstant(o) -> [TextToken(o.ToString())]
    | _ -> failwith <| sprintf "PrettyPrinter does not know how to print ITerm %A" mt
   


