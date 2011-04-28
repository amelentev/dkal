namespace Microsoft.Research.Dkal.Ast.Substrate.Sql.Syntax.Simple

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Globals
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Utils.PrettyPrinting
open Microsoft.Research.Dkal.Substrate
open Microsoft.Research.Dkal.Substrate.Factories
open Microsoft.Research.Dkal.Substrate.Sql

open System.Collections.Generic

/// The SimpleSqlPrettyPrinter prints substrate elements into the simple concrete syntax
type SimpleSqlPrettyPrinter() =
    
  interface ISubstratePrettyPrinter with
    member spp.PrintTerm t =
      match t with
      | :? DummySubstrateTerm as t ->
        PrettyPrinter.PrettyPrint <| spp.TokenizeTerm t.Query
      | _ -> failwith "Expecting DummySubstrateTerm when printing SimpleSqlSyntax"

    member spp.PrintSubstrate s =
      match s with
      | :? SqlSubstrate as s ->
        "substrate sql(\"" + s.ConnectionString + "\", \"" + s.SchemaFile + "\") namespaces " +
          (String.concat ", " (Seq.map (fun ns -> "\"" + ns + "\"") (s :> ISubstrate).Namespaces))
      | _ -> failwith "Expecting SqlSubstrate when printing substrate"


  member private spp.PrintTerm mt = (spp :> ISubstratePrettyPrinter).PrintTerm mt

  static member FindFunctionSymbol f = 
    match f with
    | "and" -> "&&", true
    | "or" -> "||", true
    | "not" -> "!", false
    | "eq" -> "==", true
    | "neq" -> "!=", true
    | "lt" -> "<", true
    | "lte" -> "<=", true
    | "gt" -> ">", true
    | "gte" -> ">=", true
    | "plus" -> "+", true
    | "minus" -> "-", true
    | "times" -> "*", true
    | "div" -> "/", true
    | "uminus" -> "-", false
    | f -> f, false

  member private spp.TokenizeTerm mt =
    match mt with
    | App(f, []) -> [ TextToken f.Name ]
    | App(f, mts) -> 
      let fSymbol, infix = SimpleSqlPrettyPrinter.FindFunctionSymbol f.Name
      let args = List.map spp.TokenizeTerm mts
      if infix then
        [ TextToken "(" ]
        @ List.reduce (fun t1 t2 -> t1 @ [TextToken <| " " + fSymbol + " "] @ t2) args
        @ [ TextToken ")" ]
      else
        [ TextToken <| fSymbol + "(" ]
        @ List.reduce (fun t1 t2 -> t1 @ [TextToken ", "] @ t2) args
        @ [ TextToken ")"]
    | Var(v) -> [TextToken v.Name]
    | True -> [TextToken "true"]
    | False -> [TextToken "false"]
    | Principal(p) -> [TextToken(p)]
    | SubstrateConstant(o) when o.GetType() = typeof<string> -> [TextToken("\"" + o.ToString() + "\"")]
    | SubstrateConstant(o) -> [TextToken(o.ToString())]
    | :? ISubstrateTerm as t ->
      let substrate = SubstrateMap.GetSubstrate t.Namespace
      let pp = SubstratePrettyPrinterFactory.SubstratePrettyPrinter substrate "simple"
      let printedSubstrateTerm = pp.PrintTerm t
      [ TextToken <| "{| \"" + t.Namespace + "\" | " + printedSubstrateTerm + " |}" ]      
    | _ -> failwith <| sprintf "PrettyPrinter does not know how to print ITerm %A" mt
   
