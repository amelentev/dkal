namespace Microsoft.Research.Dkal.Ast.Substrate.Basic.Syntax.Simple

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Utils.PrettyPrinting
open Microsoft.Research.Dkal.Substrate.Basic

open System.Collections.Generic

/// The SimpleBasicPrettyPrinter prints substrate elements into the simple concrete syntax
type SimpleBasicPrettyPrinter() =
    
  interface ISubstratePrettyPrinter with
    member spp.PrintTerm t =
      match t with
      | :? BasicSubstrateTerm as t ->
        PrettyPrinter.PrettyPrint <| 
          [ ManyTokens <| spp.TokenizeTerm t.Left;
            TextToken <| " := ";
            ManyTokens <| spp.TokenizeTerm t.Right ]
      | _ -> failwithf "Unexpected term when printing simple basic substrate syntax: %O" t

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

  member private spp.TokenizeTerm t =
    match t with
    | App(f, []) -> [ TextToken f.Name ]
    | App(f, mts) -> 
      let fSymbol, infix = SimpleBasicPrettyPrinter.FindFunctionSymbol f.Name
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
    | PrincipalConstant(p) -> [TextToken(p)]
    | SubstrateConstant(o) when o.GetType() = typeof<string> -> [TextToken("\"" + o.ToString() + "\"")]
    | SubstrateConstant(o) -> [TextToken(o.ToString())]
    | _ -> failwith <| sprintf "PrettyPrinter does not know how to print ITerm %O" t
   
