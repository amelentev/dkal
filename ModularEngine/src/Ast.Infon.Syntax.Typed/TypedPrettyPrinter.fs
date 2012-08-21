// *********************************************************
//
//    Copyright (c) Microsoft. All rights reserved.
//    This code is licensed under the Apache License, Version 2.0.
//    THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
//    ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
//    IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
//    PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.
//
// *********************************************************

namespace Microsoft.Research.Dkal.Ast.Infon.Syntax.Typed

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Globals
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Ast.Infon
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
    | PrincipalConstant(p) -> [TextToken(p)]
    | SubstrateConstant(o) when o.GetType() = typeof<string> -> [TextToken("\"" + o.ToString() + "\"")]
    | SubstrateConstant(o) -> [TextToken(o.ToString())]
    | :? ISubstrateTerm as t -> 
      let substrate = SubstrateMap.GetSubstrate t.Namespace
      let pp = SubstratePrettyPrinterFactory.SubstratePrettyPrinter substrate "typed"
      let printedSubstrateTerm = pp.PrintTerm t
      [ TextToken <| "{|\"" + t.Namespace + "\"|" + printedSubstrateTerm + "|}" ]
    | :? ExplicitSubstitutionTerm as t ->
      [ TextToken <| "(";
        ManyTokens <| tpp.TokenizeTerm t.Term;
        TextToken <| " {";
        TextToken <| String.concat ", " [for v in t.Substitution.Domain -> "(" + tpp.PrintTerm(v) + ") -> " + tpp.PrintTerm(t.Substitution.Apply(v))];
        TextToken <| "})" ]
    | :? ForallTerm as ft ->
      [ TextToken <| "forall " + (String.concat ", " [for v in (ft :> ITerm).BoundVars -> v.Name + ": " + v.Type.FullName]) + " (";
        ManyTokens <| tpp.TokenizeTerm ft.InnerTerm;
        TextToken <| ")" ]
    | _ -> failwith <| sprintf "PrettyPrinter does not know how to print ITerm %O" mt
   
  member private tpp.TokenizePolicy (p: Policy) =
    List.collect (fun a -> tpp.TokenizeTerm a @ [ NewLineToken; NewLineToken ]) p.Rules


