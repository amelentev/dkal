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

namespace Microsoft.Research.Dkal.Ast.Substrate.Basic.Syntax.Typed

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Substrate
open Microsoft.Research.Dkal.Substrate.Basic
open Microsoft.Research.Dkal.Utils.PrettyPrinting

open System.Collections.Generic

/// The TypedBasicPrettyPrinter prints substrate elements into the typed concrete syntax,
/// which carries type annotations in every function application and every 
/// variable
type TypedBasicPrettyPrinter() =

  interface ISubstratePrettyPrinter with
    member tpp.PrintTerm t =
      match t with
      | :? BasicSubstrateTerm as t ->
        PrettyPrinter.PrettyPrint <| 
          [ ManyTokens <| tpp.TokenizeTerm t.Left;
            TextToken <| " := ";
            ManyTokens <| tpp.TokenizeTerm t.Right ]
      | _ -> failwithf "Unexpected term when printing typed basic substrate syntax: %O" t

  member private tpp.PrintTerm mt = (tpp :> ISubstratePrettyPrinter).PrintTerm mt
  member private tpp.PrintType (t: IType) = t.FullName

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
    | _ -> failwith <| sprintf "PrettyPrinter does not know how to print ITerm %O" mt
   


