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

namespace Microsoft.Research.Dkal.Ast.Infon.Syntax.Simple

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Globals
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Utils.PrettyPrinting
open Microsoft.Research.Dkal.Substrate.Factories

open System.Collections.Generic

/// The SimplePrettyPrinter prints AST elements into the simple concrete syntax,
/// which uses declared typed variables
type SimplePrettyPrinter() =
  interface IInfonPrettyPrinter with
    member spp.PrintType (t: IType) = t.FullName

    member spp.PrintTerm t =
      PrettyPrinter.PrettyPrint <| spp.TokenizeTerm t

    member spp.PrintPolicy p =
      PrettyPrinter.PrettyPrint <| spp.TokenizePolicy p

    member spp.PrintSignature s =
      PrettyPrinter.PrettyPrint <| spp.TokenizeSignature s

    member spp.PrintAssembly a =
      PrettyPrinter.PrettyPrint <| spp.TokenizeAssembly a

  member private spp.PrintType t = (spp :> IPrettyPrinter).PrintType t
  member private spp.PrintTerm mt = (spp :> IPrettyPrinter).PrintTerm mt

  static member private FindFunctionSymbol f = 
    match f with
    | "implies" -> "->", true, 1, true
    | "and" -> "&&", true, 2, true
    | "said" -> "said", true, 3, false
    | f -> f, false, 4, false

  member private spp.TokenizeTerm (mt: ITerm, ?withVars: bool, ?precedence: int) =
    let withVars =  match withVars with
                    | None -> true
                    | Some b -> b
    let precedence =  match precedence with
                      | None -> -1
                      | Some precedence -> precedence
    match mt with
    | App(f, mts) -> 
      let fSymbol, infix, prec, newLine = SimplePrettyPrinter.FindFunctionSymbol f.Name
      let args = List.map (fun mt -> spp.TokenizeTerm(mt, withVars, prec)) mts
      let beginNewLineTokens, endNewLineTokens = 
        if newLine && prec < precedence then 
          [UntabToken; NewLineToken], [TabToken; NewLineToken] 
        elif newLine then
          [TextToken " "], [NewLineToken]
        else 
          [TextToken " "], [TextToken " "]
      let leftBracket, rightBracket = 
        if prec < precedence && newLine then
          [TextToken "("; TabToken; NewLineToken], [UntabToken; NewLineToken; TextToken ")"; NewLineToken]
        elif prec < precedence then
          [TextToken "("], [TextToken ")"]
        else
          [], []
      if infix then
        leftBracket
        @ List.reduce (fun t1 t2 -> t1 @ beginNewLineTokens @ [TextToken <| fSymbol] @ endNewLineTokens @ t2) args
        @ rightBracket
      elif fSymbol = Primitives.AsInfon then
        match mts with
        | [exp] -> 
          [ TextToken <| f.Name + "(";
            ManyTokens args.[0] ]
            @ [ TextToken <| ")" ]
        | _ -> failwith "Incorrect arguments in AsInfon(...)"
      elif fSymbol = "emptyInfon" then
        [ TextToken <| "asInfon(true)" ]
      elif fSymbol = Primitives.Rule then
        let beginVars, endVars =  
          if withVars then 
            spp.TokenizeVariableDeclaration (mt.Vars |> Seq.toList)
          else
            [], []
        let mainTokens = 
          [ ManyTokens <| spp.TokenizeTerm(mts.[0], false)]
          @ [ TextToken <| "do" ]
          @ [ TabToken; NewLineToken ]
          @ [ ManyTokens <| spp.TokenizeTerm(mts.[1], false)]
          @ [ UntabToken; NewLineToken ]
        beginVars @ mainTokens @ endVars
      elif fSymbol = Primitives.SeqRule then
        let beginVars, endVars =  
          if withVars then 
            spp.TokenizeVariableDeclaration (mt.Vars |> Seq.toList)
          else
            [], []
        beginVars
        @ List.map (fun mt -> ManyTokens <| spp.TokenizeTerm(mt, false)) mts
        @ endVars
      elif fSymbol = Primitives.WireCondition then
        [ TextToken <| "upon";
          TabToken; NewLineToken;
          ManyTokens <| spp.TokenizeTerm mts.[0];
          TextToken <| " from ";
          ManyTokens <| spp.TokenizeTerm mts.[1];
          UntabToken; NewLineToken ]
      elif fSymbol = Primitives.EmptyAction || fSymbol = Primitives.EmptyCondition then
        []
      elif fSymbol = Primitives.KnownCondition then
        [ TextToken <| "if";
          TabToken; NewLineToken;
          ManyTokens <| spp.TokenizeTerm mts.[0];
          UntabToken; NewLineToken ]
      elif fSymbol = Primitives.JustifiedSend then
        [ TextToken <| "send with justification to " + spp.PrintTerm mts.[0] + ":";
          TabToken; NewLineToken;
          ManyTokens <| spp.TokenizeTerm mts.[1];
          UntabToken; NewLineToken ]
      elif fSymbol = Primitives.JustifiedSay then
        [ TextToken <| "say with justification to " + spp.PrintTerm mts.[0] + ":";
          TabToken; NewLineToken;
          ManyTokens <| spp.TokenizeTerm mts.[1];
          UntabToken; NewLineToken ]
      elif fSymbol = Primitives.Send then
        [ TextToken <| "send to " + spp.PrintTerm mts.[0] + ":";
          TabToken; NewLineToken;
          ManyTokens <| spp.TokenizeTerm mts.[1];
          UntabToken; NewLineToken ]
      elif fSymbol = Primitives.Learn then
        [ TextToken <| "learn";
          TabToken; NewLineToken;
          ManyTokens <| spp.TokenizeTerm mts.[0];
          UntabToken; NewLineToken ]
      elif fSymbol = Primitives.Apply then
        [ TextToken <| "apply";
          TabToken; NewLineToken;
          ManyTokens <| spp.TokenizeTerm mts.[0];
          UntabToken; NewLineToken ]
      elif fSymbol = Primitives.SeqAction || fSymbol = Primitives.SeqCondition then
        List.concat (List.map spp.TokenizeTerm mts)
      elif fSymbol = Primitives.Justified then
        [ ManyTokens <| spp.TokenizeTerm mts.[0];
          TextToken <| " [ "; 
          ManyTokens <| spp.TokenizeTerm mts.[1]; 
          TextToken <| " ]"]
      elif fSymbol = Primitives.EvSignature then
        [ TextToken <| "signed by ";
          ManyTokens <| spp.TokenizeTerm mts.[0];
          TextToken <| " ";
          ManyTokens <| spp.TokenizeTerm mts.[2] ]
      elif not infix && mts = [] then
        [ TextToken <| fSymbol ]
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
    | :? ISubstrateTerm as t -> 
      let substrate = SubstrateMap.GetSubstrate t.Namespace
      let pp = SubstratePrettyPrinterFactory.SubstratePrettyPrinter substrate "simple"
      let printedSubstrateTerm = pp.PrintTerm t
      [ TextToken <| "{| \"" + t.Namespace + "\" | " + printedSubstrateTerm + " |}" ]
    | :? ExplicitSubstitutionTerm as t ->
      if t.Substitution.Domain.IsEmpty then 
        [ ManyTokens <| spp.TokenizeTerm t.Term ]
      else
        [ TextToken <| "(";
          ManyTokens <| spp.TokenizeTerm t.Term;
          TextToken <| " " + t.Substitution.ToString() + ")" ]
    | :? ForallTerm as ft ->
      let keyword = if ft.Term.Type=Type.Rule then "with " else "forall "
      [ TextToken <| keyword + (String.concat ", " [for v in (ft :> ITerm).BoundVars -> v.Name + ": " + v.Type.FullName]);
        TabToken; NewLineToken;
        ManyTokens <| spp.TokenizeTerm ft.InnerTerm;
        UntabToken; NewLineToken ]
    | _ -> failwith <| sprintf "PrettyPrinter does not know how to print ITerm %O" mt
   
  member private spp.TokenizeVariableDeclaration (vars: IVar list) =
    let varsDecl = List.map (fun (v: IVar) -> v.Name + ": " + spp.PrintType v.Type) vars
    if varsDecl.Length > 0 then
      [ TextToken <| "with " + (String.concat ", " varsDecl);
        TabToken;
        NewLineToken ], [UntabToken; NewLineToken]
    else
      [], []

  member private spp.TokenizePolicy (p: Policy) =
    List.collect (fun a -> spp.TokenizeTerm a) p.Rules

  member private spp.TokenizeSignature (s: Signature) =
    List.collect (fun rd -> spp.TokenizeRelationDeclaration rd @ [ NewLineToken; NewLineToken ]) s.Relations

  member private spp.TokenizeRelationDeclaration (rd: RelationDeclaration) =
    [TextToken <| "relation " + rd.Name + "(";
      TextToken <| String.concat ", " (List.map (fun (v: IVar) -> v.Name + ": " + spp.PrintType v.Type) rd.Args);
      TextToken ")" ]

  member private spp.TokenizeAssembly (a: Assembly) =
    spp.TokenizeSignature a.Signature
      @ spp.TokenizePolicy a.Policy
