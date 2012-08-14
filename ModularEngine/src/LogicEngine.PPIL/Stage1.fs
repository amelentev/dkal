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

namespace Microsoft.Research.Dkal.LogicEngine.PPIL

open System.Text
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Tree
open AST

/// translate from ITerm AST to compact AST
module Stage1 =
  let printPrefix (sb:StringBuilder) (prefix:Prefix) = for p in prefix |> List.rev do sb.Append(p).Append(" said ") |> ignore

  let prefixize fullprefix orig =
    let rec prefixsize = function
      | JustifiedInfon(i,_) -> prefixsize i
      | SaidInfon(_, i) -> 1 + prefixsize i
      | u -> 0
    let rec addprefix orig = function
      | ppal :: tail -> addprefix (SaidInfon(ppal, orig)) tail
      | [] -> orig
    addprefix orig (fullprefix |> Seq.skip (prefixsize orig) |> List.ofSeq)

  /// translate from regular binary ast to compact ast
  let rec translate (sb: StringBuilder) prefix fullprefix orig = function
      | JustifiedInfon(i, _) ->
          translate sb prefix fullprefix orig i
      | SaidInfon(pc, i) ->
          let p = (pc :?> PrincipalConstant).Name
          translate sb (p::prefix) (pc::fullprefix) orig i
      | ImpliesInfon(l, r) ->
          let (klp, a : AST list) = constrOp sb prefix fullprefix "->" [l;r] orig
          Implies(klp, a.Head, a.Tail.Head)
      | AndInfon(args) ->
          let (klp, args) = constrOp sb prefix fullprefix "&" args orig
          SetFormula(klp, AndOp, args)
      | OrInfon(args) ->
          let (klp, args) = constrOp sb prefix fullprefix "|" args orig
          SetFormula(klp, OrOp, args)
      | EmptyInfon() ->
          let key = sb.Length
          sb.Append( Primitives.EmptyInfon ) |> ignore
          Rel({key=key; len=sb.Length-key; pref=prefix |> List.rev; orig=orig}, Primitives.EmptyInfon)
      | App(rel, args) ->
          printPrefix sb prefix
          let key = sb.Length
          let rn = rel.Name + "(" + (args |> List.map (fun a -> a.ToString()) |> String.concat ",") + ")"
          sb.Append( rn ) |> ignore
          Rel({key = key; 
              len = sb.Length-key; 
              pref = prefix |> List.rev; 
              orig = prefixize fullprefix orig}, rn)
      | _ as t -> failwithf "unknown term %O" t

  and constrOp(sb: StringBuilder) prefix fullprefix op args orig =
      printPrefix sb prefix
      let key = sb.Length      
      sb.Append("(")  |> ignore
      let hd = translate sb [] fullprefix args.Head args.Head
      let tl = args.Tail |> List.map (fun a -> 
        sb.Append(op) |> ignore
        translate sb [] fullprefix a a)
      sb.Append(")") |> ignore
      { key = key; 
        len = sb.Length-key; 
        pref = prefix |> List.rev; 
        orig = prefixize fullprefix orig }, hd :: tl

  let stage1 H Q =
      let sb = StringBuilder()
      let trans lst = lst |> List.map (fun x -> translate sb [] [] x x)
      let H = trans H
      let Q = trans Q
      (H, Q, sb.ToString())