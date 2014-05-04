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
open Microsoft.Research.Dkal.Interfaces
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
  let rec translate constrOp prefix fullprefix orig = function
      | JustifiedInfon(i, _) ->
          translate constrOp prefix fullprefix orig i
      | SaidInfon(pc, i) ->
          let p = (pc :?> PrincipalConstant).Name
          translate constrOp (p::prefix) (pc::fullprefix) orig i
      | ImpliesInfon(l, r) ->
          let (klp, a : AST list) = constrOp prefix fullprefix "->" [l;r] orig
          Implies(klp, a.Head, a.Tail.Head)
      | AndInfon(args) ->
          let (klp, args) = constrOp prefix fullprefix "&" args orig
          SetFormula(klp, AndOp, args)
      | OrInfon(args) ->
          let (klp, args) = constrOp prefix fullprefix "|" args orig
          SetFormula(klp, OrOp, args)
      | EmptyInfon() ->
          let (klp,_) = constrOp prefix fullprefix trueLabel [] orig
          Rel(klp, Primitives.EmptyInfon)
      | App(rel, args) ->
          let rn = rel.Name + "(" + (args |> List.map (fun a -> a.ToString()) |> String.concat ",") + ")"
          let (klp, _) = constrOp prefix fullprefix rn [] orig
          Rel(klp, rn)
      | _ as t -> failwithf "unknown term %O" t

  /// serialize ast and assign node.key = position in string
  let rec serialize (sb: StringBuilder) prefix fullprefix (op: string) (args: ITerm list) orig =
      printPrefix sb prefix
      let key = sb.Length      
      if args.IsEmpty then 
        sb.Append(op) |> ignore
        { key = key;
          len = sb.Length-key; 
          pref = prefix |> List.rev;
          orig = prefixize fullprefix orig }, []
      else
        sb.Append("(")  |> ignore
        let hd = translate (serialize sb) [] fullprefix args.Head args.Head
        let tl = args.Tail |> List.map (fun a -> 
          sb.Append(op) |> ignore
          translate (serialize sb) [] fullprefix a a)
        sb.Append(")") |> ignore
        { key = key; 
          len = sb.Length-key; 
          pref = prefix |> List.rev; 
          orig = prefixize fullprefix orig }, hd :: tl

  /// assign sequential keys to AST
  let rec seqKeys id prefix fullprefix (op: string) (args: ITerm list) orig =
      let args = args |> List.map (fun a -> translate (seqKeys id) [] fullprefix a a)
      let res = { key = !id; len = 0; pref = prefix |> List.rev; orig = prefixize fullprefix orig }, args
      incr id
      res

  let stage1serialize H Q =
      let sb = StringBuilder()
      let trans = List.map (fun x -> translate (serialize sb) [] [] x x)
      let H = trans H
      let Q = trans Q
      (H, Q, sb.ToString())

  let stage1 H Q =
      let id = ref 0
      let trans = List.map (fun x -> translate (seqKeys id) [] [] x x)
      let H = trans H
      let Q = trans Q
      (H, Q, !id)