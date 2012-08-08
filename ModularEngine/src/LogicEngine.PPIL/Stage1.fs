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

module Stage1 =
  let printPrefix (sb:StringBuilder) (prefix:Prefix) = for p in prefix |> List.rev do sb.Append(p).Append(" said ") |> ignore

  /// translate from regular binary ast to compact ast
  let rec translate (sb: StringBuilder) prefix = function
      | SaidInfon(pc, i) ->
          let p = (pc :?> PrincipalConstant).Name
          translate sb (p::prefix) i
      | ImpliesInfon(l, r) ->
          let (klp, a : AST list) = constrOp sb prefix "->" [l;r]
          Implies(klp, a.Head, a.Tail.Head)
      | AndInfon(args) ->
          let (klp, args) = constrOp sb prefix "&" args
          SetFormula(klp, AndOp, args)
      | OrInfon(args) ->
          let (klp, args) = constrOp sb prefix "|" args
          SetFormula(klp, OrOp, args)
      | EmptyInfon() ->
          let key = sb.Length
          sb.Append( Primitives.EmptyInfon ) |> ignore
          Rel((key, sb.Length-key, prefix |> List.rev), Primitives.EmptyInfon)
      | App(rel, args) ->
          printPrefix sb prefix
          let key = sb.Length
          let rn = rel.Name + "(" + (args |> List.map (fun a -> a.ToString()) |> String.concat ",") + ")"
          sb.Append( rn ) |> ignore
          Rel((key, sb.Length-key, prefix |> List.rev), rn)
      | _ as t -> failwithf "unknown term %O" t

  and constrOp(sb: StringBuilder) prefix op args =
      printPrefix sb prefix
      let key = sb.Length      
      sb.Append("(")  |> ignore
      let hd = translate sb [] args.Head
      let tl = args.Tail |> List.map (fun a -> 
        sb.Append(op) |> ignore
        translate sb [] a)
      sb.Append(")") |> ignore
      (key, sb.Length-key, prefix |> List.rev), hd :: tl

  let stage1 H Q =
      let sb = StringBuilder()
      let trans lst = lst |> List.map (translate sb [])
      let H = trans H
      let Q = trans Q
      (H, Q, sb.ToString())