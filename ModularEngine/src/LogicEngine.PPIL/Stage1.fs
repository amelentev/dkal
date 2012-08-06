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

  /// translate to binary tree
  let rec normalize2 = function
        | App(f, a1 :: a2 :: a3 :: tl) ->
          let f' = {Name=f.Name; RetType=f.RetType; ArgsType=List.tail f.ArgsType; Identity=f.Identity}
          let n = normalize2( App(f', a2::a3::tl) )
          let f'' = {Name=f.Name; RetType=f.RetType; ArgsType=[f.ArgsType.Head; f.RetType]; Identity=f.Identity}
          App(f'', [normalize2 a1; n])
        | App(f, lst) ->
          App(f, lst |> List.map normalize2)
        | t -> t

  /// translate from regular binary ast to compact ast
  let rec translate (sb: StringBuilder) prefix = function
      | SaidInfon(pc, i) ->
          let p = (pc :?> PrincipalConstant).Name
          sb.Append("(").Append(p).Append(" said ") |> ignore
          let res = translate sb (p::prefix) i
          sb.Append(")") |> ignore
          res
      | AndInfon([l; r]) ->
          constrBinary sb prefix AndOp l r
      | ImpliesInfon(l, r) ->
          constrBinary sb prefix ImpliedOp l r
      | OrInfon([l; r]) ->
          constrBinary sb prefix OrOp l r
      | EmptyInfon() ->
          let key = sb.Length
          sb.Append( Primitives.EmptyInfon ) |> ignore
          Rel((key, sb.Length-key, prefix |> List.rev), Primitives.EmptyInfon)
      | App(rel, args) ->
          let key = sb.Length
          let rn = rel.Name + "(" + (args |> List.map (fun a -> a.ToString()) |> String.concat ",") + ")"
          sb.Append( rn ) |> ignore
          Rel((key, sb.Length-key, prefix |> List.rev), rn)
      | _ as t -> failwithf "unknown term %O" t

  and constrBinary (sb: StringBuilder) prefix op l r =
      let key = sb.Length
      sb.Append("(")  |> ignore
      let l = translate sb [] l
      sb.Append(op.ToString()) |> ignore
      let r = translate sb [] r
      sb.Append(")") |> ignore
      Binary((key, sb.Length-key, prefix |> List.rev), op, l, r)

  let stage1 H Q =
      let sb = StringBuilder()
      let trans lst = lst |> List.map normalize2  |> List.map (translate sb [])
      let H = trans H
      let Q = trans Q
      (H, Q, sb.ToString())