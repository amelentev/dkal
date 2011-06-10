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

namespace Microsoft.Research.Dkal.Substrate.Basic

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Substrate

open System.Collections.Generic

/// The BasicSubstrate is a calculator substrate that does not handle data. It 
/// can solve basic arithmetic computations. E.g., {| "basic" | Y := X + 10 |}
type BasicSubstrate() =

  let evalf (f:Function) (args:list<obj>) =
    let cmp (a:obj) b =
      let aa = a :?> System.IComparable
      aa.CompareTo(b)
    match f.Name, args with
    | BasicPrimitives.And, args ->
      box (not (args |> List.exists (fun x -> x.Equals(false))))
    | BasicPrimitives.Or, args ->
      box (args |> List.exists (fun x -> x.Equals(true)))
    | BasicPrimitives.Not, [a] ->
      box (a.Equals(false))
    | BasicPrimitives.Divide, [a; b] ->
      box ((a:?>int) / (b:?>int))
    | BasicPrimitives.Plus, args ->
      let (a: seq<int>) = args |> Seq.cast
      box (a |> Seq.sum)
    | BasicPrimitives.Minus, args ->
      let (a: list<int>) = args |> Seq.cast |> Seq.toList
      box (a.Head - List.sum a)
    | BasicPrimitives.Uminus, [a] ->
      box -(a :?> int)
    | BasicPrimitives.Eq, [a; b] ->
      box (a.Equals(b))
    | BasicPrimitives.Gt, [a; b] ->
      box (cmp a b > 0)
    | BasicPrimitives.Gte, [a; b] ->
      box (cmp a b >= 0)
    | BasicPrimitives.Lt, [a; b] ->
      box (cmp a b < 0)
    | BasicPrimitives.Lte, [a; b] ->
      box (cmp a b <= 0)
    | BasicPrimitives.Neq, [a; b] ->
      box (not (a.Equals(b)))
    | BasicPrimitives.Times, [a; b] ->
      box ((a:?>int)*(b:?>int))
    | _ -> failwithf "unknown function %A on args %A" f args

  let rec evaluate (substs: ISubstitution list) (expr: ITerm) =
    match expr with
      | :? IVar as v -> 
        [ for subst in substs do 
            if subst.DomainContains v then
              yield! evaluate [subst] (subst.Apply(v))
            else
              failwithf "Found free variable %O when solving basic substrate term" v ]
      | :? IConst as c -> [ for subst in substs -> (c, subst) ]
      | AsBoolean(query) ->
        [ for subst in substs do
            let substs' = SubstrateDispatcher.Solve [query] [subst]
            if Seq.isEmpty substs' then
              yield (Constant(false) :> IConst, subst) 
            else
              for subst' in substs' do
                yield (Constant(true) :> IConst, subst') ]
      | App(f, args) ->
        [ for subst in substs do
            let initialArgsLists = new HashSet<_>()
            initialArgsLists.Add [] |> ignore
            let cArgsLists, substs = List.fold (fun (cArgsLists, substs) arg -> 
                                                  let cArgs', substs' = List.unzip <| evaluate substs arg
                                                  let cArgsLists' = new HashSet<_>()
                                                  for cArgsList in cArgsLists do
                                                    for cArg' in cArgs' do
                                                      cArgsLists'.Add(cArgsList @ [cArg']) |> ignore
                                                  cArgsLists', substs') (initialArgsLists, [subst]) args
            for cArgsList in cArgsLists do
              yield Constant(evalf f (List.map (fun (c : IConst) -> c.Value) cArgsList)) :> IConst, subst]
      | _ as t -> failwithf "unknown term %A" t

  let solve1Many (substs: ISubstitution list) (q: BasicSubstrateTerm)  =
    let substs =     
      [ for r, subst in evaluate substs q.Right do
          match q.Left.UnifyFrom subst r with
          | Some subst' -> yield subst'
          | None -> () ]
    substs

  interface ISubstrate with

    member bs.Namespaces = new HashSet<_>([BasicPrimitives.BasicNamespace])

    member bs.Update _ = failwith "Basic substrate does not support updates"

    member bs.AreConsistentUpdates _ = failwith "Basic substrate does not support updates"

    member bs.RequiredVars (query: ISubstrateQueryTerm) = 
      match query with
      | :? BasicSubstrateTerm as bst -> bst.Right.Vars
      | _ -> failwithf "Basic substrate does not understand term %O" query

    member bs.Solve (queries: ISubstrateQueryTerm seq) (substs: ISubstitution seq) =
      let queries: BasicSubstrateTerm list = queries |> Seq.cast |> Seq.toList
//      let solve1Many substs (query: BasicSubstrateTerm) =
//        substs |> Seq.collect (solve11 query)
      let substs = List.fold solve1Many (Seq.toList substs) queries
      seq substs
