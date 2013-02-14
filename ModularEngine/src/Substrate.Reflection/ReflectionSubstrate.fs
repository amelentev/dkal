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
namespace Microsoft.Research.Dkal.Substrate.Reflection

open System.Collections.Generic
open NLog
open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Substrate

module ReflectionSubstrateModule =
  let log = LogManager.GetLogger("Substrate.Reflection")
  let constantFromValue typ v =
    if typ = Type.Principal then PrincipalConstant(v.ToString()) :> Constant
    else Constant v

  let addValueToSubst (subst:ISubstitution,ok) (value,var) =
    match var,ok with
    | _,false -> subst, false
    | Var(v),_ -> subst.Extend(v, constantFromValue v.Type value), ok
    | Const(c),_ -> subst, c.Value.Equals(value)
    | _,_ -> failwith "unknown term %O" var

  let getArgs (input:ITerm list) subst =
    let inputs = input |> List.map (fun x -> x.Apply subst)
    if inputs |> List.exists (fun x -> not (x :? IConst)) then 
      None
    else
      Some(Seq.cast inputs |> Seq.map (fun (c:IConst) -> c.Value) |> Seq.toArray)

  let constructOutputSubst subst (res:obj) = function
    | [] when true.Equals(res) -> Seq.singleton subst // no output => boolean query
    | [] -> Seq.empty
    | output ->
      let output = output |> List.map (fun (x:ITerm) -> x.Apply subst)
      let toSeq (res:obj) = 
        match res with
        | null -> Seq.empty
        | :? (obj seq) as res -> res
        | _ -> Seq.singleton res
      toSeq res |> Seq.collect (fun res1 ->
        let subst,ok = (Seq.zip (toSeq res1) output) |> 
                          Seq.fold addValueToSubst (subst,true)
        if ok then Seq.singleton subst else Seq.empty)

  let solve (query: ReflectionQuery) (subst: ISubstitution) =
    log.Debug("Reflection {0} under {1}", query, subst)
    match getArgs query.input subst with
    | None -> Seq.empty
    | Some(args) ->
      let res = query.meth.Invoke(null, args)
      constructOutputSubst subst res query.output

type ReflectionSubstrate(namespaces: string list, clas: System.Type) =
  inherit ASubstrate(namespaces, fun q -> ReflectionSubstrateModule.solve (q:?>ReflectionQuery))
  member x.clas = clas
  new(ns, clas) = ReflectionSubstrate(ns, System.Type.GetType(clas))
  interface ISubstrate