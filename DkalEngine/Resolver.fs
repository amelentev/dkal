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

namespace Microsoft.Research.DkalEngine

open System
open System.IO
open System.Text
open Microsoft.FSharp.Text
open Microsoft.Research.DkalEngine.Util
open Microsoft.Research.DkalEngine.Ast
open Microsoft.Research.DkalEngine.PreAst

module Resolver =
  let err (t:Tok) msg =
    raise (SyntaxError (t.Pos, "resolution error: " + msg + " at '" + t.ToString() + "'"))
    
  let splitList f = function
    | Tok.App (_, "list", lst) ->
      List.map f lst |> List.concat
    | t -> f t
  
  let asName = function
    | Tok.Id (_, name) -> name
    | t -> err t "expecting name"
    
  let getPrincipal (ctx:Context) who =
    let who = asName who
    if not (ctx.principals.ContainsKey who) then
      ctx.AddPrincipal who
    ctx.principals.[who]

  let rec resolveTerm (ctx:Context) (expectedType:Type) tok =
    let res =
      match tok with
        | Tok.App (_, ".", [Tok.Id (_, table); Tok.Id (_, column)]) ->
          Term.Const (Const.Column (table, column))
        | Tok.App (pos, name, [Tok.Group (_, ')', args)]) ->
          resolveTerm ctx expectedType (Tok.App (pos, name, args |> List.filter (function Tok.Id (_, ",") -> false | _ -> true)))
        | Tok.App (_, name, args) ->
          match ctx.functions.TryGetValue name with
            | true, fn ->
              if fn.argTypes.Length <> args.Length then
                err tok ("wrong number of arguments, expecting " + fn.argTypes.Length.ToString() + " got " + args.Length.ToString())
              else
                //System.Console.WriteLine ("{0} <-> {1}", l2s fn.argTypes, l2s args)
                let args = List.map2 (fun (v:Var) arg -> resolveTerm ctx v.typ arg) fn.argTypes args
                Term.App (fn, args)
            | _ -> err tok "undefined function"
        | Tok.Id (_, name) when ctx.principals.ContainsKey name ->
          Term.Const (Const.Principal ctx.principals.[name])
        | Tok.Id (_, name) when expectedType = Type.Principal ->
          ctx.AddPrincipal name
          Term.Const (Const.Principal ctx.principals.[name])
        | Tok.Var (pos, name) ->
          if not (ctx.vars.ContainsKey name) then
            ctx.AddVar name
          let v = ctx.vars.[name]  
          if v.typ = Type.Unbound then
            v.typ <- expectedType
          Term.Var v
        | Tok.StringLiteral (_, s) -> 
          Term.Const (Const.Text s)
        | Tok.Int (_, i) ->
          Term.Const (Const.Int i)
        | Tok.Group(_,c,ts) -> resolveTerm ctx expectedType ts.[0]
        | _ -> err tok "expecting a term"
    //System.Console.WriteLine ("resolved: " + tok.ToString() + " -> " + res.ToString() + ":" + res.Type.ToString() + " (expecting " + expectedType.ToString() + ")")
    if expectedType <> Type.Unbound && expectedType <> res.Type then
      err tok ("type mismatch: expecting " + expectedType.ToString() + " got " + res.Type.ToString())
    res
             
  let rec multiAnd = function
    | [] -> Infon.Empty 
    | [x] -> x
    | (x:Term) :: xs -> Infon.And (x, multiAnd xs)
              
  let rec resolveInfon (ctx:Context) = function
    | Tok.Group (_, ')', [e]) -> resolveInfon ctx e
    | Tok.App (_, ("==>"|"-->"), [premise; consequence]) ->
      let premise = resolveProviso ctx premise
      multiAnd (splitList (fun t -> [Infon.Follows (premise, resolveInfon ctx t)]) consequence)
    | Tok.App (pos, ("tdonS"|"tdonI"|"said"|"implied" as name), [p; i]) ->
      let p = resolveTerm ctx Type.Principal p
      let i = resolveInfon ctx i
      match name with
        | "said" -> Infon.Said (p, i)
        | "implied" -> Infon.Implied (p, i)
        | "tdonS" -> Infon.Follows (Infon.Said (p, i), i)
        | "tdonI" -> Infon.Follows (Infon.Implied (p, i), i)
        | _ -> failwith "cannot happen"
    | tok ->
      resolveTerm ctx Type.Infon tok
  and resolveProviso ctx ts =
    multiAnd (splitList (fun t -> [resolveInfon ctx t]) ts)
  
  let resolveSentInfon ctx t =
    splitList (fun t -> [resolveInfon ctx t]) t
  
  let resolve (ctx:Context) t = 
    ctx.vars.Clear()
    //System.Console.WriteLine ("resolving: " + t.ToString())
    match t with
      | Tok.App (_, "knows", [who; what]) ->
        let who = getPrincipal ctx who
        splitList (fun t -> [Assertion.Knows { ai = { origin = t.Pos; principal = who }; infon = resolveInfon ctx t }]) what
      | Tok.App (_, "send", [who; precond; targets]) ->
        let who = getPrincipal ctx who          
        let precond = resolveProviso ctx precond
        let doTarget = function
          | Tok.App (_, name, [target; what]) ->
            let kind = 
              match name with
                | "send_target" -> CommKind.Processed
                | "send_target_cert" -> CommKind.Certified
                | "say_target_cert" -> CommKind.CertifiedSay
                | _ -> err t "expecting 'to' after 'then they send'"
            let target = resolveTerm ctx Type.Principal target
            [Assertion.SendTo { ai = { origin = t.Pos; principal = who }
                                target = target
                                message = multiAnd (splitList (fun t -> [resolveInfon ctx t]) what)
                                proviso = Infon.Empty
                                trigger = precond
                                certified = kind }]
          | t ->
            err t "expecting 'to' after 'then they send'"
        splitList doTarget targets
      | Tok.NewLine _ -> []
      | t ->
        err t "invalid top-level assertion"
        
  let resolveFunctions (ctx:Context) =
    let resolveFun (fn:Function) =
      ctx.vars.Clear()
      fn.retType :: fn.argTypes |> List.iter (fun v -> ctx.vars.Add (v.name, v))
      match Parser.applyRules ctx (fn.body :?> list<Tok>) with
        | [tok; Tok.NewLine (_)] ->
          fn.body <- (resolveTerm ctx Type.Unbound tok :> obj)
        | tok :: _ as toks ->
          err tok ("only a single expression expected as the function body, got " + Parser.ts2s toks)
        | [] ->
          raise (SyntaxError (fakePos, "an expression expected as a function body after 'is'"))          
    List.iter resolveFun ctx.pendingFunctions
    ctx.pendingFunctions <- []    
    