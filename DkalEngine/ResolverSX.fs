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

module ResolverSX =
  type Context = PreAst.Context

  let err (t:SX) msg =
    raise (SyntaxError (t.Pos, "resolution error: " + msg + " at '" + t.ToString() + "'"))
    
  let asName = function
    | SX.Var (_, name) -> name
    | t -> err t "expecting name"
    
  let getPrincipal (ctx:Context) who =
    let who = asName who
    if not (ctx.principals.ContainsKey who) then
      ctx.AddPrincipal who
    ctx.principals.[who]

  let rec resolveTerm (ctx:Context) (expectedType:Type) (tok:SX) =
    let res =
      match tok.UnAssoc() with
        | SX.App (_, ".", [SX.Var (_, table); SX.Var (_, column)]) ->
          Term.Const (Const.Column (table, column))
        | SX.App (_, name, []) when ctx.principals.ContainsKey name ->
          Term.Const (Const.Principal ctx.principals.[name])
        | SX.App (_, name, []) when expectedType = Type.Principal ->
          ctx.AddPrincipal name
          Term.Const (Const.Principal ctx.principals.[name])
        | SX.App (_, name, args) ->
          match ctx.functions.TryGetValue name with
            | true, fn ->
              if fn.argTypes.Length <> args.Length then
                err tok ("wrong number of arguments, expecting " + fn.argTypes.Length.ToString() + " got " + args.Length.ToString())
              else
                //System.Console.WriteLine ("{0} <-> {1}", l2s fn.argTypes, l2s args)
                let args = List.map2 (fun (v:Var) arg -> resolveTerm ctx v.typ arg) fn.argTypes args
                Term.App (fn, args)
            | _ -> err tok "undefined function"
        | SX.Var (pos, name) ->
          if not (ctx.vars.ContainsKey name) then
            ctx.AddVar name
          let v = ctx.vars.[name]  
          if v.typ = Type.Unbound then
            v.typ <- expectedType
          Term.Var v
        | SX.String (_, s) -> 
          Term.Const (Const.Text s)
        | SX.Int (_, i) ->
          Term.Const (Const.Int i)
        | _ -> err tok "expecting a term"
    //System.Console.WriteLine ("resolved: " + tok.ToString() + " -> " + res.ToString() + ":" + res.Type.ToString() + " (expecting " + expectedType.ToString() + ")")
    if expectedType <> Type.Unbound && expectedType <> res.Type then
      err tok ("type mismatch: expecting " + expectedType.ToString() + " got " + res.Type.ToString())
    res
             
  let resolveInfon ctx tok = resolveTerm ctx Type.Infon tok

  let chopLast l =
    match List.rev l with
      | x :: xs -> (xs |> List.rev, x)
      | [] -> failwith ""
  
  let resolve (ctx:Context) top = 
    ctx.vars.Clear()
    let ai t =
      if ctx.me.IsNone then err t "missing (me ...)"
      else { origin = t.Pos; principal = ctx.me.Value } 
    let resolveVar = function
      | SX.App (_, typeName, ([SX.Var _]|[] as args)) as t ->
        let varName =
          match args with
            | [SX.Var (_, name)] -> name
            | _ -> ""
        match ctx.types.TryGetValue typeName with
          | true, t ->
            let v = ctx.MkVar varName t
            ctx.vars.Add (v.name, v)
            v
          | _ -> err t ("unbound type " + typeName)
      | t -> err t "expecting (type-name var-name)"
    //System.Console.WriteLine ("resolving: " + t.ToString())
    match top with
      | SX.App (_, "knows", what) ->
        what |> List.map (fun t -> Assertion.Knows { ai = ai t; infon = resolveInfon ctx t })
      | SX.App (_, ("fun"|"macro" as kind), retType :: SX.Var (_, name) :: argsBody) ->
        let (args, body) =
          if kind = "fun" then argsBody, None
          else fst (chopLast argsBody), Some (snd (chopLast argsBody))
        let fn =
          ({
            id = ctx.NextId()
            name = name
            retType = resolveVar retType
            argTypes = List.map resolveVar args
            body = null
          } : Function)
        if body.IsSome then
          fn.body <- resolveTerm ctx Type.Bool body.Value
        ctx.functions.Add (fn.name, fn)
        []
      | SX.App (_, "type", [SX.Var (_, name)]) ->
        ctx.AddType name
        []
      | SX.App (_, "me", [SX.Var (_, name)]) ->
        if ctx.me.IsSome then
          err top "(me ...) given twice"
        ctx.AddPrincipal name
        let p = ctx.principals.[name]
        ctx.principals.["me"] <- p
        ctx.me <- Some p
        []
      | SX.App (_, "set", [SX.Var (_, name); SX.String (_, s)]) ->
        ctx.options.[name] <- s
        []
      | SX.App (_, "send", precond :: targets) ->
        let precond = resolveInfon ctx precond
        let doTarget = function
          | SX.App (_, name, target :: what) as t ->
            let kind = 
              match name with
                | "say*" -> CommKind.Processed
                | "to" -> CommKind.Certified
                | "say" -> CommKind.CertifiedSay
                | _ -> err t "expecting (to ...) or (say ...) after (send ...)"
            let target = resolveTerm ctx Type.Principal target
            what |> List.map (fun what ->
                Assertion.SendTo { ai = ai t
                                   target = target
                                   message = resolveInfon ctx what
                                   proviso = Infon.Empty
                                   trigger = precond
                                   certified = kind })
          | t ->
            err t "expecting 'to' after 'then they send'"
        targets |> List.collect doTarget
      | t ->
        err t "invalid top-level assertion"
        
