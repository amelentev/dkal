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

open Microsoft.Research.DkalEngine.Ast
open Microsoft.Research.DkalEngine.Util

/// Term<->string serialization/deserialization. Requires a parsing context with
/// function definitions.
type Serializer(ctx:ParsingCtx) =
  let idChar c = Char.IsLetterOrDigit c || c = '_'
  
  member this.SerializeTerm t =
    let sb = System.Text.StringBuilder()
    let wr (s:obj) = sb.Append s |> ignore
    let quote (s:string) =
      for c in s do
        if idChar c then wr c
        else wr '\\'; wr c       
    let visitedVars = dict()
    let rec aux = function
      | Term.Const (Int i) -> wr "#"; wr i
      | Term.Const (Bool b) -> wr "@"; wr (if b then "T" else "F")
      | Term.Const (Principal p) -> wr "%"; quote p.name
      | Term.Const (Column _) -> failwith "cannot serilize column"
      | Term.Var v -> 
        wr "$"; quote v.name
        if not (visitedVars.ContainsKey v.id) then
          wr ":"; quote v.typ.name
          visitedVars.Add (v.id, true)
      | Term.App (f, []) ->
        quote f.name; wr "()"
      | Term.App (f, args) ->
        quote f.name
        wr "("
        for t in args do
          aux t
          wr ", "
        sb.Length <- sb.Length - 2
        wr ")"
    aux t
    sb.ToString()
          
  member this.DeserializeTerm (s:string) =
    let parseName i =
      let sb = System.Text.StringBuilder()
      let wr (s:obj) = sb.Append s |> ignore
      let rec loop i =
        if s.[i] = '\\' then
          wr s.[i + 1]; loop (i + 2)
        elif idChar s.[i] then
          wr s.[i]; loop (i + 1)
        else
          (i, sb.ToString())
      loop i
    let parseInt i =
      let rec findEnd cur =
        if (i = cur && s.[i] = '-') || Char.IsDigit s.[cur] then findEnd (cur + 1)
        else cur
      let theEnd = findEnd i
      (theEnd, System.Int32.Parse (s.Substring (i, theEnd - i)))
    let vars = dict()
    let rec parseTerm i =
      match s.[i] with
        | '#' ->
          let (i, v) = parseInt (i + 1)
          (i, Term.Const (Const.Int v))
        | '@' ->
          (i + 2, Term.Const (Const.Bool (s.[i+1] = 'T')))
        | '%' ->
          let (i, v) = parseName (i + 1)
          (i, Term.Const (Const.Principal (ctx.LookupOrAddPrincipal v)))
        | '$' ->
          let (i, name) = parseName (i + 1)
          let (i, tp) =
            if s.[i] = ':' then
              let (i, tpName) = parseName (i + 1)
              (i, Some (ctx.LookupType tpName))
            else (i, None)
          let v = 
            match vars.TryGetValue name with
              | true, v -> v
              | _ ->
                let v = ctx.MakeVar name tp.Value
                vars.Add (name, v)
                v
          (i, Term.Var (v))          
        | c when c = '\\' || idChar c ->
          let (i, name) = parseName i
          let fn = ctx.LookupFunction name
          let rec parseList acc i =
            match s.[i] with
              | ')' -> (i + 1, List.rev acc)
              | ' ' | ',' -> parseList acc (i + 1)
              | _ ->
                let (i, t) = parseTerm i
                parseList (t :: acc) i
          if s.[i] <> '(' then failwith "expecting ( in term app"
          let (i, args) = parseList [] (i + 1)
          (i, Term.App (fn, args))          
        | _ -> failwith "invalid character"  
    snd (parseTerm 0)
          

  member this.SerializeTerms ts =
    this.SerializeTerm (Term.App (ctx.LookupFunction "__tuple", ts))
  
  member this.DeserializeTerms s =
    match this.DeserializeTerm s with
      | Term.App ({ name = "__tuple" }, ts) -> ts
      | _ -> failwith "expecting __tuple(...)"

  member this.SerializeMessage (msg:Ast.Message) =
    this.SerializeTerms [Term.Const (Const.Principal msg.source); Term.Const (Const.Principal msg.target); msg.message; msg.proviso]

  member this.DeserializeMessage s =
    match this.DeserializeTerms s with
      | [Term.Const (Const.Principal src); Term.Const (Const.Principal trg); msg; prov] ->
        { source = src
          target = trg
          message = msg
          proviso = prov }
      | _ -> failwith "wrong serialized message format"
