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

module Microsoft.Research.DkalEngine.SExpr

open System
open System.IO
open System.Text
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Text
open Microsoft.Research.DkalEngine.Util

type PP =
  | PString of string
  | PBlock of int * list<PP>

  member this.Append str =
    match this with
      | PString s -> PString (s + str)
      | PBlock (n, lst) ->
        match List.rev lst with
          | x :: xs ->
            PBlock (n + str.Length, List.rev (x.Append str :: xs))
          | [] -> PString str          

  member this.Prepend (str:string) =
    match this with
      | PString s -> PString (str + s)
      | PBlock (n, x :: xs) ->
        PBlock (n + str.Length, x.Prepend str :: xs)
      | PBlock (_, []) -> PString str

  member this.Length =
    match this with
      | PString s -> s.Length
      | PBlock (n, _) -> n

  member this.Print margin (sb:System.Text.StringBuilder) =
    let wr (s:string) = sb.Append s |> ignore
    let rec wrpp = function
      | PP.PString s -> wr s
      | PP.PBlock (_, l) ->
        match List.rev l with
          | x :: xs ->
            for x in List.rev xs do
              wrpp x
              wr " "
            wrpp x
          | [] -> () 
    let rec line ind = function
      | PP.PString s ->
        wr (String(' ', ind))
        wr s
        wr "\n"
      | PP.PBlock (len, x :: xs) as pp ->
        if len + ind > margin then
          line ind x
          List.iter (line (ind + 2)) xs
        else
          wr (String(' ', ind))
          wrpp pp
          wr "\n"
      | PP.PBlock (_, []) -> ()
    line 0 this
    
  static member Block lst =
    let lst = lst |> List.filter (function PString "" -> false | _ -> true)
    PBlock (List.map (fun (s:PP) -> s.Length) lst |> List.sum, lst)

type SX =
  | App of Pos * string * list<SX>
  | Var of Pos * string
  | Int of Pos * int
  | Float of Pos * float
  | String of Pos * string

  member this.Pos =
    match this with
      | App (p, _, _)
      | Var (p, _)
      | Int (p, _)
      | Float (p, _)
      | String (p, _) -> p

  member this.WriteTo (sb:StringBuilder) =
    let wr (s:string) = sb.Append s |> ignore
    match this with
      | App (_, n, args) ->
        wr "("
        wr n
        for a in args do
          wr " "
          a.WriteTo sb
        wr ")"
      | Var (_, n) -> wr n
      | Int (_, k) -> wr (k.ToString())
      | Float (_, f) -> wr (f.ToString())
      | String (_, p) -> 
        wr "\""
        // TODO quote
        wr p
        wr "\""
  
  member this.Serialize() = tempStringBuilder this.WriteTo

  member this.PrettyPrint () = 
    let rec aux = function
      | App (_, n, args) ->      
        (PP.Block (PP.PString ("(" + n) :: List.map aux args)).Append ")"
      | t ->
        PP.PString (tempStringBuilder t.WriteTo)
    let r = tempStringBuilder ((aux this).Print 90)
    if r.Length > 0 && r.IndexOf '\n' = r.Length - 1 then
      r.Substring (0, r.Length - 1)
    else r


  override this.ToString() = this.PrettyPrint()

  static member IsAssoc = function
    | "and" | "&&" | "||" -> true
    | _ -> false

  member this.UnAssoc() =
    match this with
      | App (p, name, [a]) when SX.IsAssoc name -> a.UnAssoc()
      | App (p, name, (_ :: _ :: _ :: _ as args)) when SX.IsAssoc name ->
        args |> List.rev |> List.reduce (fun a b -> App (p, name, [b; a]))
      | App (p, "tdon", [w; i]) ->
        App (p, "follows", [App (p, "said", [w; i]); i])
      | t -> t

  member this.Optimize() =
    let opt (t:SX) = t.Optimize()
    match this with
      | App (p, name, App (_, name', a1) :: a2) when SX.IsAssoc name && name = name' ->
        opt (App (p, name, a1 @ a2))
      | App (p, name, [a1; App (_, name', a2)]) when SX.IsAssoc name && name = name' ->
        opt (App (p, name, a1 :: a2))
      | App (p, name, args) ->
        App (p, name, List.map opt args)
      | t -> t

  static member OptimizeList lst =
    let opt (t:SX) = t.Optimize()
    lst |> Seq.toList |> List.map opt

  static member private FromLexbuf filename lexbuf =
    let err pos s = raise (SyntaxError (pos, s))
    let rec shift() =
      let tok = SxLexer.token lexbuf
      (tok, getPos())
    and getPos() = { filename = filename; line = lexbuf.StartPos.Line; column = lexbuf.StartPos.Column }

    let rec parse pos = function
      | SxLexer.IntLiteral i -> Int (pos, i)
      | SxLexer.StringLiteral s -> String (pos, s)
      | SxLexer.Id n -> Var (pos, n)
      | SxLexer.LParen ->
        match shift() with
          | SxLexer.Id n, _ -> 
            let rec aux acc =
              match shift() with
                | SxLexer.RParen, _ -> App (pos, n, List.rev acc)
                | tok, pos -> aux (parse pos tok :: acc)
            aux []
          | tok, _ -> err pos ("expecting function name after '(', got " + tok.ToString())
      | tok -> err pos ("expecting s-expression, got " + tok.ToString())

    let pos = getPos()
    let rec aux acc =
      match shift() with
        | SxLexer.Eof, _ -> List.rev acc
        | tok, pos -> aux (parse pos tok :: acc)
    aux []

  static member FromStream filename stream =
    SX.FromLexbuf filename (Lexing.LexBuffer<char>.FromTextReader stream)

  static member FromString (text:string) =
    let chars = Array.create text.Length ' '
    text.CopyTo (0, chars, 0, chars.Length)
    SX.FromLexbuf "-string-" (Lexing.LexBuffer<char>.FromChars chars)
