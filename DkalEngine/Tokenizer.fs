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
open Microsoft.Research.DkalEngine.PreToken
open Microsoft.Research.DkalEngine.PreAst
open Microsoft.Research.DkalEngine.Ast

module Tokenizer =
  let rec tokenize includeStack getStream lexbuf : list<Tok> =
    let err pos s = raise (SyntaxError (pos, s))
    
    let rec handleLine acc = 
      let tok0 = Lexer.token lexbuf
      let pos = { filename = List.head includeStack; line = lexbuf.StartPos.Line; column = lexbuf.StartPos.Column }
      let newAcc = (pos, tok0) :: acc
      match tok0 with        
        | Tok0.Eof -> false, List.rev ((pos, Tok0.NewLine) :: acc), pos
        | Tok0.NewLine -> true, List.rev newAcc, pos
        | Tok0.Spaces _ as t ->
          match acc with
            | [] -> handleLine newAcc
            | _ -> handleLine acc
        | Tok0.Invalid s ->
          err pos ("invalid character in input '" + s + "'")
        | Tok0.Late _
        | Tok0.StringLiteral _
        | Tok0.Var _
        | Tok0.Id _
        | Tok0.Int _
        | Tok0.Float _
        | Tok0.LParen _
        | Tok0.RParen _ -> handleLine newAcc
    
    let rec collectLines acc =
      let cont, line, pos = handleLine []
      let line, indent =
        match line with
          | (_, Tok0.Spaces n) :: rest -> rest, n
          | _ -> line, 0      
      let acc = 
        match line with
          | (pos, Tok0.Id "#") :: (_, Tok0.Id "include") :: (_, Tok0.StringLiteral filename) :: _ ->
            if includeStack.Length > 7 then
              err pos "maximal #include depth exceeded"
            else
              let fn () =
                tokenize (filename :: includeStack) getStream (Lexing.LexBuffer<char>.FromTextReader (getStream (pos, filename))) :> obj
              (0, [(pos, Tok0.Late fn)]) :: acc
          | [(_, Tok0.NewLine)] -> acc
          | _ -> (indent, line) :: acc
      if cont then collectLines acc else List.rev acc

    let rec group acc = function
      | [] -> List.rev acc, []
      | (pos, tok) :: rest ->
        let cont add = group (add :: acc) rest
        match tok with
          | Tok0.Id n -> cont (Tok.Id (pos, n))
          | Tok0.Var n -> cont (Tok.Var (pos, n))
          | Tok0.Int i -> cont (Tok.Int (pos, i))
          | Tok0.Float f -> cont (Tok.Float (pos, f))
          | Tok0.NewLine -> cont (Tok.NewLine pos)
          | Tok0.StringLiteral s -> cont (Tok.StringLiteral (pos, s))
          
          | Tok0.LParen c ->
            match group [] rest with
              | subGroup, ((_, Tok0.RParen c') :: rest) when c = c' ->
                group (Tok.Group (pos, c, subGroup) :: acc) rest
              | _, [] -> err pos ("did not find " + c.ToString() + " before end of the line")
              | _, ((pos', Tok0.RParen c') :: _) -> err pos' ("expecting " + c.ToString() + "; got " + c'.ToString())
              | _, (_ :: _) -> failwith "can't happen"
              
          | Tok0.RParen _ -> List.rev acc, (pos, tok) :: rest
          
          | Tok0.Late _
          | Tok0.Invalid _
          | Tok0.Eof _
          | Tok0.Spaces _ -> failwith "can't happen"
          
    let topGroup line =
      match group [] line with
        | res, [] -> res
        | res, (pos, Tok0.RParen c) :: _ ->
          err pos ("unmatched " + c.ToString())
        | _ -> failwith "can't happen"
        
    let rec block prevIndent indent (acc:list<Tok>) = function
      | ((0, (_, Tok0.Late fn) :: _) :: rest) ->
        block indent indent (rev_append (fn() :?> list<Tok>) acc) rest        
      | ((newIndent, line) :: rest) as all ->
        if newIndent = indent then
          block indent indent (rev_append (topGroup line) acc) rest
        elif newIndent > indent then
          if prevIndent <> indent then
            err (fst line.Head) ("expecting indentation to go back to " + indent.ToString() + " from previous " + prevIndent.ToString() + " but got " + newIndent.ToString())
          let res, rest = block newIndent newIndent [] all
          block newIndent indent (Tok.NewLine (fst line.Head) :: Tok.Block (fst line.Head, res) :: acc) rest
        else
          List.rev acc, all
      | [] -> List.rev acc, []
        
   
    match collectLines [] with
      | ((0, _) :: _) as all -> 
        let res, rest = block 0 0 [] all
        if not rest.IsEmpty then failwith "can't happen, input left"
        res
      | [] -> []
      | (k, l) :: _ -> err (fst l.Head) "first line of input should have no indentation"
   
  let fromFile filename getStream stream =     
    tokenize [filename] getStream (Lexing.LexBuffer<char>.FromTextReader stream)
    
  let fromString (text:string) =
    let getStream (pos, _) =
      raise (SyntaxError (pos, "#include not supported when parsing strings"))
    let chars = Array.create text.Length ' '
    text.CopyTo (0, chars, 0, chars.Length)
    tokenize ["-string-"] getStream (Lexing.LexBuffer<char>.FromChars chars)
