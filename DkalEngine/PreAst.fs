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
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Text

open Microsoft.Research.DkalEngine.PreToken
open Microsoft.Research.DkalEngine.Util
open Microsoft.Research.DkalEngine.Ast

//
// This AST is used as an intermediate step in parsing/resolving.
//

module PreAst =
  let opening = function
    | ')' -> '('
    | ']' -> '['
    | '}' -> '{'
    | c -> c
  
  type Pat =
    | Id of string
    | Group of char
    | Block
    | App of string
    | NewLine
    | Any
    
    override this.ToString() =
      match this with
        | Id s -> "'" + s + "'"
        | Group c -> "Group" + c.ToString()
        | Block -> "Block"
        | NewLine -> "NL"
        | App s -> s + "(...)"
        | Any -> "*"
  
  type Tok =
    | Group of Pos * char * list<Tok>
    | Block of Pos * list<Tok>
    | NewLine of Pos
    | Int of Pos * int
    | Id of Pos * string  // non-capitalized
    | Var of Pos * string // capitalized
    | StringLiteral of Pos * string
    
    | App of Pos * string * list<Tok>
    
    member this.Pos =
      match this with
        | Group (p, _, _)
        | Block (p, _)
        | NewLine p
        | Int (p, _)
        | Id (p, _)
        | StringLiteral (p, _)
        | Var (p, _)
        | App (p, _, _) -> p
        
    
    override this.ToString() =
      let sb = new StringBuilder()
      let wr (s:obj) = sb.Append s |> ignore
      let rec pr l = function
        | Group (_, c, toks) ->
          wr (opening c)
          wr " "
          for t in toks do
            pr l t
          wr c
          wr " "
        | Block (_, toks) ->
          wr "{ "
          for t in toks do
            pr (l+2) t
          sb.Length <- sb.Length - 2
        | NewLine _ ->
          wr " NL\n"
          wr (new String (' ', l))
        | Int (_, i) -> wr i; wr " "
        | Var (_, s) -> wr "$"; wr s; wr " "
        | Id (_, s) -> wr s; wr " "
        | StringLiteral (_, s) -> wr "\""; wr s; wr "\""
        | App (_, p, args) ->
          wr p
          wr "("
          for t in args do
            pr l t
            wr ", "
          sb.Length <- sb.Length - 2
          wr ") "
          
      pr 0 this
      sb.ToString()

  
    
  type Rule =
    {
      name : string;
      pats : list<Pat>;
      body : list<Tok> -> list<Tok>;
      priority : int;
    }
  
  type Context =
    {
      options : Dict<string, string>;
      types : Dict<string, Type>;
      functions : Dict<string, Function>;
      rules : Dict<int, list<Rule>>;
      principals : Dict<string, Principal>;
      vars : Dict<string, Var>;
      mutable pendingFunctions : list<Function>;
      mutable id : int;
      mutable trace : int;
      mutable me : option<Principal>;
    }    
    
    member this.AddRule r =
      this.rules.[r.priority] <- r :: getDefl this.rules r.priority []
    
    member this.NextId () =
      this.id <- this.id + 1
      this.id
      
    member this.AddType name =
      this.types.Add (name, { id = this.NextId(); name = name })
    
    member this.AddPrincipal name =
      this.principals.Add (name, { name = name; internal_id = this.NextId(); typ = Type.Principal })
    
    member this.MkVar name tp =
      let id = this.NextId()
      let name =
        if name = "" then
          "anon#" + id.ToString()
        else name
      ({ name = name; id = id; typ = tp } : Var)
      
    member this.AddVar name =
      this.vars.Add (name, { name = name; id = this.NextId(); typ = Type.Unbound })
        
    static member Make() =
      let ctx = { options = dict(); types = dict(); functions = dict(); rules = dict(); principals = dict(); vars = dict(); id = 100; pendingFunctions = []; trace = 0; me = None }
      ctx.types.Add ("infon", Type.Infon)
      ctx.types.Add ("principal", Type.Principal)
      ctx.types.Add ("int", Type.Int)
      ctx.types.Add ("bool", Type.Bool)
      ctx.types.Add ("text", Type.Text)
      for k in Ast.globalFunctions.Keys do
        ctx.functions.Add (k, Ast.globalFunctions.[k])
      ctx
