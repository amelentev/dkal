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

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast.Infon
open System.Collections.Generic

module AST =
  type SetOperation = 
      | AndOp
      | OrOp
      override this.ToString() =
         match this with
         | AndOp -> "&"
         | OrOp -> "|"

  type Prefix = string list

  type ASTCommon = {
    key: int;
    len: int;
    pref: Prefix;
    orig: ITerm
  }
  type AST =
      | Rel of ASTCommon*string
      | SetFormula of ASTCommon*SetOperation*(AST list)
      | Implies of ASTCommon*AST*AST
      member this.Common =
          match this with
          | Rel(c,_) 
          | SetFormula(c,_,_) 
          | Implies(c,_,_) -> c
      member this.Key = this.Common.key
      member this.Length = this.Common.len
      member this.Prefix = this.Common.pref
      member this.PrefStr = this.Prefix |> List.map (fun s -> s + ":") |> String.concat ""
      member this.ChildrenCount = 
        match this with
        | Rel(_) -> 0
        | SetFormula(_,_,args) -> List.length args
        | Implies(_) -> 2

      override this.ToString() =
          match this with
          | Rel(_,s) -> 
              this.PrefStr + (if s.EndsWith("()") then s.Remove(s.Length-2, 2) else s)
          | SetFormula(_,op,args) ->
              this.PrefStr + "(" + (args |> List.map (fun a -> a.ToString()) |> String.concat (op.ToString())) + ")"
          | Implies(_,l,r) ->
              this.PrefStr + "(" + l.ToString() + "->" + r.ToString()+")"

  type Proof =
    | Hypothesis of AST
    | ConjElimination of Proof*AST
    | ConjIntroduction of (Proof list)*AST
    | DisjIntroduction of Proof*AST
    | ImplicationElimination of Proof*Proof*AST
    | ImplicationIntroduction of Proof*AST

  type ASTMap = IDictionary<int, AST>

  let trueLabel = Primitives.EmptyInfon