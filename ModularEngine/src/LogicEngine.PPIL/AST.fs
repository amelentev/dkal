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

module AST =
  type SetOperation = 
      | AndOp
      | OrOp
      override this.ToString() =
         match this with
         | AndOp -> "&"
         | OrOp -> "|"

  type Prefix = string list

  type AST =
      | Rel of (int*int*Prefix)*string // key*len*
      | SetFormula of (int*int*Prefix)*SetOperation*(AST list)
      | Implies of (int*int*Prefix)*AST*AST
      member this.getKLP =
          match this with
          | Rel((k,l,p),_) 
          | SetFormula((k,l,p),_,_) 
          | Implies((k,l,p),_,_) -> (k,l,p)
      member this.Key =
          let (k,_,_) = this.getKLP
          k
      member this.Length = 
          let (_,l,_) = this.getKLP
          l
      member this.Prefix =
          let (_,_,p) = this.getKLP
          p
      member this.PrefStr = this.Prefix |> List.map (fun s -> s + " said ") |> String.concat ""
        
      override this.ToString() =
          match this with
          | Rel(_,s) -> 
              this.PrefStr + s
          | SetFormula(_,op,args) ->
              this.PrefStr + "(" + (args |> List.map (fun a -> a.ToString()) |> String.concat (op.ToString())) + ")"
          | Implies(_,l,r) ->
              this.PrefStr + "(" + l.ToString() + "->" + r.ToString()+")"
