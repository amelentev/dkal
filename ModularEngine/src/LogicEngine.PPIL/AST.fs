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
  type Operation = 
      | AndOp
      | OrOp
      | ImpliedOp
      override this.ToString() =
         match this with
         | AndOp -> "&"
         | OrOp -> "|"
         | ImpliedOp -> "->"

  type Prefix = string list

  [<ReferenceEquality>]
  type AST =
      | Rel of (int*int*Prefix)*string // key*len*
      | Binary of (int*int*Prefix)*Operation*AST*AST
      member this.getKLP =
          match this with
          | Rel((k,l,p),_) -> (k,l,p)
          | Binary((k,l,p),_,_,_) -> (k,l,p)
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
      member this.Right = 
          match this with
          | Binary(_,_,l,r) -> r
          | _ -> failwith "Right on nonbinary"
      member this.Left = 
          match this with
          | Binary(_,_,l,r) -> l
          | _ -> failwith "Left on nonbinary"
        
      override this.ToString() =
          let pref = this.PrefStr
          match this with
          | Rel(_,s) -> 
              pref + s
          | Binary(_,op,l,r) ->
              pref + "(" + l.ToString() + op.ToString() + r.ToString()+")"
