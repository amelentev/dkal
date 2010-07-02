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

//
// Misc utils
//
  
module Util =
  type Pos = 
    { 
      filename : string;
      line : int; 
      column : int 
    }
    
    override this.ToString() =
      this.filename + ":" + (this.line+1).ToString() + ":" + (this.column+1).ToString()
  
  let fakePos = { filename = ""; line = 0; column = 0 }
  
  exception SyntaxError of Pos * string
  


  let log (msg:string) = System.Console.WriteLine msg
  
  type Dict<'A,'B> = System.Collections.Generic.Dictionary<'A,'B>
  type Vec<'A> = System.Collections.Generic.List<'A>
  
  let dict() = new Dict<_,_>()
  let vec() = new Vec<_>()
  let l2s l = "[" + String.concat "; " (List.map (fun o -> o.ToString()) l) + "]"

  let tempStringBuilder f =
    let sb = StringBuilder ()
    f sb
    sb.ToString()

  let getDefl (dict:Dict<_,_>) k d =
    match dict.TryGetValue k with
      | true, r -> r
      | _ -> d
      
  let rec rev_append a b =
    match a with
      | x :: xs -> rev_append xs (x :: b)
      | [] -> b
