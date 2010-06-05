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

  let getDefl (dict:Dict<_,_>) k d =
    match dict.TryGetValue k with
      | true, r -> r
      | _ -> d
      
  let rec rev_append a b =
    match a with
      | x :: xs -> rev_append xs (x :: b)
      | [] -> b
