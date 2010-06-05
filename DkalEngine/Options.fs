namespace Microsoft.Research.DkalEngine

open System
open System.IO
open System.Text
open Microsoft.FSharp.Text
open Microsoft.FSharp.Collections


type Options =
  {
    mutable PrivateSql : string
    mutable Trace : int
  }

  static member Create() = 
    { PrivateSql = ""; Trace = 0 }
