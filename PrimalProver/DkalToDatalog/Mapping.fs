namespace Microsoft.Research.Dkal2Datalog

open System.Collections.Generic
open System.IO

module Mapping = 

 type Mapping<'t when 't: equality>() = 
    let t2i = new List<'t>()
    
    member s.Count = t2i.Count

    member s.Add (i: 't) = 
      if not (t2i.Contains i) then
        t2i.Add(i)
  
    override s.ToString() =
      let mutable ret = ""
      for index in [0 .. t2i.Count-1] do
        ret <- ret + "# " + index.ToString() + ": " + t2i.[index].ToString() + "\n"
      ret

    member s.ToMapFile(f: string) =
      let mutable ret = ""
      for index in [0 .. t2i.Count-1] do
        ret <- ret + t2i.[index].ToString() + "\n"
      File.WriteAllText(f, ret)
