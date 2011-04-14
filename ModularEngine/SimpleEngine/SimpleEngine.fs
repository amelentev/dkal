namespace Microsoft.Research.Dkal.SimpleEngine

open System.Collections.Generic

open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Interfaces

type SimpleEngine() = 
  let knowledge = new HashSet<MetaTerm>()

  interface IEngine with
    member se.Start () = ()
    member se.Stop () = ()

    member se.Derive (target: MetaTerm) (tmpInfons: MetaTerm list) = 
      // TODO: implement
      match target with
      | AsInfon(True) -> true
      | target -> 
        knowledge.Contains target
    
    member se.Learn (infon: MetaTerm) = 
      printfn "learning"
      knowledge.Add infon

    member se.AreConsistentActions (actions: MetaTerm list) = 
      // TODO: implement
      true

