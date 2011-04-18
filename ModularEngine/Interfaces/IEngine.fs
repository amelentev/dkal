namespace Microsoft.Research.Dkal.Interfaces

open Microsoft.Research.Dkal.Ast

type IEngine =
  abstract member Start: unit -> unit
  abstract member Stop: unit -> unit

  abstract member Knowledge: unit -> MetaTerm list
  abstract member Derive: target: MetaTerm -> (Substitution * MetaTerm list) list
  abstract member Learn: MetaTerm -> bool
  abstract member AreConsistentActions: MetaTerm list -> bool
