namespace Microsoft.Research.Dkal.Interfaces

open Microsoft.Research.Dkal.Ast

type IEngine =
  abstract member Start: unit -> unit
  abstract member Stop: unit -> unit

  abstract member Derive: target: MetaTerm -> tmpInfons: MetaTerm list -> bool
  abstract member Learn: MetaTerm -> bool
  abstract member AreConsistentActions: MetaTerm list -> bool
