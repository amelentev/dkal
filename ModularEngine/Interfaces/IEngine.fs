namespace Microsoft.Research.Dkal.Interfaces

open Microsoft.Research.Dkal.Ast

type IEngine =
  abstract member Start: unit -> unit

  abstract member Receive: MetaTerm -> MetaTerm -> unit
  abstract member Send: (MetaTerm -> MetaTerm -> unit) -> unit

