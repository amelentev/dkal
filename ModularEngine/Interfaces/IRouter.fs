namespace Microsoft.Research.Dkal.Interfaces

open Microsoft.Research.Dkal.Ast

type IRouter =
  abstract member Receive: (MetaTerm -> MetaTerm -> unit) -> unit
  abstract member Send: infon: MetaTerm -> ppal: MetaTerm -> unit

  abstract member Start: unit -> unit
  abstract member Stop: unit -> unit

