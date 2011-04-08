namespace Microsoft.Research.Dkal

open Microsoft.Research.Dkal.Ast

type IEngine =
  abstract member Start: unit -> unit

  abstract member Receive: MetaTerm -> unit
  abstract member Send: (MetaTerm -> unit) -> unit

