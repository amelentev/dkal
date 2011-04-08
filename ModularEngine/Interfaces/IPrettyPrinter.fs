namespace Microsoft.Research.Dkal.Interfaces

open Microsoft.Research.Dkal.Ast

type IPrettyPrinter =
  abstract member PrintType: Type -> string
  abstract member PrintMetaTerm: MetaTerm -> string
  abstract member PrintPolicy: Policy -> string
  abstract member PrintSignature: Signature -> string
  abstract member PrintAssembly: Assembly -> string
