namespace Microsoft.Research.Dkal.Interfaces

open Microsoft.Research.Dkal.Ast

type IPrettyPrinter =
  abstract member PrintType: Type -> string
  abstract member PrintTerm: MetaTerm -> string
  abstract member PrintInfon: MetaTerm -> string
  abstract member PrintAssertion: Assertion -> string
  abstract member PrintPolicy: Policy -> string
  abstract member PrintSignature: Signature -> string
  abstract member PrintAssembly: Assembly -> string
