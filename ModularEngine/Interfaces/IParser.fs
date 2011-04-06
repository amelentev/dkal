namespace Microsoft.Research.Dkal.Interfaces

open Microsoft.Research.Dkal.Ast

type IParser =
  abstract member ParseTerm: string -> MetaTerm
  abstract member ParseInfon: string -> MetaTerm
  abstract member ParseAssertion: string -> Assertion
  abstract member ParsePolicy: string -> Policy
  abstract member ParseSignature: string -> Signature
  abstract member ParseAssembly: string -> Assembly
