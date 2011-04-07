namespace Microsoft.Research.Dkal.Interfaces

open Microsoft.Research.Dkal.Ast

type IParser =
  abstract member ParseInfon: string -> MetaTerm
  abstract member ParseRule: string -> MetaTerm
  abstract member ParsePolicy: string -> Policy
  abstract member ParseSignature: string -> Signature
  abstract member ParseAssembly: string -> Assembly
