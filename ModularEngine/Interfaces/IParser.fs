namespace Microsoft.Research.Dkal.Interfaces

open Microsoft.Research.Dkal.Ast

/// IParser provides an interface for different parsers. All implementations
/// of IParser process input strings and return AST elements (MetaTerms or
/// Policies, etc.)
type IParser =

  /// Parse an infon (MetaTerm) from the input string
  abstract member ParseInfon: string -> MetaTerm

  /// Parse a rule (MetaTerm) from the input string
  abstract member ParseRule: string -> MetaTerm

  /// Parse a Policy from the input string
  abstract member ParsePolicy: string -> Policy

  /// Parse a Signature from the input string
  abstract member ParseSignature: string -> Signature

  /// Parse an Assembly from the input string
  abstract member ParseAssembly: string -> Assembly
