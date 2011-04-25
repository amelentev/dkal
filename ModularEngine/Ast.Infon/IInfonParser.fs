namespace Microsoft.Research.Dkal.Ast.Infon

open Microsoft.Research.Dkal.Interfaces

/// IInfonParser provides an interface for top-level parsers that interpret policies
type IInfonParser =
  inherit IParser

  /// Parse an infon (ITerm) from the input string
  abstract member ParseInfon: string -> ITerm

  /// Parse a rule (ITerm) from the input string
  abstract member ParseRule: string -> ITerm

  /// Parse a Policy from the input string
  abstract member ParsePolicy: string -> Policy

  /// Parse a Signature from the input string
  abstract member ParseSignature: string -> Signature

  /// Parse an Assembly from the input string
  abstract member ParseAssembly: string -> Assembly
