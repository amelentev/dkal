namespace Microsoft.Research.Dkal.Ast.Infon

open Microsoft.Research.Dkal.Interfaces

/// IInfonPrettyPrinter provides an interface to print top-level AST elements
type IInfonPrettyPrinter =
  inherit IPrettyPrinter

  /// Returns a string representation for the given Policy
  abstract member PrintPolicy: Policy -> string

  /// Returns a string representation for the given Signature
  abstract member PrintSignature: Signature -> string

  /// Returns a string representation for the given Assembly
  abstract member PrintAssembly: Assembly -> string
