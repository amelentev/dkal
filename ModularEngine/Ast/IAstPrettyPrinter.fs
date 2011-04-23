namespace Microsoft.Research.Dkal.Ast

open Microsoft.Research.Dkal.Interfaces

/// IAstPrettyPrinter provides an interface to print top-level AST elements
type IAstPrettyPrinter =
  inherit IPrettyPrinter

  /// Returns a string representation for the given Policy
  abstract member PrintPolicy: Policy -> string

  /// Returns a string representation for the given Signature
  abstract member PrintSignature: Signature -> string

  /// Returns a string representation for the given Assembly
  abstract member PrintAssembly: Assembly -> string
