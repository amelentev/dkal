namespace Microsoft.Research.Dkal.Interfaces

open Microsoft.Research.Dkal.Ast

/// IPrettyPrinter provides an interface to print the AST elements back into
/// different concrete syntaxes. An IPrettyPrinter implementation should be
/// bound to an IParser implementation that can parse the output back into
/// MetaTerms (or Policies, etc.)
type IPrettyPrinter =

  /// Returns a string representation for the given Type
  abstract member PrintType: Type -> string

  /// Returns a string representation for the given MetaTerm (infon, rule, etc.)
  abstract member PrintMetaTerm: MetaTerm -> string

  /// Returns a string representation for the given Policy
  abstract member PrintPolicy: Policy -> string

  /// Returns a string representation for the given Signature
  abstract member PrintSignature: Signature -> string

  /// Returns a string representation for the given Assembly
  abstract member PrintAssembly: Assembly -> string
