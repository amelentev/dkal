namespace Microsoft.Research.Dkal.Interfaces

/// IParser provides an interface for different parsers. All implementations
/// of IParser process input strings and return ITerms
type IParser =

  /// Parse an IType from the input string
  abstract member ParseType: string -> IType

  /// Parse an ITerm from the input string
  abstract member ParseTerm: string -> ITerm

