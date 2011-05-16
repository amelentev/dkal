namespace Microsoft.Research.Dkal.Interfaces

// Provides an interface for a repository of knowledge in the form of infon ITerms
type IInfostrate =
  
  /// Adds the given infon ITerm to the infostrate
  abstract member Learn: ITerm -> bool

  /// Removes the given infon ITerm from the infostrate
  abstract member Forget: ITerm -> bool

  /// Returns a sequence of knowledge from the infostrate 
  abstract member Knowledge: ITerm seq
