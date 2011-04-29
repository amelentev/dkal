namespace Microsoft.Research.Dkal.Interfaces

/// IEngine provides an interface for logic reasoning engines that handle
/// the infostrate
type IEngine =
  
  /// Must be called to initialize the engine
  abstract member Start: unit -> unit

  /// Must be called after the engine is no longer needed
  abstract member Stop: unit -> unit

  /// Given an infon ITerm with (possibly) free variables and an initial list
  /// of substitutions it returns all those (possibly specialized) substitutions
  /// that make the infon hold
  abstract member Derive: ITerm * ISubstitution list -> ISubstitution list

  /// Adds the given infon ITerm to the knowledge base of the engine
  abstract member Learn: ITerm -> bool

  /// Removes the given infon ITerm from the knowledge base of the engine
  abstract member Forget: ITerm -> bool

