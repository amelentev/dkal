namespace Microsoft.Research.Dkal.Interfaces

/// IEngine provides an interface for logic reasoning engines that handle
/// the infostrate
type IEngine =
  
  /// Must be called to initialize the engine
  abstract member Start: unit -> unit

  /// Must be called after the engine is no longer needed
  abstract member Stop: unit -> unit

  /// Given an infon ITerm with (possibly) free variables it returns all
  /// the Substitutions that make the infon hold
  abstract member Derive: ITerm -> ISubstitution list

  /// Adds the given infon ITerm to the knowledge base of the engine
  abstract member Learn: ITerm -> bool

  /// Removes the given infon ITerm from the knowledge base of the engine
  abstract member Forget: ITerm -> bool

