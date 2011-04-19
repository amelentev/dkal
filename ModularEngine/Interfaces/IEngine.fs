namespace Microsoft.Research.Dkal.Interfaces

open Microsoft.Research.Dkal.Ast

/// IEngine provides an interface for logic reasoning engines that handle
/// the infostrate
type IEngine =
  
  /// Must be called to initialize the engine
  abstract member Start: unit -> unit

  /// Must be called after the engine is no longer needed
  abstract member Stop: unit -> unit

  /// Given an infon MetaTerm with (possibly) free variables it returns all
  /// the Substitutions that make the infon hold
  abstract member Derive: MetaTerm -> Substitution list

  /// Adds the given infon MetaTerm to the knowledge base of the engine
  abstract member Learn: MetaTerm -> bool

  /// Removes the given infon MetaTerm from the knowledge base of the engine
  abstract member Forget: MetaTerm -> bool
