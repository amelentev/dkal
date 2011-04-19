namespace Microsoft.Research.Dkal.Interfaces

open Microsoft.Research.Dkal.Ast

/// IRouter provides an interface for communication with the environment. 
/// Implementations of IRouter handle both incoming and outcoming messages
type IRouter =
  
  /// Must be called to initialize the router
  abstract member Start: unit -> unit

  /// Must be called after the router is no longer needed
  abstract member Stop: unit -> unit

  /// Sets a receive message handler. The function passed as argument is 
  /// going to be invoked by the router whenever a new message arrives. The
  /// first argument will be the received infon MetaTerm and the second 
  /// argument will be the sender principal MetaTerm
  abstract member Receive: (MetaTerm -> MetaTerm -> unit) -> unit

  /// Sends an infon MetaTerm to the target principal MetaTerm
  abstract member Send: infon: MetaTerm -> ppal: MetaTerm -> unit


