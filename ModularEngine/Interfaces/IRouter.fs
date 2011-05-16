namespace Microsoft.Research.Dkal.Interfaces

/// IRouter provides an interface for communication with the environment. 
/// Implementations of IRouter handle both incoming and outcoming messages
type IRouter =
  
  /// Returns the current principal name
  abstract member Me: string

  /// Returns the names of the known principals
  abstract member Roster: string list

  /// Must be called to initialize the router
  abstract member Start: unit -> unit

  /// Must be called after the router is no longer needed
  abstract member Stop: unit -> unit

  /// Sets a receive message handler. The function passed as argument is 
  /// going to be invoked by the router whenever a new message arrives. The
  /// first argument will be the received infon ITerm and the second one
  /// the principal constant ITerm that originated it
  abstract member Receive: (ITerm -> ITerm -> unit) -> unit

  /// Sends an infon ITerm to the target principal ITerm
  abstract member Send: infon: ITerm -> ppal: ITerm -> unit

