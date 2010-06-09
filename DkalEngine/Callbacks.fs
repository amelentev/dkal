namespace Microsoft.Research.DkalEngine

open Microsoft.Research.DkalEngine.Util
open Microsoft.Research.DkalEngine.Ast

/// Gives an actual value for a variable.
type Binding =
  {
    formal : Var
    actual : Term
  }

/// Callbacks passed to every async method of Engine.
type ICommunicator =
  /// Get a principal corresponding to the id used in the database.
  abstract PrincipalById : int -> Principal
  /// Get a database id of named principal.
  abstract PrincipalId : Principal -> int
  /// Called when the engine needs to send a message. It should put the message the sending queue.
  abstract SendMessage : Message -> unit
  /// Called when the engine learns a new fact from communication.
  abstract Knows : Knows -> unit
  /// Called when results of an Ask query are available. It also provides the infon which was asked for.
  abstract QueryResults : Infon * seq<seq<Binding>> -> unit
  /// Called when some extraordinary condition arise (i.e., malformed communication arrives).
  abstract Warning : string -> unit
  /// Called when there is an exception in execution of the current task.
  abstract ExceptionHandler : System.Exception -> unit


