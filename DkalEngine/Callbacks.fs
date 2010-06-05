namespace Microsoft.Research.DkalEngine

open Microsoft.Research.DkalEngine.Util
open Microsoft.Research.DkalEngine.Ast

type Binding =
  {
    formal : Var
    actual : Term
  }

type ICommunicator =
  abstract PrincipalById : int -> Principal
  abstract PrincipalId : Principal -> int
  abstract SendMessage : Message -> unit
  abstract Knows : Knows -> unit
  abstract QueryResults : Infon * seq<seq<Binding>> -> unit
  abstract Warning : string -> unit
  abstract ExceptionHandler : System.Exception -> unit


