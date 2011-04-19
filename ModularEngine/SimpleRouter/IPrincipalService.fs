namespace Microsoft.Research.Dkal.SimpleRouter

open System.ServiceModel
open System.Runtime.Serialization

open Microsoft.Research.Dkal.Ast

/// IPrincipalService provides an interface for exposing an inbox web service
/// that other principals can use to leave us messages.
[< ServiceContract() >]   
type IPrincipalService = 
  
  /// This operation is invoked by other principals when the want to leave us
  /// a message. They have to provide the (serialized) message and their name
  [<OperationContract>]
  abstract member ReceiveMessage : message: string * from: string -> unit
