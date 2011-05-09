namespace Microsoft.Research.Dkal.Router.Simple

open System.ServiceModel
open System.Runtime.Serialization

open Microsoft.Research.Dkal.Ast

/// IPrincipalService provides an interface for exposing an inbox web service
/// that other principals can use to leave us messages.
[< ServiceContract() >]   
type IPrincipalService = 
  
  /// This operation is invoked by other principals when the want to leave us
  /// a message. 
  [<OperationContract>]
  abstract member ReceiveMessage : message: string * from: string * fromAddress: string -> unit
