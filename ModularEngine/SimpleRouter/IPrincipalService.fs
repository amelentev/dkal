namespace Microsoft.Research.Dkal.SimpleRouter

open System.ServiceModel
open System.Runtime.Serialization

open Microsoft.Research.Dkal.Ast

[< ServiceContract() >]   
type IPrincipalService = 
  [<OperationContract>]
  abstract member ReceiveMessage : message: string * from: string -> unit
