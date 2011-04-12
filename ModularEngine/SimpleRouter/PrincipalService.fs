namespace Microsoft.Research.Dkal.SimpleRouter

open System.ServiceModel
open System.Runtime.Serialization

[<ServiceBehavior(Name="Microsoft.Research.Dkal.SimpleRouter.PrincipalService",InstanceContextMode=InstanceContextMode.Single)>]
type PrincipalService(messageProcessingFunc) =
  interface IPrincipalService with
    member ps.ReceiveMessage (message, from) =
      messageProcessingFunc message from
