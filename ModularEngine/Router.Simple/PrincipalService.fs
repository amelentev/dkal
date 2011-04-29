namespace Microsoft.Research.Dkal.Router.Simple

open System.ServiceModel
open System.Runtime.Serialization

/// PrincipalService implements the IPrincipalService interface and is 
/// initialized with a callback messageProcessingFunc that is invoked every
/// time a new message arrives. 
[<ServiceBehavior(Name="Microsoft.Research.Dkal.SimpleRouter.PrincipalService",InstanceContextMode=InstanceContextMode.Single)>]
type PrincipalService(messageProcessingFunc) =
  interface IPrincipalService with
    member ps.ReceiveMessage (message) =
      messageProcessingFunc message 
