// *********************************************************
//
//    Copyright (c) Microsoft. All rights reserved.
//    This code is licensed under the Apache License, Version 2.0.
//    THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
//    ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
//    IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
//    PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.
//
// *********************************************************

namespace Microsoft.Research.Dkal.Router.Simple

open System.ServiceModel
open System.Runtime.Serialization

/// PrincipalService implements the IPrincipalService interface and is 
/// initialized with a callback messageProcessingFunc that is invoked every
/// time a new message arrives. 
[<ServiceBehavior(Name="Microsoft.Research.Dkal.SimpleRouter.PrincipalService",InstanceContextMode=InstanceContextMode.Single)>]
type PrincipalService(messageProcessingFunc) =
  interface IPrincipalService with
    member ps.ReceiveMessage(message, from, fromAddress) =
      messageProcessingFunc message from fromAddress
