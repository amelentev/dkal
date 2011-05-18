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

open Microsoft.Research.Dkal.Ast

/// IPrincipalService provides an interface for exposing an inbox web service
/// that other principals can use to leave us messages.
[< ServiceContract() >]   
type IPrincipalService = 
  
  /// This operation is invoked by other principals when the want to leave us
  /// a message. 
  [<OperationContract>]
  abstract member ReceiveMessage : message: string * from: string * fromAddress: string -> unit
