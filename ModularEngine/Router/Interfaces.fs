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

namespace Microsoft.Research.Dkal.Router

type IPrincipalAddress = interface end

type IRoutingTable =
  abstract member Me: string
  abstract member MyAddress: IPrincipalAddress
  abstract member Principals: string list
  abstract member HasPrincipal: string -> bool
  abstract member AddPrincipal: string -> IPrincipalAddress -> bool
  abstract member PrincipalAddress: ppal: string -> IPrincipalAddress

