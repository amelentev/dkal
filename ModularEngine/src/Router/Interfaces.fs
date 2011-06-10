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

/// Empty interface to be used by any type that is used to represent a principal
/// address/location. It can be a URL, a name, etc.
type IPrincipalAddress = interface end

/// Interface implemented by routing tables. A routing table knows the name of 
/// the current principal and his/her address. It also knows how to locate any
/// other principal. 
type IRoutingTable =
  /// Name of the current principal
  abstract member Me: string
  /// Address of the current princiapl
  abstract member MyAddress: IPrincipalAddress
  /// List of all kwown principals names
  abstract member Principals: string list
  /// Returns true only if the given principal name is known
  abstract member HasPrincipal: string -> bool
  /// Adds the given principal and address to the routing table
  abstract member AddPrincipal: string -> IPrincipalAddress -> bool
  /// Returns the principal address of the given known principal
  abstract member PrincipalAddress: ppal: string -> IPrincipalAddress

