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

namespace Microsoft.Research.Dkal.Router.Local

open System.Collections.Generic
open System.IO

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Router

/// A SimpleRoutingTable contains information about the current principal's name and
/// address as well as the names and addresses of all the other principals known
type LocalRoutingTable(me: string, mailer: LocalMailer) = 
  
  interface IRoutingTable with

    /// My name
    member rt.Me = me

    /// My address
    member rt.MyAddress = { new IPrincipalAddress }

    /// Returns the list of principal names that I know
    member rt.Principals =
      mailer.Principals

    /// Returns true if the principal name is known to the RoutingTable
    member rt.HasPrincipal (name: string) =
      mailer.Principals |> List.exists (fun e -> e = name)

    /// Adds the principal (with name and address) and returns true if the 
    /// principal was not present before addition
    member rt.AddPrincipal (name: string) (address: IPrincipalAddress) =
      failwithf "Local routing table does not support adding principals dynamically"

    /// Gets the principal address from the RoutingTable
    member rt.PrincipalAddress (name: string) =
      { new IPrincipalAddress }
  