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

    /// Gets the principal address from the RoutingTable
    member rt.PrincipalAddress (name: string) =
      { new IPrincipalAddress }
  