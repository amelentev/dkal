namespace Microsoft.Research.Dkal.Router.Local

open System.Collections.Generic
open System.IO

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Router

type LocalMailer() = 

  let principals = new Dictionary<string, ITerm -> unit>()
  
  member lm.SetPrincipalInbox (ppalName: string) (inbox: ITerm -> unit) =
    principals.[ppalName] <- inbox

  member lm.Principals =
    [ for principal in principals.Keys -> principal ]

  member lm.SendMessage (msg: ITerm) (ppalName: string) =
    let found, inbox = principals.TryGetValue ppalName
    if found then
      inbox msg
    else
      failwithf "Unknown principal %O" ppalName
