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
open System.Threading

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Router

/// A LocalMailer is a central hub used to communicate several principals
/// that run in the same computer. Each principal suscribes an inbox function,
/// which is invoked by the LocalMailer whenever a message for that principal
/// needs to be delivered
type LocalMailer() = 

  let amountOfSentMessages: int ref = ref 0

  let callbacks = new Dictionary<int, List<(unit -> unit)>>()

  let principals = new Dictionary<string, ITerm -> ITerm -> unit>()
  
  /// Adds a callback that will be invoked when the total number of exchanged
  /// messages among all principals reaches the specified target amount
  member lm.AddCallback (targetAmountOfMessages: int) (f: unit -> unit) =
    let found, fs = callbacks.TryGetValue targetAmountOfMessages
    if found then
      fs.Add f
    else
      callbacks.[targetAmountOfMessages] <- new List<_>([f])

  /// Adds a principal to the LocalMailer, specifying its callback inbox function
  /// that will be invoked passing the message as first argument and the sender
  /// principal as second argument
  member lm.SetPrincipalInbox (ppalName: string) (inbox: ITerm -> ITerm -> unit) =
    principals.[ppalName] <- inbox

  /// Returns the list of known principal names
  member lm.Principals =
    [ for principal in principals.Keys -> principal ]

  /// Sends the given message from the given principal (second argument) to the 
  /// given destination principal (third argument)
  member lm.SendMessage (msg: ITerm) (from: ITerm) (ppalName: string) =
    let found, inbox = principals.TryGetValue ppalName
    if found then
      inbox msg from
      let amount = Interlocked.Increment(amountOfSentMessages)
      lm.ExecuteCallbacks amount
    else
      failwithf "Unknown principal %O" ppalName

  member private lm.ExecuteCallbacks (amount: int) =
    let found, fs = callbacks.TryGetValue amount
    if found then
      Seq.iter (fun f -> f()) fs
      callbacks.Remove amount |> ignore
      