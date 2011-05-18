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

type LocalMailer() = 

  let amountOfSentMessages: int ref = ref 0

  let callbacks = new Dictionary<int, List<(unit -> unit)>>()

  let principals = new Dictionary<string, ITerm -> ITerm -> unit>()
  
  member lm.AddCallback (targetAmountOfMessages: int) (f: unit -> unit) =
    let found, fs = callbacks.TryGetValue targetAmountOfMessages
    if found then
      fs.Add f
    else
      callbacks.[targetAmountOfMessages] <- new List<_>([f])

  member lm.SetPrincipalInbox (ppalName: string) (inbox: ITerm -> ITerm -> unit) =
    principals.[ppalName] <- inbox

  member lm.Principals =
    [ for principal in principals.Keys -> principal ]

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
      