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

namespace Microsoft.Research.Dkal.MailBox.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.MailBox.Simple

/// The MailBoxFactory provides a factory to construct different mailboxes.
/// A mailbox kind and a logic engine to check for proof correctness must be provided.
type MailBoxFactory() =
  static member MailBox (kind: string) (logicEngine: ILogicEngine) = 
    match kind with
    | "simple" -> new SimpleMailBox(logicEngine) :> IMailBox
    | k -> failwith <| "Unrecognized mailbox kind: " + k


