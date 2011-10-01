(*
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
*)

module Comm
    open Microsoft.Research.Dkal.LogicEngine.ML.MLType
    open Basic

    type Message = { from: Principal; target: Principal; content: term }

    let mutable suscribers = []

    let suscribe (p: Principal) (f: Message -> unit) = 
        suscribers <- suscribers @ [(p,f)]

    let send (m: Message) =
        let _, targetF = List.find (fun (p,f) -> m.target = p) suscribers
        targetF m

