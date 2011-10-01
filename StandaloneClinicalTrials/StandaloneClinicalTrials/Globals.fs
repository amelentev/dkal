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

module Globals
    open Basic

    let records = [ { patient=1005; trial=42; data="ala ma kota" };
                    { patient=1015; trial=42; data="kot ma ale" };
                    { patient=1016; trial=42; data="abecadlo" };
                    { patient=1100; trial=42; data="z pieca spadlo" };
                  ]

    let trials = [  { id=42; organizer="org1"; name="highly classified trial" } ]

    let principalPubKey (p: Principal) : PubKey = 
        match p with
        | "org1" -> "<org1pubkey>"
        | "site1" -> "<site1pubkey>"
        | "phys1" -> "<phys1pubkey>"
        | "keyMgr" -> "<keymgrpubkey>"
        | _ -> failwith "Principal not found"

