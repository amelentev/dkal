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

(* TODO: 
    - Both org1 and site1 delegate access under the condition that the requested patient number is 
        between the allocated range. These restrictions need to be encoded as a substratequery (in fact
        they don't really rely on data, so a Basic calculator-like substrate would suffice). I don't know
        how this was handled in the ML engine, so you will notice that there are some TODOs in Site1.fs and
        Org1.fs related to this issue. The keyMgr will not be able to honor requests until this is somehow fixed.
    - Both public and private keys are defined for each principal. They are only dummy keys for now, but the visibility
        is guaranteed: i.e., public keys are defined in Globals.fs; private keys are defined on each principal.
        However, keys are not currently used in any way.
    - Justification is not contemplated at all in this implementation.
*)

module Main
    open System.Threading

    open Basic
    open Comm
    open Globals
    open Org1
    open Site1
    open Phys1
    open KeyMgr

    do
        Org1.suscribe()
        Site1.suscribe()
        Phys1.suscribe()
        KeyMgr.suscribe()

        let t1 = Thread Org1.execute
        let t2 = Thread Site1.execute
        let t3 = Thread Phys1.execute
        let t4 = Thread KeyMgr.execute
        
        t1.Start()
        t2.Start()
        t3.Start()
        t4.Start()

        t1.Join()
        t2.Join()
        t3.Join()
        t4.Join()

