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

module Phys1
    open System
    open Microsoft.Research.Dkal.LogicEngine.ML.MLType
    open Basic
    open Comm
    open Globals

    let me: Principal = "phys1"
    let myPrivKey: PrivKey = "<phys1privkey>"

    let mutable _t: int option = None
    let mutable _site: Principal option = None
    let mutable _n1: int option = None
    let mutable _n2: int option = None
    let mutable _delegations = []

    let receive (m: Message) =
        printfn "%O>> got from %O:\n%A\n" me m.from m.content
        match m.content with
        | App(RelationInfon(physParticipates), [Const(PrincipalConstant(me)); Const(SubstrateConstant(t)); Const(PrincipalConstant(site))]) 
            when m.from = site ->
                _site <- Some site
                match t with
                | :? int as t -> 
                    _t <- Some t
                | _ -> failwithf "%O>> expecting an integer value for trial id" me
        | App(RelationInfon(physAssignment), [Const(PrincipalConstant(me)); Const(SubstrateConstant(n1)); Const(SubstrateConstant(n2)); Const(SubstrateConstant(t)); Const(PrincipalConstant(site))]) 
            when site = _site.Value && site = m.from ->
                match n1 with
                | :? int as n1 -> 
                    _n1 <- Some n1
                    match n2 with
                    | :? int as n2 -> 
                        _n2 <- Some n2
                    | _ -> failwithf "%O>> expecting an integer value for n2" me
                | _ -> failwithf "%O>> expecting an integer value for n1" me
        | App(ImpliesInfon, _) ->
            _delegations <- _delegations @ [m.content]
        | App(RelationInfon(keyForRecord), [Const(SubstrateConstant(key)); Const(SubstrateConstant(patient)); Const(SubstrateConstant(trial))]) ->
            let record = List.find (fun (r: Record) -> r.patient.Equals(patient) && r.trial.Equals(trial)) records
            printfn "%O>> key to open record for patient %O in trial %O with cyphered data %O is %O" me patient trial record.data key
        | _ -> failwithf "%O>> got unrecognizable message" me

    let suscribe() =
        suscribe me receive

    let execute() = 
        let mutable input = "_"
        while input.Length > 0 do
            printfn "%O>> please enter the patient and trial numbers separated by a comma..." me
            printfn "%O>> or leave it blank to exit..." me
            input <- System.Console.ReadLine()
            if input.Length > 0 then
                let parts = input.Split(',')
                match parts with
                | [| p1; p2 |] -> 
                    let patient, trial = 
                        try 
                            Int32.Parse(p1.Trim()), Int32.Parse(p2.Trim())
                        with
                            e -> failwithf "%O>> expecting patient and trial integer numbers" me
                    if _t.IsNone || _n1.IsNone || _n2.IsNone then
                        failwithf "%O>> got request before site indicated trial information" me
                    let i1 = rel(requestToRead, [Const(PrincipalConstant(me)); Const(SubstrateConstant(patient)); Const(SubstrateConstant(trial))])
                    for i in _delegations do
                        send({from=me; target="keyMgr"; content=i})
                    send({from=me; target="keyMgr"; content=i1})
                | _ -> printfn "%O>> expecting exactly two arguments, separated by a comma" me


