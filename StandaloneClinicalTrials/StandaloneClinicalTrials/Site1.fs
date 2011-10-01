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

module Site1
    open Microsoft.Research.Dkal.LogicEngine.ML.MLType
    open Basic
    open Comm
    open Globals

    let me: Principal = "site1"
    let myPrivKey: PrivKey = "<site1privkey>"

    let physAssignments = [ { phys="phys1"; trial=42; n1=1010; n2=1050 } ]

    let mutable _t: Trial option = None

    let receive (m: Message) =
        printfn "%O>> got from %O:\n%A\n" me m.from m.content
        match m.content with
        | App(RelationInfon(siteParticipates), [Const(PrincipalConstant(me)); Const(SubstrateConstant(t))]) ->
            // check that the message comes from the organizer of a trial
            try
                _t <- Some <| List.find (fun (t': Trial) -> t'.id.Equals(t) && t'.organizer = m.from) trials
            with
                e -> failwith "%O>> got message from a ppal which does not organize any trial" me
        | App(RelationInfon(siteAssignment), [Const(PrincipalConstant(me)); Const(SubstrateConstant(n1)); Const(SubstrateConstant(n2)); Const(SubstrateConstant(t))]) 
            when m.from = _t.Value.organizer && _t.Value.id.Equals(t) ->
                match n1 with
                | :? int as n1 -> 
                    match n2 with
                    | :? int as n2 -> 
                        // contact physicians for this trial with patients between n1 and n2 
                        for pa in physAssignments do
                            if pa.trial = _t.Value.id && n1 <= pa.n1 && pa.n2 <= n2 then
                                let i1 = rel(physParticipates, 
                                            [Const(PrincipalConstant(pa.phys)); 
                                                Const(SubstrateConstant(_t.Value.id)); 
                                                Const(PrincipalConstant(me))])
                                send({from=me; target=pa.phys; content=i1})
                                let i2 = rel(physAllocatedPatients, 
                                            [Const(PrincipalConstant(pa.phys)); 
                                                Const(SubstrateConstant(pa.n1)); 
                                                Const(SubstrateConstant(pa.n2)); 
                                                Const(SubstrateConstant(_t.Value.id)); 
                                                Const(PrincipalConstant(me))])
                                send({from=me; target=pa.phys; content=i2})
                                let patient = Var({ name="PATIENT"; typ=typ.Int32 })
                                let i3 =  App(ImpliesInfon, 
                                                [
                                                // TODO: how to include the query that pa.n1 <= patient && patient <= pa.n2
                                                App(AsInfon, [(*BasicSubstrateTerm(XXX)*)])
                                                // ->
                                                App(SaidInfon, [Const(PrincipalConstant(me)); rel(mayRead, [Const(PrincipalConstant(pa.phys)); patient; Const(SubstrateConstant(_t.Value.id))])])])
                                send({from=me; target=pa.phys; content=i3})
                    | _ -> failwithf "%O>> expecting an integer value for n2" me
                | _ -> failwithf "%O>> expecting an integer value for n1" me
        | App(ImpliesInfon, [_; App(SaidInfon, [Const(PrincipalConstant(org)); 
                                                App(RelationInfon(mayRead), [person; patient; Const(SubstrateConstant(t))])])])
            when m.from = _t.Value.organizer && m.from = org && _t.Value.id.Equals(t) ->
                // contact physicians and delegate this authorization from the organizer
                for pa in physAssignments do
                    if pa.trial = _t.Value.id then
                        send({from=me; target=pa.phys; content=m.content})
        | _ -> failwithf "%O>> got unrecognizable message" me

    let suscribe() =
        suscribe me receive

    let execute() =
        () 
