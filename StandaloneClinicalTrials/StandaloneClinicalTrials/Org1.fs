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

module Org1
    open Microsoft.Research.Dkal.LogicEngine.ML.MLType
    open Basic
    open Comm
    open Globals

    let me: Principal = "org1"
    let myPrivKey: PrivKey = "<org1privkey>"

    let siteAssignments = [ { site="site1"; trial=42; n1=1000; n2=1250 } ]

    let receive (m: Message) =
        printfn "%O>> got from %O:\n%A\n" me m.from m.content

    let suscribe() =
        suscribe me receive

    let execute() = 
        // for each trial organized by me, inform the sites about their assignments
        let myTrials = List.filter (fun (t: Trial) -> t.organizer = me) trials
        for t in myTrials do
            let relevantSiteAssignments = List.filter (fun (s: SiteAssignment) -> s.trial = t.id) siteAssignments
            for s in relevantSiteAssignments do
                // inform site that it participates in the trial
                let i1 = rel(siteParticipates, 
                                [Const(PrincipalConstant(s.site)); Const(SubstrateConstant(t.id))])
                send({from=me; target=s.site; content=i1})
                let i2 = rel(siteAllocatedPatients, 
                                [Const(PrincipalConstant(s.site)); Const(SubstrateConstant(s.n1)); Const(SubstrateConstant(s.n2)); Const(SubstrateConstant(t.id))])
                send({from=me; target=s.site; content=i2})
                let person = Var({ name="PERSON"; typ=typ.Principal })
                let patient = Var({ name="PATIENT"; typ=typ.Int32 })
                let i3 = App(ImpliesInfon, 
                                [App(AndInfon, [    App(SaidInfon, [Const(PrincipalConstant(s.site)); rel(mayRead, [person; patient; Const(SubstrateConstant(t.id))])]);
                                                    // TODO: how to include the query that s.n1 <= patient && patient <= s.n2
                                                    App(AsInfon, [(*BasicSubstrateTerm(XXX)*)])
                                                ]);
                                // ->
                                App(SaidInfon, [Const(PrincipalConstant(me)); rel(mayRead, [person; patient; Const(SubstrateConstant(t.id))])])])
                send({from=me; target=s.site; content=i3})



