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

module Basic
    open Microsoft.Research.Dkal.LogicEngine.ML.MLType

    type Principal = string

    type PubKey = string
    type PrivKey = string

    let siteParticipates = 
        // SITE: Principal, T: Trial
        { name="siteParticipates"; retType=typ.Infon; argsType=[typ.Principal; typ.Int32]; identity=None }
    let siteAllocatedPatients = 
        // SITE: Principal, N1: Int, N2: Int, T: Trial
        { name="siteAllocatedPatients"; retType=typ.Infon; argsType=[typ.Principal; typ.Int32; typ.Int32; typ.Int32]; identity=None }
    let physParticipates = 
        // PHYS: Principal, T: Trial, SITE: Principal
        { name="physParticipates"; retType=typ.Infon; argsType=[typ.Principal; typ.Int32; typ.Principal]; identity=None }
    let physAllocatedPatients = 
        // PHYS: Principal, N1: Int, N2: Int, T: Trial, SITE: Principal
        { name="physAllocatedPatients"; retType=typ.Infon; argsType=[typ.Principal; typ.Int32; typ.Int32; typ.Int32; typ.Principal]; identity=None }
    let requestToRead = 
        // PERSON: Principal, P: Patient, T: Trial
        { name="requestToRead"; retType=typ.Infon; argsType=[typ.Principal; typ.Int32; typ.Int32]; identity=None }
    let mayRead = 
        // PERSON: Principal, P: Patient, T: Trial
        { name="mayRead"; retType=typ.Infon; argsType=[typ.Principal; typ.Int32; typ.Int32]; identity=None }
    let keyForRecord = 
        // K: String, P: Patient, T: Trial
        { name="keyForRecord"; retType=typ.Infon; argsType=[typ.String; typ.Int32; typ.Int32]; identity=None }

    let rel (r, args) = 
        App(RelationInfon(r), args)

    type Record = 
        { patient: int; trial: int; data: string } 
    type Trial = 
        { id: int; organizer: Principal; name: string }
    type SiteAssignment = 
        { site: Principal; trial: int; n1: int; n2: int }
    type PhysAssignment =
        { phys: Principal; trial: int; n1: int; n2: int }
    type RecordKey = 
        { patient: int; trial: int; key: string }



