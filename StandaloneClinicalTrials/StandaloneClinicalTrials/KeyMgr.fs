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

module KeyMgr
    open Microsoft.Research.Dkal.LogicEngine.ML.MLType
    open Microsoft.Research.Dkal.LogicEngine.ML.MLLogicEngineImpl
    open Microsoft.Research.Dkal.LogicEngine.ML.TranslationfromML
    open Microsoft.Research.Dkal.Infostrate.Factories
    open Basic
    open Comm
    open Globals

    let me: Principal = "keyMgr"
    let myPrivKey: PrivKey = "<keymgrprivkey>"

    let recordKeys = [  { patient=1005; trial=42; key="13" };
                        { patient=1015; trial=42; key="1313" };
                        { patient=1016; trial=42; key="131313" };
                        { patient=1100; trial=42; key="13131313" };
                     ]

    let infostrate = InfostrateFactory.Infostrate("simple")

    let receive (m: Message) =
        printfn "%O>> got from %O:\n%A\n" me m.from m.content
        match m.content with
        | App(ImpliesInfon, _) ->
            infostrate.Learn(ITermOfMLterm(m.content)) |> ignore
        | App(RelationInfon(requestToRead), [Const(PrincipalConstant(phys)); Const(SubstrateConstant(patient)); Const(SubstrateConstant(trial))]) 
            when m.from = phys -> 
                let canRead = rel(mayRead, [Const(PrincipalConstant(phys)); Const(SubstrateConstant(patient)); Const(SubstrateConstant(trial))])
                let result = derive (Some infostrate) canRead []
                printfn "%O>> query result is %A" me result
                if result.Length > 0 then
                    let key = 
                        try
                            List.find (fun (rk: RecordKey) -> rk.patient.Equals(patient) && rk.trial.Equals(trial)) recordKeys
                        with
                            e -> failwithf "%O>> requested patient was not found in that trial" me
                    let i = rel(keyForRecord, [Const(SubstrateConstant(key)); Const(SubstrateConstant(patient)); Const(SubstrateConstant(trial))])
                    send({from=me; target=m.from; content=i})
        | _ -> failwithf "%O>> got unrecognizable message" me

    let suscribe() =
        suscribe me receive

    let execute() = 
        ()
