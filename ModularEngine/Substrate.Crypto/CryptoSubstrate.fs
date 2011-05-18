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

namespace Microsoft.Research.Dkal.Substrate

open Microsoft.Research.Dkal.Substrate.FSharp

// TODO 
// * actually add cryptographic functionallity
// * add more arithmetic functions
module Crypto =

  let CryptoSubstrate() = 
    let x = new FSharpSubstrate(["Crypto,SimpleIntArithmetic"])
    do x.Add "plus" (fun (x:int) (y:int) -> x + y)
    do x.Add "minus" (fun (x:int) (y:int) -> x - y)
    do x.Add "mult" (fun (x:int) (y:int) -> x * y)
    do x.Add "eq" (fun (x:int) (y:int) -> x = y)
    x


