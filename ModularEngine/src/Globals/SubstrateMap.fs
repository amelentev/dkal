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

namespace Microsoft.Research.Dkal.Globals

open Microsoft.Research.Dkal.Interfaces

open System.Collections.Generic

module SubstrateMap =
  let private substrates = new Dictionary<string, ISubstrate>()

  let AddSubstrate (s: ISubstrate) =
    for ns in s.Namespaces do
      substrates.[ns] <- s

  let GetSubstrate (ns: string) = 
    let found, substrate = substrates.TryGetValue ns
    if found then
      substrate
    else
      failwithf "No substrate found for namespace %O" ns

  