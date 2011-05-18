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

namespace Microsoft.Research.Dkal.Interfaces

open System.Collections.Generic

type ISubstrateParser =

  abstract SetParsingContext: IParsingContext -> unit
  abstract SetNamespace: string -> unit
  abstract SetSubstrate: ISubstrate -> unit

  abstract ParseTerm: string -> ISubstrateTerm
