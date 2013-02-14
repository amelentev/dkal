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

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast

type ASubstrateParser(parsefun) =
  let mutable Namespace = ""
  let mutable Context: Option<IParsingContext> = None
  let mutable Substrate: Option<ISubstrate> = None
  interface ISubstrateParser with
    member x.SetParsingContext (context: IParsingContext) = 
      Context <- Some context
    member x.SetNamespace (ns: string) = 
      Namespace <- ns
    member x.SetSubstrate (substrate: ISubstrate) =
      Substrate <- Some substrate
    member x.ParseTerm squery = parsefun squery Context.Value Namespace Substrate.Value