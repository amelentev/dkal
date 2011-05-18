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

[<AutoOpen>]
module Microsoft.Research.Dkal.Substrate.Basic.ActivePatterns

  open Microsoft.Research.Dkal.Interfaces
  open Microsoft.Research.Dkal.Ast.Tree
  open Microsoft.Research.Dkal.Ast

  // Bool patterns
  let (|AndBool|_|) mt =  match mt with
                          | App({Name=BasicPrimitives.And; RetType=Substrate(b)}, mts) when b = typeof<bool> -> Some mts
                          | _ -> None
  let (|OrBool|_|) mt = match mt with
                        | App({Name=BasicPrimitives.Or; RetType=Substrate(b)}, mts) when b = typeof<bool> -> Some mts
                        | _ -> None
