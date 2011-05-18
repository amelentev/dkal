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
module Microsoft.Research.Dkal.Substrate.Basic.Builders

  open Microsoft.Research.Dkal.Interfaces
  open Microsoft.Research.Dkal.Ast
  open Microsoft.Research.Dkal.Ast.Tree

  // Bool builders
  let AndBool (bools: ITerm list) = 
    App({ Name = BasicPrimitives.And; 
          RetType = Type.Boolean; 
          ArgsType = List.replicate bools.Length Type.Boolean;
          Identity = Some True }, bools)

  let OrBool (bools: ITerm list) = 
    App({ Name = BasicPrimitives.Or; 
          RetType = Type.Boolean; 
          ArgsType = List.replicate bools.Length Type.Boolean;
          Identity = Some False }, bools)
     
