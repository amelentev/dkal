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
module Microsoft.Research.Dkal.Ast.Builders

  open Microsoft.Research.Dkal.Interfaces

  // Variables
  let Var (v: IVar) =
    v :> ITerm

  // Constants
  let Const (c: IConst) =
    c :> ITerm
  
  // Literal builders
  let Principal (ppal: string) = 
    Const(PrincipalConstant(ppal))

  let True = Const(Constant(true))
  let False = Const(Constant(false))

  // Quantified builders
  let Forall (var: IVar, t: ITerm) = 
    { Var = var; Term = t } :> ITerm

  let ForallMany (vars: IVar list, t: ITerm) = 
    List.foldBack (fun v i -> Forall(v,i)) vars t 
