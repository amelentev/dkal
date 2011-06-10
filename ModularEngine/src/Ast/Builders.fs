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

/// Defines the public interface on how to construct AST elements defined
/// in the Ast module
[<AutoOpen>]
module Microsoft.Research.Dkal.Ast.Builders

  open Microsoft.Research.Dkal.Interfaces

  /// Constructs a variable ITerm
  let Var (v: IVar) =
    v :> ITerm

  /// Constructs a constant ITerm
  let Const (c: IConst) =
    c :> ITerm
  
  /// Constructs a principal constant ITerm
  let Principal (ppal: string) = 
    Const(PrincipalConstant(ppal))

  /// Constructs a true literal ITerm
  let True = Const(Constant(true))

  /// Constructs a false literal ITerm
  let False = Const(Constant(false))

  /// Constructs a forall quantified ITerm with a single variable
  let Forall (var: IVar, t: ITerm) = 
    { Var = var; Term = t } :> ITerm

  /// Constructs a forall quantified ITerm with many variables
  let ForallMany (vars: IVar list, t: ITerm) = 
    List.foldBack (fun v i -> Forall(v,i)) vars t 
