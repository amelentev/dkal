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

namespace Microsoft.Research.Dkal.Ast.Translations.Z3Translator

open System
open System.Collections.Generic

open NLog

open Microsoft.Z3

type Z3Type = Bool | Int | Real | BitVec of uint32 | Array of Z3Type * Z3Type | Uninterpreted of string

type Z3TypesUtil() =
  static member z3TypeToString(typ) =
    match typ with
    | Bool -> "Bool"
    | Int -> "Int"
    | Real -> "Real"
    | BitVec(n) -> String.Format("_ BitVec {0}", n)
    | Array(dom, ran) -> "(Array " + Z3TypesUtil.z3TypeToString(dom) + " " + Z3TypesUtil.z3TypeToString(ran) + ")"
    | Uninterpreted(name) -> name

  static member getZ3TypeSort(typ, ctx: Context) =
    match typ with
    | Bool -> ctx.MkBoolSort() :> Sort
    | Int -> ctx.MkIntSort() :> Sort
    | Real -> ctx.MkRealSort() :> Sort
    | BitVec(n) -> ctx.MkBitVecSort(n) :> Sort
    | Array(dom, ran) -> ctx.MkArraySort(Z3TypesUtil.getZ3TypeSort(dom, ctx), Z3TypesUtil.getZ3TypeSort(ran, ctx)) :> Sort
    | Uninterpreted(name) -> ctx.MkUninterpretedSort(name) :> Sort


type Z3TypeDefinition() =
  let _typeDefs= Dictionary<string, Z3Type>()
  let log = LogManager.GetLogger("LogicEngine.UFOL")
  do
    _typeDefs.Add("Dkal.Principal", Uninterpreted("Dkal.Principal"))
    _typeDefs.Add("System.String", Uninterpreted("System.String"))
    _typeDefs.Add("Dkal.Rule", Uninterpreted("Dkal.Rule"))
    _typeDefs.Add("System.DateTime", Uninterpreted("System.String"))
    _typeDefs.Add("System.Int32", Int)

  member z3types.setZ3TypeForDkalType(dkalType, z3Type) =
    _typeDefs.Add(dkalType, z3Type)

  member z3types.getZ3TypeForDkalType(dkalType) =
    try 
      _typeDefs.[dkalType]
    with
      | e -> log.Error("Unknown type {0}", dkalType); failwith "Unknown type"
      
