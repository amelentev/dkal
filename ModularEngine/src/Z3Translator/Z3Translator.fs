﻿// *********************************************************
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

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Z3

open System
open System.Collections.Generic

type Z3Translator(ctx: Context, types: Z3TypeDefinition, rels: Dictionary<string, FuncDecl>) =
  let _ctx= ctx
  let _rels= rels
  let _types = types
  
  interface ITranslator with
    member translator.translate(term: ITerm) =
      match term with
        | PrincipalConstant(t) -> Z3Expr(_ctx.MkConst(t, Z3TypesUtil.getZ3TypeSort(_types.getZ3TypeForDkalType("Dkal.Principal"), _ctx))) :> ITranslatedExpr
        | Const(t) -> failwith "Const not implemented"
        | EmptyInfon(t) -> Z3Expr(_ctx.MkTrue()) :> ITranslatedExpr
        | AsInfon(t) -> (translator :> ITranslator).translate(EmptyInfon)   // assume it is already solved as true for the substitutions under Substrate
        | AndInfon(t) -> Z3Expr(_ctx.MkAnd(t |>
                                           List.map (fun x -> (translator :> ITranslator).translate(x).getUnderlyingExpr() :?> BoolExpr) |>
                                           List.toArray)) :> ITranslatedExpr
        | OrInfon(t) -> Z3Expr(_ctx.MkOr(t |>
                                           List.map (fun x -> (translator :> ITranslator).translate(x).getUnderlyingExpr() :?> BoolExpr) |>
                                           List.toArray)) :> ITranslatedExpr
        | ImpliesInfon(t) -> Z3Expr(_ctx.MkImplies((translator :> ITranslator).translate(fst t).getUnderlyingExpr() :?> BoolExpr,
                                                   (translator :> ITranslator).translate(snd t).getUnderlyingExpr() :?> BoolExpr))
                             :> ITranslatedExpr
        | App(t) -> Z3Expr(_ctx.MkApp(_rels.[(fst t).Name], (snd t) |>
                                                                    List.map (fun x -> (translator :> ITranslator).translate(x).getUnderlyingExpr() :?> Expr) |>
                                                                    List.toArray )) :> ITranslatedExpr
        | Var(t) ->
          Z3Expr(_ctx.MkConst(t.Name, Z3TypesUtil.getZ3TypeSort(_types.getZ3TypeForDkalType(t.Type.FullName), _ctx))) :> ITranslatedExpr
        | t -> failwith (String.Format("Translation not implemented {0}", t))