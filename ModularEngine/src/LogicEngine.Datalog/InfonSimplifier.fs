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

namespace Microsoft.Research.Dkal.LogicEngine.Datalog

open System
open System.Collections.Generic
open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.DkalBackends.Ast
open Microsoft.Z3

module InfonSimplifier =
    let simplifyArg(arg: ITerm) =
        match arg with
        | Const(c) -> ConstTerm(c.Value.ToString())
        | Var(v) -> VarTerm(v.Name)
        | _ -> failwith "Argument cannot be more complex than a constant or variable for Datalog engine"

    let simplifyPrincipal(ppalTerm: ITerm) =
        match ppalTerm with
        | PrincipalConstant(ppal) -> ConstTerm(ppal)
        | Var(ppal) -> VarTerm(ppal.Name)
        | _ -> failwith ("Term " + ppalTerm.ToString() + " does not denote a principal")

    let rec simplify(infon: ITerm) =
        match infon with
        | AsInfon(t) -> failwith "Attempting to simplify AsInfon ..."
        | EmptyInfon(t) -> failwith "Attempting to simplify EmptyInfon ..."
        | SaidInfon(ppal, t) -> SpeechFormula(simplifyPrincipal(ppal), SaidSpeech, simplify(t))
        | AndInfon(t) -> match t with
                         | [] -> failwith "AndInfon may not be composed of no infons"
                         | term :: [] -> simplify(term)
                         | term :: terms -> AndFormula(simplify(term), simplify(AndInfon(terms))) 
        | ImpliesInfon(a, b) -> ImpliesFormula(simplify(a), simplify(b))
        | Forall(var, term) -> simplify(term)
        | App(f, args) -> AtomFormula(f.Name, args |> Seq.map (fun t -> simplifyArg(t)) |> Seq.toList)
        | t -> failwith (String.Format("Translation not implemented {0}", t))

