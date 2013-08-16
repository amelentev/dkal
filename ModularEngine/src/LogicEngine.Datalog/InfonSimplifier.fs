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
open Microsoft.Research.DkalBackends.DatalogBackend.DatalogTranslator.Datalog

module Seq =
    // useful extension method for folding while keeping the index at hand
    let public foldi foldfun accum sequence =
        sequence
        |> Seq.fold (fun (acc, i) elem -> (foldfun i acc elem, i+1)) (accum, 0)
        |> fst

module InfonSimplifier =
    let FAKE_TRUE_RELATION= "FakeTrue"

    let relationToBoolExpr(rel: Relation, regRels: Dictionary<string, FuncDecl>, regConsts: Dictionary<Microsoft.Z3.Sort, Dictionary<Expr, uint32>>, varDefs: Dictionary<string, Expr>, ctx: Context) =
        let dom= regRels.[rel.Name].Domain
        ctx.MkApp (regRels.[rel.Name], rel.Args |> Seq.mapi ( fun i arg -> match arg with
                                                                           | VarTerm(s) -> varDefs.[s]
                                                                           | AtomTerm(s) -> ctx.MkNumeral(regConsts.[dom.[i]].[ctx.MkConst(s, dom.[i])], dom.[i])
                                                                           | WildcardTerm -> failwith "Wildcard shouldn't appear in rule"
                                                            ) |> Seq.toArray
                  ) :?> BoolExpr

    let simplifyArg(arg: ITerm) =
        match arg with
        | Const(c) -> ConstTerm(c.Value.ToString())
        | Var(v) -> Microsoft.Research.DkalBackends.Ast.VarTerm(v.Name)
        | _ -> failwith "Argument cannot be more complex than a constant or variable for Datalog engine"

    let simplifyPrincipal(ppalTerm: ITerm) =
        match ppalTerm with
        | PrincipalConstant(ppal) -> ConstTerm(ppal)
        | Var(ppal) -> Microsoft.Research.DkalBackends.Ast.VarTerm(ppal.Name)
        | _ -> failwith ("Term " + ppalTerm.ToString() + " does not denote a principal")

    let rec simplify(infon: ITerm) =
        match infon with
        | AsInfon(t) -> AtomFormula(FAKE_TRUE_RELATION, [])
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


    let relationRuleToFact(relationRule: Relation, regRels: Dictionary<string, FuncDecl>, regConsts: Dictionary<Expr, uint32>, varDefs: Dictionary<string, Expr>, ctx: Context) =
        let dom= regRels.[relationRule.Name].Domain
        ( regRels.[relationRule.Name], relationRule.Args
                                           |> Seq.mapi ( fun i arg -> match arg with
                                                                      | VarTerm(s) -> varDefs.[s]
                                                                      | AtomTerm(s) -> ctx.MkConst(s, dom.[i])
                                                                      | WildcardTerm -> failwith "Wildcard shouldn't appear in rule"
                                                       )
                                           |> Seq.map (fun expr -> regConsts.[expr]) |> Seq.toArray
        )

    let impliesRuleToBoolExpr(head: Relation, body: Relation list, regRels: Dictionary<string, FuncDecl>, regConsts: Dictionary<Microsoft.Z3.Sort, Dictionary<Expr, uint32>>, varDefs: Dictionary<string, Expr>, ctx: Context) =
        let premise= ctx.MkAnd(body |> Seq.fold ( fun acc rel -> acc |> Array.append [| relationToBoolExpr(rel, regRels, regConsts, varDefs, ctx) |] ) [||])
        let dom= regRels.[head.Name].Domain
        let consq= relationToBoolExpr(head, regRels, regConsts, varDefs, ctx)
        ctx.MkImplies(premise, consq)

    let rec rulePartToBoolExpr(rp: ProgramRulePart, regRels: Dictionary<string, FuncDecl>, regConsts:Dictionary<Microsoft.Z3.Sort, Dictionary<Expr, uint32>>, varDefs: Dictionary<string, Expr>, ctx: Context) =
        match rp with
        | RulePart(AtomRule(relationRule)) -> relationToBoolExpr(relationRule, regRels, regConsts, varDefs, ctx)
        | RulePart(ImpliesRule(cons, prems)) ->
                                                let p= ctx.MkAnd(prems |> Seq.map (fun prem -> relationToBoolExpr(prem, regRels, regConsts, varDefs, ctx)) |> Seq.toArray)
                                                let q= relationToBoolExpr(cons, regRels, regConsts, varDefs, ctx)
                                                let res= ctx.MkImplies(p, q)
                                                res
        | _ -> failwith "Cannot make a query from anything but a relation rule"
