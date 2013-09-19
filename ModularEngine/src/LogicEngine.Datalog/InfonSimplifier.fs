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
    let FAKE_TRUE_FACT= AtomFormula(FAKE_TRUE_RELATION, [])

    type InfonSimplifier()= 
        member is.relationToBoolExpr(rel: Relation, regRels: Dictionary<string, FuncDecl>, regConsts: Dictionary<Microsoft.Z3.Sort, Dictionary<Expr, uint32>>,
                                     varDefs: Dictionary<string, Expr>, ctx: Context) =
            let dom= regRels.[rel.Name].Domain
            ctx.MkApp (regRels.[rel.Name], rel.Args |> Seq.mapi ( fun i arg -> match arg with
                                                                               | VarTerm(s) -> varDefs.[s]
                                                                               | AtomTerm(s) -> 
                                                                                    let value= ctx.MkConst(s, dom.[i])
                                                                                    ctx.MkNumeral(regConsts.[dom.[i]].[value], dom.[i])
                                                                               | WildcardTerm -> failwith "Wildcard shouldn't appear in rule"
                                                                ) |> Seq.toArray
                      ) :?> BoolExpr

        /// Answers returned by Z3 are not indexed in the same way as the original queries. In fact, it seems that the ordering of the variables in the returned answer
        /// is the ordering of the variables in the original query, but right-to-left
        /// This is all conjecture and has NOT been 100% confirmed by Nikolaj or Leo. However, it is currently the best option we have as other possibilities are either
        /// not working or have been ruled out. Expect changes in this sense
        member is.remapVariables (valuation: Expr, query: BoolExpr, context: Context) =
            let rec getVars (expr:Expr) =
                if expr.IsTrue then
                    [] |> List.toArray
                else if expr.IsEq then
                    let lhs= if expr.Args.[0].IsVar then expr.Args.[0] else expr.Args.[1]
                    [lhs] |> List.toArray
                else if expr.IsAnd then
                    expr.Args |> Seq.collect (fun arg -> getVars(arg)) |> Seq.toArray
                else
                    // shouldn't happen
                    failwith "Error: valuation should be made up of Horn clauses"

            let rec remap (mapping:Dictionary<Expr,Expr>) (valuation:Expr) (context:Context)=
                if valuation.IsTrue then
                    valuation
                else if valuation.IsEq then
                    let (lhs,rhs) = if valuation.Args.[0].IsVar then valuation.Args.[0],valuation.Args.[1] else valuation.Args.[1], valuation.Args.[0]
                    context.MkEq(mapping.[lhs], rhs) :> Expr
                else if valuation.IsAnd then
                    context.MkAnd( valuation.Args |> Seq.map (fun arg -> remap mapping arg context :?> BoolExpr) |> Seq.toArray ) :> Expr
                else if valuation.IsOr then 
                    context.MkOr( valuation.Args |> Seq.map (fun arg -> remap mapping arg context :?> BoolExpr) |> Seq.toArray ) :> Expr
                else
                    failwith "I can't recognise the valuation form" 

            // valuation may actually be several horn clauses. If such is the case, let's just take the first one, as the ordering seems to be consistent in each clause
            if valuation.IsTrue then    // nothing to do
                valuation
            else
                let ans= if valuation.IsOr then valuation.Args.[0] else valuation
                // bear in mind query is an AndExpr
                let reversedQueryVars= query.Args |> Seq.collect (fun arg -> arg.Args) |> Seq.toList
                                       |> List.fold (fun acc x -> if x.IsConst || List.exists (fun y -> y = x) acc then acc else acc @ [x]) [] |>
                                       List.toArray |> Array.rev
                let valuationVars= getVars(ans)
                let mapping= Dictionary<Expr,Expr>()
                valuationVars |> Seq.iteri (fun i exp -> mapping.[exp] <- reversedQueryVars.[i])

                // return the remapped valuation
                let newValuation= remap mapping valuation context
                newValuation

        member is.simplifyArg(arg: ITerm) =
            match arg with
            | Const(c) -> ConstTerm(c.Value.ToString(), c.Type.ToString())
            | Var(v) -> Microsoft.Research.DkalBackends.Ast.VarTerm(v.Name, v.Type.ToString())
            | _ -> failwith "Argument cannot be more complex than a constant or variable for Datalog engine"

        member is.simplifyPrincipal(ppalTerm: ITerm) =
            match ppalTerm with
            | PrincipalConstant(ppal) -> ConstTerm(ppal, Type.Principal.ToString())
            | Var(ppal) -> Microsoft.Research.DkalBackends.Ast.VarTerm(ppal.Name, Type.Principal.ToString())
            | _ -> failwith ("Term " + ppalTerm.ToString() + " does not denote a principal")

        member is.simplify(infon: ITerm) =
            match infon with
            | AsInfon(t) -> AtomFormula(FAKE_TRUE_RELATION, [])
            | EmptyInfon(t) -> AtomFormula(FAKE_TRUE_RELATION, []) // failwith "Attempting to simplify EmptyInfon ..."
            | SaidInfon(ppal, t) -> SpeechFormula(is.simplifyPrincipal(ppal), SaidSpeech, is.simplify(t))
              // datalog does not care about justifications, DKAL needs to take care of that
            | JustifiedInfon(inf, ev) -> is.simplify(inf)
            | AndInfon(t) -> match t with
                             | [] -> failwith "AndInfon may not be composed of no infons"
                             | term :: [] -> is.simplify(term)
                             | term :: terms -> AndFormula(is.simplify(term), is.simplify(AndInfon(terms)))
            | ImpliesInfon(a, b) -> ImpliesFormula(is.simplify(a), is.simplify(b))
            | Forall(var, term) -> is.simplify(term)
            | OrInfon(t) -> failwith (String.Format("Datalog cannot handle OR formulae {0}", infon))
            | App(f, args) -> AtomFormula(f.Name, args |> Seq.map (fun t -> is.simplifyArg(t)) |> Seq.toList)
            | t -> failwith (String.Format("Translation not implemented {0}", t))


        member is.relationRuleToFact(relationRule: Relation, regRels: Dictionary<string, FuncDecl>, regConsts: Dictionary<Expr, uint32>, varDefs: Dictionary<string, Expr>, ctx: Context) =
            let dom= regRels.[relationRule.Name].Domain
            ( regRels.[relationRule.Name], relationRule.Args
                                               |> Seq.mapi ( fun i arg -> match arg with
                                                                          | VarTerm(s) -> varDefs.[s]
                                                                          | AtomTerm(s) -> ctx.MkConst(s, dom.[i])
                                                                          | WildcardTerm -> failwith "Wildcard shouldn't appear in rule"
                                                           )
                                               |> Seq.map (fun expr -> regConsts.[expr]) |> Seq.toArray
            )

        member is.impliesRuleToBoolExpr(head: Relation, body: Relation list, regRels: Dictionary<string, FuncDecl>, regConsts: Dictionary<Microsoft.Z3.Sort, Dictionary<Expr, uint32>>,
                                        varDefs: Dictionary<string, Expr>, ctx: Context) =
            let premise= ctx.MkAnd(body |> Seq.fold ( fun acc rel -> acc |> Array.append [| is.relationToBoolExpr(rel, regRels, regConsts, varDefs, ctx) |] ) [||])
            let dom= regRels.[head.Name].Domain
            let consq= is.relationToBoolExpr(head, regRels, regConsts, varDefs, ctx)
            ctx.MkImplies(premise, consq)

        member is.rulePartToBoolExpr(rp: ProgramRulePart, regRels: Dictionary<string, FuncDecl>, regConsts:Dictionary<Microsoft.Z3.Sort, Dictionary<Expr, uint32>>,
                                     varDefs: Dictionary<string, Expr>, ctx: Context) =
            match rp with
            | RulePart(AtomRule(relationRule)) -> is.relationToBoolExpr(relationRule, regRels, regConsts, varDefs, ctx)
            | RulePart(ImpliesRule(cons, prems)) ->
                                                    let p= ctx.MkAnd(prems |> Seq.map (fun prem -> is.relationToBoolExpr(prem, regRels, regConsts, varDefs, ctx)) |> Seq.toArray)
                                                    let q= is.relationToBoolExpr(cons, regRels, regConsts, varDefs, ctx)
                                                    let res= ctx.MkImplies(p, q)
                                                    res
            | _ -> failwith "Cannot make a query from anything but a relation rule"

        member is.renameVars(rule: ProgramRulePart, i: int)=
            let renameTerm(t: Term)=
                match t with
                | VarTerm(varname) -> VarTerm(varname + "_" + i.ToString())
                | _ -> t

            let renameVarsFromRelation(rel: Relation)=
                Relation(rel.Name, rel.Args |> Seq.map (fun arg -> renameTerm(arg)))

            let rec renameVarsFromRule(r: Rule)=
                match r with
                | AtomRule(rel) -> AtomRule(renameVarsFromRelation(rel))
                | ImpliesRule(head, body) -> ImpliesRule(renameVarsFromRelation(head), body |> List.map (fun bd -> renameVarsFromRelation(bd)))

            match rule with
            | RulePart(r) -> RulePart(renameVarsFromRule(r))
            | _ -> rule

    // TODO move this two funs away to utils or something like that
    let z3ExprToDkal (expr:Microsoft.Z3.Expr) (typ:IType)
                     (regConsts:Dictionary<uint32, (Microsoft.Z3.Sort*Expr)>) =
        let originalDkalExpr= snd(regConsts.[uint32(expr.ToString())])
        // let ndx= int(originalDkalExpr.ToString().Replace("|",""))
        let mapped= originalDkalExpr.ToString().Replace("|","")
        // let mapped= mappedConstants.[ndx]

        if typ = Microsoft.Research.Dkal.Ast.Type.Boolean then
            match mapped.ToString() with
            | "true" -> Const(Constant(true))
            | _ -> Const(Constant(false))
        elif typ = Microsoft.Research.Dkal.Ast.Type.Int32 then
            Const(Constant(int(mapped.ToString())))
        elif typ = Microsoft.Research.Dkal.Ast.Type.Double then
            Const(Constant(double(mapped.ToString())))
        elif typ = Microsoft.Research.Dkal.Ast.Type.String then
            Const(Constant(mapped.ToString()))
        elif typ = Microsoft.Research.Dkal.Ast.Type.DateTime then
            Const(Constant(System.DateTime.Parse(mapped.ToString())))
        elif typ = Microsoft.Research.Dkal.Ast.Type.Principal then
            PrincipalConstant(mapped.ToString()) :> ITerm
        else
            failwith ("Unrecognized type for constant " + (originalDkalExpr.ToString()) + "! : "  + (typ.ToString()))

    let dkalTypeFromString (typ:string) =
        match typ with
        | "Dkal.Principal" -> Microsoft.Research.Dkal.Ast.Type.Principal
        | "System.Int32" -> Microsoft.Research.Dkal.Ast.Type.Int32
        | "System.Double" -> Microsoft.Research.Dkal.Ast.Type.Double
        | "System.DateTime" -> Microsoft.Research.Dkal.Ast.Type.DateTime
        | "System.String" -> Microsoft.Research.Dkal.Ast.Type.String
        | "System.Boolean" -> Microsoft.Research.Dkal.Ast.Type.Boolean
        | _ -> failwith ("Unrecognized type " + typ)
