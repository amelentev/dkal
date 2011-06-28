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

namespace Microsoft.Research.Dkal.LogicEngine.ML

open System.Collections.Generic
//open Microsoft.Research.Dkal.LogicEngine
open Microsoft.Research.Dkal

open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Substrate // for SubstrateDispatcher
open Microsoft.Research.Dkal.Interfaces // for ISignatureProvider and IInfostrate

module MLType =
  // type substrateTerm = Microsoft.Research.Dkal.Interfaces.ISubstrateTerm
  // in F*, import this type abstractly from another module
  type substrateQueryTerm = Microsoft.Research.Dkal.Interfaces.ISubstrateQueryTerm
  type substrateUpdateTerm = Microsoft.Research.Dkal.Interfaces.ISubstrateUpdateTerm

  type principal = string

  and var = // IVar
    { typ : typ; name : string }

  and typ =  // IType
  // from Types.fs, see also Ast/ActivePatterns.fs
  // BasicType types
    | Infon
    | Principal
    | SubstrateUpdate
    | SubstrateQuery
    | Action
    | Condition
    | RuleT
    | Evidence
  // Substrate types
    | Boolean
    | Int32
    | Double
    | String

  and constant =
    | True
    | False
    | SubstrateConstant of obj // of obj in the original code
    | PrincipalConstant of principal // of string in original code

  and func =
  // from Ast.Infon/ActivePatterns.fs
    // Rule: <condition> do <action>
    | SeqRule
    | EmptyRule
    | Rule
    | RuleOnce
    // Condition
    | SeqCondition
    | EmptyCondition
    | WireCondition // upon in concrete syntax
    | KnownCondition // if in concrete syntax
    // Action
    | SeqAction
    | EmptyAction
    | Send // ppal (destination), msg
    | JustifiedSend // ppal (destination), msg
    | JustifiedSay // ppal (destination), msg
    | Learn
    | Forget
    | Install // add a rule to set of rules
    | Uninstall // regarding a rule
    | Apply //of substrateUpdateTerm // apply this update to the substrate
    | Drop // regarding an infon that came in as a message
    // Infon
    | EmptyInfon
    | AsInfon //of substrateQueryTerm
    | AndInfon // may be easier to consider just binary case
    | ImpliesInfon // infon, infon
    | SaidInfon  // ppal (sender), msg
    | JustifiedInfon // infon, evidence
    // Evidence
    | EmptyEvidence
    | SignatureEvidence // ppal, term, int
      // might want to change the third type to dsig, signature for .Net
    | ModusPonensEvidence
    | AndEvidence
    | AsInfonEvidence //of substrateQueryTerm
    | RelationInfon of string // no active pattern for it, base case for infons

  and term = // ITerm
  // from Ast.Tree/ActivePatterns.fs
  // from Ast/ActivePatterns.fs
    | Var of var
    | Const of constant
    | Forall of var * term
    | App of func * (term list)
    | ConcretizationEvidence of term * substitution // ExplicitSubstitutionTerm.fs
    | SubstrateQueryTerm of substrateQueryTerm
    | SubstrateUpdateTerm of substrateUpdateTerm

  and substitution = Dictionary< var, term >
  // wrap the functions of dictionary used inside
  // other functions and only use them there.
  // use Guido's functional wrapper from Substitution

module Subst =
  let domainContains (s: MLType.substitution) (v: MLType.var) : bool =
    s.ContainsKey v 

  let apply (s: MLType.substitution) (v: MLType.var) : MLType.term =
    let found, ret = s.TryGetValue v
    if found then ret else MLType.Var(v)

  let id : MLType.substitution = 
    new Dictionary<_, _>()

  let extend (s: MLType.substitution) (v: MLType.var, t: MLType.term) =
    if t = MLType.Var(v) then 
      s
    else
      let newSubst = new Dictionary<_, _>(s)
      newSubst.[v] <- t
      newSubst

  let domain (s: MLType.substitution) : MLType.var list =
    [ for kvp in s -> kvp.Key ]    


  let restrictTo (s: MLType.substitution) (vars: MLType.var list) = 
    let newSubst = new Dictionary<_, _>(s)
    for v in s.Keys do
      if not <| List.exists (fun v' -> v' = v) vars then
        newSubst.Remove(v) |> ignore
    newSubst

  let forget (s: MLType.substitution) (vars: MLType.var list) =
    let relevantVars = new HashSet<_>(domain s)
    relevantVars.ExceptWith vars
    restrictTo s (relevantVars |> Seq.toList)

module Translation =
  let MLtypOfIType (t: IType) : MLType.typ =
    match t with
    | Infon -> MLType.Infon
    | Principal -> MLType.Principal
    | SubstrateUpdate -> MLType.SubstrateUpdate
    | SubstrateQuery -> MLType.SubstrateQuery
    | Action -> MLType.Action
    | Condition -> MLType.Condition
    | Rule -> MLType.RuleT
    | Evidence -> MLType.Evidence
    | Substrate(t1) when t1=typeof<bool> -> MLType.Boolean
    | Substrate(t1) when t1=typeof<int32> -> MLType.Int32
    | Substrate(t1) when t1=typeof<double> -> MLType.Double
    | Substrate(t1) when t1=typeof<string> -> MLType.String
    | Substrate(_) -> failwith "Substrate type not supported" 
    | _ -> failwith "IType not supported"

  let ITypeOfMLtyp (t: MLType.typ) : IType =
    match t with
    | MLType.Infon -> Type.Infon
    | MLType.Principal -> Type.Principal
    | MLType.SubstrateUpdate -> Type.SubstrateUpdate
    | MLType.SubstrateQuery -> Type.SubstrateQuery
    | MLType.Action -> Type.Action
    | MLType.Condition -> Type.Condition
    | MLType.RuleT -> Type.Rule
    | MLType.Evidence -> Type.Evidence
    | MLType.Boolean -> Type.Boolean
    | MLType.Int32 -> Type.Int32
    | MLType.Double -> Type.Double
    | MLType.String -> Type.String

  let MLvarOfIVar (v: IVar) : MLType.var =
    let t = MLtypOfIType v.Type
    { typ = t ; name = v.Name }

  let IVarOfMLvar (v: MLType.var) : Variable =
    { Name = v.name ; Type = ITypeOfMLtyp v.typ }
  
  let MLfuncOfFunction (f: Function) : MLType.func =
    match f.Name with
    | Primitives.SeqRule -> MLType.SeqRule
    | Primitives.EmptyRule -> MLType.EmptyRule
    | Primitives.Rule -> MLType.Rule
    | Primitives.RuleOnce -> MLType.RuleOnce
    | Primitives.SeqCondition -> MLType.SeqCondition
    | Primitives.EmptyCondition -> MLType.EmptyCondition
    | Primitives.WireCondition -> MLType.WireCondition
    | Primitives.KnownCondition -> MLType.KnownCondition
    | Primitives.SeqAction -> MLType.SeqAction
    | Primitives.EmptyAction -> MLType.EmptyAction
    | Primitives.Send -> MLType.Send
    | Primitives.JustifiedSend -> MLType.JustifiedSend
    | Primitives.JustifiedSay -> MLType.JustifiedSay
    | Primitives.Learn -> MLType.Learn
    | Primitives.Forget -> MLType.Forget
    | Primitives.Install -> MLType.Install
    | Primitives.Uninstall -> MLType.Uninstall
    | Primitives.Apply -> MLType.Apply
    | Primitives.Drop -> MLType.Drop
    | Primitives.EmptyInfon -> MLType.EmptyInfon
    | Primitives.AsInfon -> MLType.AsInfon
    | Primitives.And when f.RetType = Type.Infon -> MLType.AndInfon
    | Primitives.Implies when f.RetType = Type.Infon -> MLType.ImpliesInfon
    | Primitives.Said -> MLType.SaidInfon
    | Primitives.Justified -> MLType.JustifiedInfon
    | Primitives.EvEmpty -> MLType.EmptyEvidence
    | Primitives.EvSignature -> MLType.SignatureEvidence
    | Primitives.EvModusPonens -> MLType.ModusPonensEvidence
    | Primitives.EvAnd -> MLType.AndEvidence
    | Primitives.EvAsInfon -> MLType.AsInfonEvidence
    //| RelationInfon of string //?? TODO
    | _ -> failwith ("MLfuncOfFunction, case not supported: " + f.Name)

  let rec MLtermOfITerm (term: ITerm) : MLType.term =
    match term with
    | Var(v) -> MLType.Var(MLvarOfIVar v)
    | SubstrateConstant(o) -> MLType.Const(MLType.SubstrateConstant(o))
    | PrincipalConstant(n) -> MLType.Const(MLType.PrincipalConstant(n))
    | True -> MLType.Const(MLType.True)
    | False -> MLType.Const(MLType.False)
    | Forall(v, t) -> MLType.Forall(MLvarOfIVar v, MLtermOfITerm t)
    | App(f, tl) -> MLType.App(MLfuncOfFunction f, List.map MLtermOfITerm tl)
    | :? ISubstrateQueryTerm as sq -> MLType.SubstrateQueryTerm(sq)
    | :? ISubstrateUpdateTerm as su -> MLType.SubstrateUpdateTerm(su) 
    | ConcretizationEvidence(t, s) -> 
        MLType.ConcretizationEvidence(MLtermOfITerm t, MLsubstitutionOfISubstitution s)
    | _ -> failwith "MLtermOfITerm, case not supported"

  and MLsubstitutionOfISubstitution (subst: ISubstitution) : MLType.substitution =
    List.fold (fun res k -> 
               Subst.extend res
                 (MLvarOfIVar k, MLtermOfITerm (subst.Apply k)))
              Subst.id subst.Domain

  let FunctionOfMLfunc (f: MLType.func) : Function = 
    let res = 
      match f with
      | MLType.SeqRule -> Primitives.SeqRule
      | MLType.EmptyRule ->  Primitives.EmptyRule
      | MLType.Rule ->  Primitives.Rule
      | MLType.RuleOnce ->  Primitives.RuleOnce
      | MLType.SeqCondition ->  Primitives.SeqCondition
      | MLType.EmptyCondition ->  Primitives.EmptyCondition
      | MLType.WireCondition ->  Primitives.WireCondition
      | MLType.KnownCondition ->  Primitives.KnownCondition
      | MLType.SeqAction ->  Primitives.SeqAction
      | MLType.EmptyAction ->  Primitives.EmptyAction
      | MLType.Send ->  Primitives.Send
      | MLType.JustifiedSend ->  Primitives.JustifiedSend
      | MLType.JustifiedSay ->  Primitives.JustifiedSay
      | MLType.Learn ->  Primitives.Learn
      | MLType.Forget ->  Primitives.Forget
      | MLType.Install ->  Primitives.Install
      | MLType.Uninstall ->  Primitives.Uninstall
      | MLType.Apply ->  Primitives.Apply
      | MLType.Drop ->  Primitives.Drop
      | MLType.EmptyInfon ->  Primitives.EmptyInfon
      | MLType.AsInfon ->  Primitives.AsInfon
      | MLType.AndInfon ->  Primitives.And
      | MLType.ImpliesInfon ->  Primitives.Implies
      | MLType.SaidInfon ->  Primitives.Said
      | MLType.JustifiedInfon ->  Primitives.Justified
      | MLType.EmptyEvidence ->  Primitives.EvEmpty
      | MLType.SignatureEvidence ->  Primitives.EvSignature
      | MLType.ModusPonensEvidence ->  Primitives.EvModusPonens
      | MLType.AndEvidence ->  Primitives.EvAnd
      | MLType.AsInfonEvidence ->  Primitives.EvAsInfon
      //| RelationInfon of string //??
      | _ -> failwith "FunctionOfMLfunc cannot be called on Apply, AsInfon or AsInfonEvidence"
    res |> Primitives.SolveFunction |> Option.get

  let rec ITermOfMLterm (t: MLType.term) : ITerm =
    match t with
    | MLType.Var(v) -> Var(IVarOfMLvar v)
    | MLType.Const(MLType.SubstrateConstant(o)) -> Constant(o) :> ITerm
    | MLType.Const(MLType.PrincipalConstant(n)) -> PrincipalConstant(n) :> ITerm
    | MLType.Const(MLType.True) -> Constant(true) :> ITerm
    | MLType.Const(MLType.False) -> Constant(false) :> ITerm
    | MLType.Forall(v, t) -> {Var = IVarOfMLvar v; 
                          Term = ITermOfMLterm t} :> ITerm
    // 5 special cases for the the functions where the number of arguments
    // can vary. Special cases here because they are special cases in Builders.fs
    | MLType.App(MLType.SeqRule, tl) ->
      Builders.SeqRule(List.map ITermOfMLterm tl)
    | MLType.App(MLType.SeqCondition, tl) ->
      Builders.SeqCondition(List.map ITermOfMLterm tl)
    | MLType.App(MLType.SeqAction, tl) ->
      Builders.SeqAction(List.map ITermOfMLterm tl)
    | MLType.App(MLType.AndInfon, tl) ->
      Builders.AndInfon(List.map ITermOfMLterm tl)
    | MLType.App(MLType.AndEvidence, tl) ->
      Builders.AndEvidence(List.map ITermOfMLterm tl)
    // general case for App: all the other cases are the same in Builders.fs
    | MLType.App(f, tl) -> 
      { Function = FunctionOfMLfunc f ;
        Args = List.map ITermOfMLterm tl } :> ITerm
    | MLType.SubstrateQueryTerm(t0) -> t0 :> ITerm
    | MLType.SubstrateUpdateTerm(t0) -> t0 :> ITerm
    | MLType.ConcretizationEvidence(t0, s) -> 
       ExplicitSubstitutionTerm(ITermOfMLterm t0, ISubstitutionOfMLsubstitution s) :> ITerm
   // | _ -> failwith "MLType.Apply, MLType.AsInfon and MLType.AsInfonEvidence cannot have arguments"

  and ISubstitutionOfMLsubstitution (subst: MLType.substitution) : ISubstitution =
    List.fold (fun res k ->
               res.Extend 
                 (IVarOfMLvar k, ITermOfMLterm (Subst.apply subst k)))
              Substitution.Id (Subst.domain subst) 

module Utilities =

  let int_to_string (i: int) =
    i.ToString()

  let value_knowledge (_infostrate : IInfostrate option) () =
    _infostrate.Value.Knowledge |> Seq.toList |>
    List.map Translation.MLtermOfITerm

  let value_checkSignature (_signatureProvider : ISignatureProvider option) (infon : MLType.term) (principal : MLType.principal) (sign : obj) =
    _signatureProvider.Value.CheckSignature (Translation.ITermOfMLterm infon) principal (sign :?> int)

  let substrateDispatcher_solve (q : MLType.substrateQueryTerm list) s =
    let q' = Seq.ofList (List.map (fun q0 -> q0 :> ISubstrateQueryTerm) q)
    let s' = Seq.ofList (List.map Translation.ISubstitutionOfMLsubstitution s)
    SubstrateDispatcher.Solve q' s' |> Seq.toList |>
    List.map Translation.MLsubstitutionOfISubstitution

module Term =
  let rec vars (t: MLType.term) : MLType.var list =
    match t with
    | MLType.Forall(v, t) ->       
      let termVars = new HashSet<_>(vars t)
      termVars.Remove(v) |> ignore
      termVars |> Seq.toList
    | MLType.App(_, tl) -> new HashSet<_>(List.collect (fun (a: MLType.term) -> vars a) tl) |> Seq.toList
    | MLType.Var v -> [v]
    | MLType.Const _ -> []
    | MLType.ConcretizationEvidence(t, s) -> 
      let ret = new HashSet<_>(vars t)
      for v in Subst.domain s do  
        if ret.Contains(v) then
          ret.Remove v |> ignore
          ret.UnionWith <| vars (Subst.apply s v)
      ret |> Seq.toList
    | MLType.SubstrateQueryTerm(t0) ->
      t0.Vars |> Seq.toList |> List.map Translation.MLvarOfIVar
    | MLType.SubstrateUpdateTerm(t0) ->
      t0.Vars |> Seq.toList |> List.map Translation.MLvarOfIVar

  let rec boundVars (t: MLType.term) : MLType.var list =
    match t with
    | MLType.Forall(v, t) ->
      new HashSet<_>(v :: boundVars t) |> Seq.toList
    | MLType.App(_, tl) -> new HashSet<_>(List.collect (fun (a: MLType.term) -> vars a) tl) |> Seq.toList
    | MLType.Var _ -> []
    | MLType.Const _ -> []
    | MLType.ConcretizationEvidence(t, s) -> 
      let ret = new HashSet<_>(boundVars t)
      for v in Subst.domain s do  
        if ret.Contains(v) then
          ret.Remove v |> ignore
          ret.UnionWith <| boundVars (Subst.apply s v)
      ret |> Seq.toList
    | MLType.SubstrateQueryTerm(t0) ->
      t0.BoundVars |> Seq.toList |> List.map Translation.MLvarOfIVar
    | MLType.SubstrateUpdateTerm(t0) ->
      t0.BoundVars |> Seq.toList |> List.map Translation.MLvarOfIVar

  let freshVar (var: MLType.var) (otherVars: MLType.var list) =
  // from ForallTerm.fs
  // TODO: uses objects on strings
    let prefix = "FreshVar#"
    let freshVars = List.filter (fun (v: MLType.var) -> (v.name).StartsWith(prefix)) (var::otherVars)
    let freshNumbers = List.map (fun (v: MLType.var) -> System.Int32.Parse((v.name).Substring(prefix.Length))) freshVars
    let freshVarId = if List.isEmpty freshNumbers then 0 else (List.max freshNumbers) + 1
    let freshVar = { MLType.name = prefix + freshVarId.ToString(); MLType.typ = var.typ }
    let subst = Subst.extend Subst.id (var, MLType.Var(freshVar))
    freshVar, subst

  let rec composeWith (s: MLType.substitution) (s': MLType.substitution) : MLType.substitution =
  // from Subst; moved here because of mutual recursions
    let (newSubst : MLType.substitution) = new Dictionary<_, _>(s)
    for v in Subst.domain s' do
      newSubst.[v] <- apply (Subst.apply s' v) s
    for v in Subst.domain s do
      if not <| Subst.domainContains s' v then
        newSubst.[v] <- Subst.apply s v
    newSubst

  // Subst.composeWith (above) and Term.apply (below) are mutually recursive!!
  and apply (t: MLType.term) (s: MLType.substitution) : MLType.term =
    match t with
    | MLType.Var(v) -> Subst.apply s v // from Variable.fs
    | MLType.Const _ -> t // from Constants.fs
    | MLType.Forall(v0, t0) -> // from ForallTerm.fs
      // the substitution is not applied to the quantified variable
      let s = Subst.forget s [v0]
      // check that there will be no variable capture
      let varsToCheck = new HashSet<_>(vars t0)
      varsToCheck.IntersectWith (Subst.domain s)
      let mappedVars = Seq.collect (fun (v': MLType.var) -> vars (Subst.apply s v')) varsToCheck
      if Seq.exists (fun v -> v = v0) mappedVars 
      then
        let newVar, newVarSubst = freshVar v0 ((vars t0) @ (mappedVars |> Seq.toList))
        MLType.Forall(newVar, apply (apply t0 newVarSubst) s)
      else
        MLType.Forall(v0, apply t0 s)
    | MLType.App(f, tl) -> MLType.App(f, List.map (fun t0 -> apply t0 s) tl) // from TreeTerm.fs
    | MLType.ConcretizationEvidence(t1, s1) -> // from ExplicitSubstitutionTerm.fs
      MLType.ConcretizationEvidence(t1, composeWith s s1)
    | MLType.SubstrateQueryTerm(t0) -> 
      t0.Apply (Translation.ISubstitutionOfMLsubstitution s) |>
      Translation.MLtermOfITerm
    | MLType.SubstrateUpdateTerm(t0) ->
      t0.Apply (Translation.ISubstitutionOfMLsubstitution s) |>
      Translation.MLtermOfITerm
    
  let rec contains (t: MLType.term) (v: MLType.var) =
    match t with
    | MLType.Var(v1) -> v=v1
    | _ -> false // List.fold (fun res t0 -> res || contains t0 v) false (list_of_subterms t)//TODO

  let rec innerTerm (ft: MLType.term) = // from ForallTerm.fs
    match ft with 
    | MLType.Forall(v, t) ->
      match t with 
      | MLType.Forall(v', t') -> innerTerm t'
      | _ -> t
    | _ -> failwith "innerTerm can only be called on a ForallTerm"

  let instantiate (ft: MLType.term) (s: MLType.substitution) = // from ForallTerm.fs
    let remainingVars = new HashSet<_>(boundVars ft) // TODO HashSet
    remainingVars.ExceptWith (Subst.domain s) // TODO HashSet
    let innerSubst = apply ft s
    List.fold (fun t v -> MLType.Forall(v, t)) 
      innerSubst (Seq.toList remainingVars) // TODO HashSet

  let changeVarName (ft: MLType.term) (s:MLType.substitution) = // from ForallTerm.fs
    match ft with
    | MLType.Forall(v, t) ->
      let v', s' = 
        freshVar
          v 
          ((List.foldBack 
              (fun v acc -> (vars (Subst.apply s v)) @ acc)
              (Subst.domain s) [])
            @ (Subst.domain s) @ (vars t))
      MLType.Forall(v, apply t s'), s'
    | _ -> failwith "changeVarName can only be called on a ForallTerm"

  let rec unifyFrom (t1: MLType.term) (s: MLType.substitution) (t2: MLType.term) : MLType.substitution option =
    match t1 with
    | MLType.Var(v1) -> // from Variable.fs
      match t2 with
      | _ when apply t1 s = apply t2 s -> Some s
      | _ when not(List.exists (fun v' -> v1 = v') (vars t2)) -> 
        if Subst.domainContains s v1 then
          unifyFrom (apply t1 s) s t2
        else
          Some <| composeWith (Subst.extend Subst.id (v1, apply t2 s)) s
      | _ -> None
    | MLType.Const(c1) -> // from Constants.fs
      match t2 with
      | _ when t1 = apply t2 s -> Some s
      | MLType.Var(v2) -> unifyFrom t2 s t1
      | _ -> None
    | MLType.Forall(v1, t1') -> // from ForallTerm.fs
      match t2 with
      | MLType.Forall(v2, t2') -> 
        match unifyFrom (innerTerm t1) s (innerTerm t2) with
        | Some s -> 
          if List.forall 
                (fun v -> 
                  if List.exists (fun v' -> v' = v) (boundVars t1) then
                    match Subst.apply s v with
                    | MLType.Var(v0) -> List.exists (fun v' -> v' = v0) (boundVars t2)
                    | _ -> false
                  elif List.exists (fun v' -> v' = v) (boundVars t2) then
                    match Subst.apply s v with
                    | MLType.Var(v0) -> List.exists (fun v' -> v' = v0) (boundVars t1)
                    | _ -> false
                  else 
                    true) 
                (Subst.domain s) then
            Some s
          else 
            None
        | _ -> None
      | _ -> 
        unifyFrom t1' s t2
    | MLType.App(f1, tlist1) ->
      match t2 with
      | MLType.Var(v) -> unifyFrom t2 s t1
      | MLType.App(f2, tlist2)
        when f1 = f2
          && List.length tlist1 = List.length tlist2 -> 
        let mutable okSoFar = true
        let mutable ret = s
        let mutable i = 0
        while okSoFar && i < List.length tlist1 do
          match unifyFrom (apply tlist1.[i] ret) ret (apply tlist2.[i] ret) with
          | Some s ->
            ret <- s
          | None -> 
            okSoFar <- false
          i <- i + 1
        if okSoFar then
          Some ret
        else
          None
      | _ -> None
    | MLType.ConcretizationEvidence(et1, s1) ->
      match t2 with
      | MLType.ConcretizationEvidence(et2, s2) -> 
        unifyFrom (apply et1 s1) s2 (apply et2 s2)
      | _ -> unifyFrom t2 s t1
    | MLType.SubstrateQueryTerm(t0) ->
      t0.UnifyFrom (Translation.ISubstitutionOfMLsubstitution s) (Translation.ITermOfMLterm t2) |>
      Option.map Translation.MLsubstitutionOfISubstitution
    | MLType.SubstrateUpdateTerm(t0) ->
      t0.UnifyFrom (Translation.ISubstitutionOfMLsubstitution s) (Translation.ITermOfMLterm t2) |>
      Option.map Translation.MLsubstitutionOfISubstitution

  let unify (t1: MLType.term) (t2: MLType.term) : MLType.substitution option =
    unifyFrom t1 (Subst.id) t2

//module ForallTerm = // from ForallTerm.fs




