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

namespace Microsoft.Research.Dkal.LogicEngine.FStar.Deps

open System.Collections.Generic
open Microsoft.Research.Dkal

open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Ast

open Microsoft.Research.Dkal.Interfaces

module Builders =
  type substrateQueryTerm = Microsoft.Research.Dkal.Interfaces.ISubstrateQueryTerm
  type substrateUpdateTerm = Microsoft.Research.Dkal.Interfaces.ISubstrateUpdateTerm

  let SeqRuleStr (b:bool) : string = Primitives.SeqRule
  let EmptyRule (b:bool) : string = Primitives.EmptyRule
  let RuleF (b:bool) : string = Primitives.Rule  (* to avoid conflict with Type.Rule *)
  let RuleOnce (b:bool) : string = Primitives.RuleOnce  
  let SeqConditionStr (b:bool) : string = Primitives.SeqCondition  
  let EmptyCondition (b:bool) : string = Primitives.EmptyCondition  
  let WireCondition (b:bool) : string = Primitives.WireCondition  
  let KnownCondition (b:bool) : string = Primitives.KnownCondition  
  let SeqAction (b:bool) : string = Primitives.SeqAction  
  let EmptyAction (b:bool) : string = Primitives.EmptyAction  
  let Send (b:bool) : string = Primitives.Send 
  let JustifiedSend (b:bool) : string = Primitives.JustifiedSend  
  let JustifiedSay (b:bool) : string = Primitives.JustifiedSay  
  let Learn (b:bool) : string = Primitives.Learn 
  let Forget (b:bool) : string = Primitives.Forget 
  let Install (b:bool) : string = Primitives.Install 
  let Uninstall (b:bool) : string = Primitives.Uninstall 
  let Apply (b:bool) : string = Primitives.Apply 
  let Drop (b:bool) : string = Primitives.Drop 
  let EmptyInfon (b:bool) : string = Primitives.EmptyInfon 
  let AsInfon (b:bool) : string = Primitives.AsInfon 
  let And (b:bool) : string = Primitives.And 
  let Implies (b:bool) : string = Primitives.Implies 
  let Said (b:bool) : string = Primitives.Said 
  let Justified (b:bool) : string = Primitives.Justified 
  let EvEmpty (b:bool) : string = Primitives.EvEmpty 
  let EvSignature (b:bool) : string = Primitives.EvSignature
  let EvModusPonens (b:bool) : string = Primitives.EvModusPonens 
  let EvAnd (b:bool) : string = Primitives.EvAnd 
  let EvAsInfon (b:bool) : string = Primitives.EvAsInfon 

  let Constant_ITerm (o:obj) : ITerm = Constant(o) :> ITerm
  let Constant_ITerm_bool (b:bool) : ITerm = Constant(b) :> ITerm
  let PrincipalConstant_ITerm (n:string) : ITerm = PrincipalConstant(n) :> ITerm

  let Application_ITerm (a:Application) : ITerm = a :> ITerm
  let substrateUpdateTerm_ITerm (s:substrateUpdateTerm) : ITerm = s :> ITerm
  let substrateQueryTerm_ITerm (s:substrateQueryTerm) : ITerm = s :> ITerm
  let explicitSubstitutionTerm_ITerm (t:ITerm) (s:ISubstitution) : ITerm = 
    ExplicitSubstitutionTerm(t, s) :> ITerm

  let mkIVar (n:string) (t:IType) : IVar =
    { Name = n; Type = t } :> IVar
  let mkFunction (n:string) (r:IType) (a:IType list) (i:ITerm option) =
    { Name = n; RetType = r; ArgsType = a; Identity = i } 
  let mkApplication (f:Function) (a:ITerm list) =
    { Function = f; Args = a }

  let ITermOfIVar (v:IVar) : ITerm = Var(v)

  let SubstExtend (s:ISubstitution) (k:IVar, t:ITerm) : ISubstitution = s.Extend (k, t)

  let Infon (b : bool) : IType = Type.Infon
  let Principal (b : bool) : IType = Type.Infon
  let SubstrateUpdate (b : bool) : IType = Type.SubstrateUpdate 
  let SubstrateQuery (b : bool) : IType = Type.SubstrateQuery
  let Action (b : bool) : IType = Type.Action
  let Condition (b : bool) : IType = Type.Condition
  let Rule (b : bool) : IType = Type.Rule
  let Evidence (b : bool) : IType = Type.Evidence
  let Boolean (b : bool) : IType = Type.Boolean
  let Int32 (b : bool) : IType = Type.Int32
  let Double (b : bool) : IType = Type.Double
  let String (b : bool) : IType = Type.String

  (* Hashset utilities *)
  let HashSet_new (l : list<'a>) : HashSet<'a> = new HashSet<_>(l)
  let HashSet_remove (h : HashSet<'a>) (e : 'a) : bool = h.Remove(e)
  let HashSet_toList (h : HashSet<'a>) : list<'a> = h |> Seq.toList
  let HashSet_contains (h : HashSet<'a>) (e : 'a) : bool = h.Contains e
  let HashSet_unionWith (h : HashSet<'a>) (l : list<'a>) : unit = h.UnionWith(l)
  let HashSet_intersectWith (h : HashSet<'a>) (l : list<'a>) : unit = h.IntersectWith(l)
  let HashSet_exceptWith (h : HashSet<'a>) (l : list<'a>) : unit = h.ExceptWith(l)

  (* String utilities *)
  let String_StartsWith (s : string) (p: string) : bool = s.StartsWith(p)
  let String_Substring (s: string) (i: int) : string = s.Substring(i)
  let String_Length (s: string) = s.Length

  let isEmpty (l: list<'a>) : bool = List.isEmpty l
  let max (l: list<'a>) : 'a = List.max l
  let int_to_string (i:int) : string = i.ToString()
  let System_Int32_Parse (s:string) : int = System.Int32.Parse(s)
  let nth (l: list<'a>) (i: int) : 'a = l.[i]
  let lessThan (i:int) (j:int) : bool = i < j

  type listFS<'a> = list<'a>
  let NilFS (b:bool) : listFS<'a> = []
  let ConsFS (h:'a) (t:listFS<'a>) : listFS<'a> = h::t

  let rec PrimsListOfList (l:list<'a>) : Prims.list<'a> = 
    match l with
    | [] -> new Prims.Nil<'a>() :> Prims.list<'a>
    | h::t -> new Prims.Cons<'a>(h, PrimsListOfList t) :> Prims.list<'a>

  type optionFS<'a> = option<'a>
  let NoneFS (b:bool) : optionFS<'a> = None
  let SomeFS (a:'a) : optionFS<'a> = Some(a)
  let PrimsOptionOfOption (o:option<'a>) : Prims.option<'a> = 
    match o with
    | None -> new Prims.None<'a>() :> Prims.option<'a>
    | Some(a) -> new Prims.Some<'a>(a) :> Prims.option<'a>

  type tupleFS<'a,'b> = ('a * 'b)
  let PrimsTupleOfTuple (t:tupleFS<'a,'b>) : Prims.DepTuple<'a, 'b> =
    let (x, y) = t in
    new Prims.DconDepTuple<'a, 'b>(x, y) :> Prims.DepTuple<'a, 'b>

  let _substrateQueryTerm_Vars (s:substrateQueryTerm) = s.Vars
  let _substrateUpdateTerm_Vars (s:substrateUpdateTerm) = s.Vars
  let _substrateQueryTerm_boundVars (s:substrateQueryTerm) = s.BoundVars
  let _substrateUpdateTerm_boundVars (s:substrateUpdateTerm) = s.BoundVars
  let substrateQueryTerm_apply (s:substrateQueryTerm) (s2:ISubstitution) : ITerm = s.Apply s2
  let substrateUpdateTerm_apply (s:substrateUpdateTerm) (s2:ISubstitution) : ITerm = s.Apply s2
  let _substrateQueryTerm_unifyFrom (s:substrateQueryTerm) (s2:ISubstitution) (t:ITerm) : ISubstitution option = s.UnifyFrom s2 t
  let _substrateUpdateTerm_unifyFrom (s:substrateUpdateTerm) (s2:ISubstitution) (t:ITerm) : ISubstitution option = s.UnifyFrom s2 t

  (* Dictionary utilities *)
  let domainContains (s: Dictionary<'a,'b>) (v: 'a) : bool =
    s.ContainsKey v 

  let subst_apply_def (s: Dictionary<'a,'b>) (v: 'a) (def:'b) : 'b = 
    let found, ret = s.TryGetValue v
    if found then ret else def
(*
  let emptySubst (b:bool) : Dictionary<'a,'b> = 
    new Dictionary<_, _>()
*)
  let copy (s : Dictionary<'a,'b>) : Dictionary<'a,'b> =
    new Dictionary<_, _>(s)

  let assign (s: Dictionary<'a,'b>) (v:'a) (t:'b) : bool =
    s.[v] <- t; false

  let extend (s: Dictionary<'a,'b>) (v:'a) (t:'b) = 
//    if t = (Types.Var(v) :> Types.term) then 
//      s
//    else
      let newSubst = new Dictionary<_, _>(s)
      newSubst.[v] <- t
      newSubst

  let _domain (s: Dictionary<'a,'b>) : list<'a> =
    [ for kvp in s -> kvp.Key ]    

  let _restrictTo (s: Dictionary<'a,'b>) (vars: list<'a>) = 
    let newSubst = new Dictionary<_, _>(s)
    for v in s.Keys do
      if not <| List.exists (fun v' -> v' = v) vars then
        newSubst.Remove(v) |> ignore
    newSubst

  let _forget (s: Dictionary<'a,'b>) (vars: list<'a>) = 
    let relevantVars = new HashSet<_>(_domain s)
    relevantVars.ExceptWith vars
    _restrictTo s (relevantVars |> Seq.toList)


    
    