namespace Microsoft.Research.Dkal2Datalog

open DkalPrimalProver
open Mapping
open Datalog

open System
open System.Collections.Generic

module Utils = 

  let arrayListToList (al: System.Collections.ArrayList) = 
    let mutable ret = []
    for a in al do
      match a with
      | :? Infon as  i -> ret <- ret @ [i]
      | _ -> failwith "expecting infons"
    ret
  
  let extendArgs (n: int) (args: Term seq) =
    if n <= Seq.length args then
      Seq.take n args |> Seq.toList
    else
      Seq.toList args @ List.replicate (n - Seq.length args) WildcardTerm

  let removeLast (l: 'a list) = 
    List.rev (List.rev l).Tail

  let cloneInfon (i: Infon) = 
    match i.Clone() with
    | :? Infon as i -> i
    | _ -> failwith "impossible"

  let computeSubformulas (i: Infon) = 
    let ret = new HashSet<Infon>()
    let rec traverse (i: Infon) =
      let copy = cloneInfon i
      copy.removePrefix()
      ret.Add(copy) |> ignore
      match i with
      | :? Plus as p ->
        traverse (p.getLeft())
        traverse (p.getRight())
      | :? Implies as i ->
        traverse (i.getLeft())
        traverse (i.getRight())
      | :? SaidImplied as si ->
        traverse (si.getKnowledge())
      | _ -> ()
    traverse i
    ret

  let maxQuotationDepthOf (infons: Infon seq) =
    let ret = ref 0
    let rec visit (i: Infon) (depth: int) = 
      match i with
      | :? Plus as p ->
        visit (p.getLeft()) depth
        visit (p.getRight()) depth
      | :? Implies as i ->
        visit (i.getLeft()) depth
        visit (i.getRight()) depth
      | :? SaidImplied as si ->
        if depth + 1 > ret.Value then
          ret.Value <- depth + 1
        visit (si.getKnowledge()) (depth + 1)
      | _ -> ()
    for i in infons do
      visit i 0
    ret.Value

  // collects the variables that appear in an infon in the same order they appear from left to right (with duplicates)
  let rec vars (i: Infon) = 
    match i with
    | :? SaidImplied as si -> 
      vars (si.getKnowledge())
    | :? Plus as p -> 
      vars (p.getLeft()) @ vars (p.getRight())
    | :? Implies as i ->
      vars (i.getLeft()) @ vars (i.getRight())
    | :? Variable as v ->
      [v.getName().ToLower()]
    | _ -> []

  // collects the variables and constants that appear in an infon in the same order they appear from left to right (with duplicates)
  let rec varsConsts (i: Infon) = 
    match i with
    | :? SaidImplied as si -> 
      let ppalKind = if si.getPrincipal().isVar() then VarTerm else AtomTerm
      [ppalKind(si.getPrincipal().getName().ToLower())] @ varsConsts (si.getKnowledge()) 
    | :? Plus as p -> 
      varsConsts (p.getLeft()) @ varsConsts (p.getRight())
    | :? Implies as i ->
      varsConsts (i.getLeft()) @ varsConsts (i.getRight())
    | :? Variable as v ->
      [VarTerm (v.getName().ToLower())]
    | :? Function as f ->
      if f.getArguments().Count = 0 then 
        [AtomTerm (f.ToString())]
      else
        List.map (fun a -> 
                   match (a: Infon) with
                   | :? Function as g -> AtomTerm (g.ToString())
                   | :? Variable as v -> VarTerm (v.getName().ToLower())
                   | _ -> failwith "expecting constant or variable arguments in function"
                  ) (arrayListToList (f.getArguments()))
    | _ -> []

  let numberOfVars (i: Infon) = 
    (vars i).Length

  let numberOfVarsConsts (i: Infon) = 
    (varsConsts i).Length
