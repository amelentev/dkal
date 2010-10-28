namespace Microsoft.Research.Dkal2Datalog

open DkalPrimalProver

open System
open System.Collections.Generic

module VarsCounter = 
  
  type VarsCounter(counter) = 
    let mutable plusVars = 0, 0
    let mutable impliesVars = 0, 0
    let mutable saidVars = 0
    let mutable impliedVars = 0

    let pairMax (p1: int, p2: int) (q1: int, q2: int) =
      Math.Max(p1,q1), Math.Max(p2,q2)

    member vc.Add(i: Infon) = 
      match i with
      | :? SaidImplied as si ->
        let v = counter (si.getKnowledge())
        match si with
        | :? Said -> saidVars <- Math.Max(saidVars, v)
        | :? Implied -> impliedVars <- Math.Max(impliedVars, v)
        | _ -> failwith "impossible"
        vc.Add(si.getKnowledge()) |> ignore
      | :? Plus as p ->
        let vl = counter (p.getLeft())
        vc.Add(p.getLeft())
        let vr = counter (p.getRight())
        vc.Add(p.getRight())
        plusVars <- pairMax plusVars (vl, vr)
      | :? Implies as i ->
        let vl = counter (i.getLeft())
        vc.Add(i.getLeft())
        let vr = counter (i.getRight())
        vc.Add(i.getRight())
        impliesVars <- pairMax impliesVars (vl, vr)
      | _ -> ()

    member vc.PlusCombinations() = 
      let kl, kr = plusVars 
      List.collect (fun i -> List.map (fun j -> i,j) [0..kr]) [0..kl] 

    member vc.PlusCombinations (sum: int) = 
      List.filter (fun (i,j) -> i+j = sum) (vc.PlusCombinations())

    member vc.ImpliesCombinations() = 
      let kl, kr = impliesVars 
      List.collect (fun i -> List.map (fun j -> i,j) [0..kr]) [0..kl] 

    member vc.ImpliesCombinations (sum: int) = 
      List.filter (fun (i,j) -> i+j = sum) (vc.ImpliesCombinations())

    member vc.SaidCombinations() = 
      [0 .. Math.Max(saidVars, impliedVars)]

    member vc.ImpliedCombinations() = 
      [0 .. Math.Max(saidVars, impliedVars)]

    member vc.MaxCombinations() = 
      [0 .. vc.MaxVars()]

    member vc.MaxVars() = 
      let mutable max = 0
      max <- Math.Max(max, let kl, kr = plusVars in kl + kr)
      max <- Math.Max(max, let kl, kr = impliesVars in kl + kr)
      max <- Math.Max(max, saidVars)
      max <- Math.Max(max, impliedVars)
      max

