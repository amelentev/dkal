namespace Microsoft.Research.GeneralPDP.XACML.PDP

open Microsoft.Research.GeneralPDP.XACML.Ast
open Basics

open System.Collections.Generic

module PolicyCombiners = 

  /// An policy combinator environment which stores XACML policy combination algorithms used in policySets
  let policyCombEnv = new Dictionary<string, PolicyCombiningAlgF>()

  let rec relevant d obls = 
    match d, obls with
      | _, [] -> []
      | Indeterminate, _ -> []
      | decision, (Obligation (effect,s))::obls -> if decision = effect
                                                    then (Obligation (effect,s)) :: relevant decision obls
                                                    else relevant decision obls

  let combine obls eObs = 
     match eObs with (effect, obs) -> (effect, relevant effect (obs @ obls))

  let rec policyDenyOverrides (ps: (PredicateF * PolicyF) list) (obls: Obligation list) (req: RequestContext) =
    let check pObl dObl = 
      match pObl, dObl with
        | (Permit,obs), (NotApplicable,obs') -> (Permit,obs @ obs')
        | (Permit,obs), (d,obs') -> (d,obs')
        | (Deny,obs), (_,obs') -> (Deny,obs @ obs')
        | (Indeterminate,[]), (_,obs') -> (Deny,obs')
        | (NotApplicable,[]), (d,obs') -> (d,obs')
        | _, _ -> (Indeterminate,[]) 
    match ps with
      | [] -> (NotApplicable, [])
      | (targ,pf) :: ps -> combine obls (check (pf req) (policyDenyOverrides ps obls req))

  let rec policyPermitOverrides (ps: (PredicateF * PolicyF) list) (obls: Obligation list) (req: RequestContext) =
    let check pObl dObl =
      match pObl, dObl with
        | (Deny,obs), (NotApplicable,obs') -> (Deny,obs @ obs')
        | (Deny,obs), (d,obs') -> (d,obs')
        | (Permit,obs), (_,obs') -> (Permit,obs @ obs')
        | (Indeterminate,[]), (_,obs') -> (Indeterminate,[])
        | (NotApplicable,[]), (d,obs') -> (d,obs')
        | _, _ -> (Indeterminate,[])
    match ps with
      | [] -> (NotApplicable, [])
      | (targ,pf) :: ps -> combine obls (check (pf req) (policyPermitOverrides ps obls req))
    
  let rec policyFirstApplicable (ps: (PredicateF * PolicyF) list) (obls: Obligation list) (req: RequestContext) =
    let check pObl dObl =
      match pObl, dObl with
        | (Permit,obs), _ -> (Permit,obs)
        | (Deny,obs), _ -> (Deny,obs)
        | (Indeterminate,[]), _ -> (Indeterminate,[])
        | (NotApplicable,[]), next -> next
        | _, _ -> (Indeterminate,[])
    match ps with 
      | [] -> (NotApplicable, [])
      | (targ,pf) :: ps -> combine obls (check (pf req) (policyFirstApplicable ps obls req))

  let rec policyOnlyOneApplicable (ps: (PredicateF * PolicyF) list) (obls: Obligation list) (req: RequestContext) =
    let check vPf =
      match vPf with
        | ((BoolAtomValue true), policyf) -> policyf req
        | ((BoolAtomValue false), policyf) -> policyf req
        | _ -> (Indeterminate, [])
    let notAppl = (BoolAtomValue false, (fun r -> (NotApplicable,[])))
    let indeterm = (IndeterminateValue, (fun r -> (Indeterminate,[])))
    let traverse vp rest = 
      match rest, vp with 
        | ((BoolAtomValue true),_), ((BoolAtomValue true),_) -> indeterm
        | ((BoolAtomValue true),p), _ -> ((BoolAtomValue true),p)
        | ((BoolAtomValue false),_), rest -> rest
        | _, _ -> indeterm
    combine obls (check (List.foldBack traverse (List.map (fun (t,pf) -> (t req, pf)) ps) notAppl))

  // ------------ Do the policy combiner environment filling ------------
  policyCombEnv.Add("deny-overrides", policyDenyOverrides)
  policyCombEnv.Add("permit-overrides", policyPermitOverrides)
  policyCombEnv.Add("first-applicable", policyFirstApplicable)
  policyCombEnv.Add("only-one-applicable", policyOnlyOneApplicable)

