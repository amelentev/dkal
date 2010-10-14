namespace Microsoft.Research.GeneralPDP.Translations.ToDKAL

open Microsoft.Research.GeneralPDP.XACML.Ast

open System.Collections.Generic

module Crawlers =

  let attributesInExpression (e: Expression) = 
    let result = HashSet<AttributeDesignator>()
    let rec crawlE e = 
      match e with
      | AttributeDesignatorExp(_) -> result.Add(e) |> ignore
      | ApplyExp(_, es) -> for e in es do crawlE e
      | _ -> ()
    crawlE e
    result

  let attributesInPolicy (p: Policy) =
    let result = HashSet<AttributeDesignator>()
    let rec crawlP p = 
      match p with
      | Policy(_, t, _, rs, _) -> crawlT t; for r in rs do crawlR r
      | PolicySet(_, t, _, ps, _) -> crawlT t; for p in ps do crawlP p
    and crawlT t =
      match t with
      | None -> ()
      | Some t -> for s in t.Subjects do for sm in s.SubjectMatchs do crawlSM sm
                  for r in t.Resources do for rm in r.ResourceMatchs do crawlRM rm
                  for a in t.Actions do for am in a.ActionMatchs do crawlAM am
    and crawlSM sm =
      match sm.Designator with
      | AttributeDesignatorExp(_) -> result.Add(sm.Designator) |> ignore
      | _ -> failwith "Expecting designator expression"
    and crawlRM rm =
      match rm.Designator with
      | AttributeDesignatorExp(_) -> result.Add(rm.Designator) |> ignore
      | _ -> failwith "Expecting designator expression"
    and crawlAM am =
      match am.Designator with
      | AttributeDesignatorExp(_) -> result.Add(am.Designator) |> ignore
      | _ -> failwith "Expecting designator expression"
    and crawlR r = crawlT r.Target
                   match r.Condition with 
                   | Some c -> result.UnionWith(attributesInExpression c)
                   | None -> ()
    crawlP p
    result
