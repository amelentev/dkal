namespace Microsoft.Research.GeneralPDP.Data

open System
open System.Collections.Generic

module Ast = 
  
  type DocumentId = string
  type LabelId = string
  type EndPointId = string
  type PolicyId = string


  type PolicyPointer = 
  | GlobalPolicyPointer of PolicyId * EndPointId
  | DelegationPolicyPointer of EndPointId
  with
    static member Parse (s: string) = 
      match List.ofArray (s.Split [|'@'|]) with 
      | [policyId; epId] -> GlobalPolicyPointer(policyId, epId)
      | [epId] -> DelegationPolicyPointer(epId)
      | _ -> failwith ("Invalid policy pointer: " + s)
    override pp.ToString() = 
      match pp with
      | GlobalPolicyPointer(pid, epid) -> pid + "@" + epid
      | DelegationPolicyPointer(epid) -> epid


  type LabelInfo(pdps: EndPointId seq, policyPointer: PolicyPointer) = 
    member li.Pdps = pdps
    member li.PolicyPointer = policyPointer


  type DocumentInfo(content: string, ?labels: LabelId seq) =
    let mutable content = content
    let labels = new HashSet<LabelId>(match labels with
                                      | None -> seq []
                                      | Some ls -> ls)
    member di.Content with get() = content and set(c) = content <- c
    member di.Labels = labels
    override di.ToString() = 
      "\"" + di.Content + "\"\r\nlabels: " + String.concat ", " di.Labels
