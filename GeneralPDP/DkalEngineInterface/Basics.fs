namespace Microsoft.Research.GeneralPDP.DKAL.Engine

open ParsingCtxFactory
open Microsoft.Research.DkalEngine.Ast
open Microsoft.Research.GeneralPDP.Utils.String
open Microsoft.Research.GeneralPDP.XACML.Ast

module Basics = 

  type DkalPolicy(pid: PolicyId, assertions: Assertion list) =
    member dp.PolicyId = pid
    member dp.Assertions = assertions
    override dp.ToString() =
      "DKALPolicy \"" + pid + "\"\r\n" + 
         String.concat "\r\n" (List.map (fun (a: Assertion) -> winEndOfLines (a.ToPrettyString())) assertions)

  type DkalPolicyRequest(pid: PolicyId) = 
    member dpr.PolicyId = pid
    override dp.ToString() =
      "DKALPolicyRequest \"" + pid + "\""
    static member ParseFrom (s: string) = 
      if s.StartsWith("DKALPolicyRequest \"") && s.EndsWith("\"") then
        let offset = "DKALPolicyRequest \"".Length
        let pid = s.Substring(offset, s.Length - offset - 1)
        DkalPolicyRequest(pid)
      else
        failwith "Malformed dkal policy request"
