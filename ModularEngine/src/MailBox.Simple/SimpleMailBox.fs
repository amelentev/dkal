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

namespace Microsoft.Research.Dkal.MailBox.Simple

open System.Collections.Generic
open NLog

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast

/// Keeps a set of incoming infons until they are no longer needed
/// and/or become too old, and/or irrelevant
type SimpleMailBox(logicEngine: ILogicEngine) =
  
  let log = LogManager.GetLogger("MailBox.Simple")

  /// Keeps the infons that are stored as keys. The values are (s, e) where
  /// s is the message sender and e is the message evidence (possibly not
  /// present) 
  let msgs = new Dictionary<ITerm, ITerm * ITerm option>()

  /// Fresh variable id
  let mutable freshVarId = 0

  interface IMailBox with
  
    /// Move incoming messages to the mailbox
    member smb.Add (msg: ITerm) (from: ITerm) =
      match msg with
      | JustifiedInfon(infon, ev) -> 
        match logicEngine.CheckJustification(ev) with
        | Some infon' -> 
          //when infon = infon' -> 
          match infon.Unify infon' with
          | Some subst -> 
            if subst.IsVariableRenaming then
              smb.DoAdd(infon, from, Some ev)
            else
              failwithf "(Well-formed) evidence in justified message does not match the message contents.\r\nEvidence:\r\n%O\r\nJustifies:\r\n%O\r\nBut received:\r\n%O" ev infon' infon
          | None -> failwithf "(Well-formed) evidence in justified message does not match the message contents.\r\nEvidence:\r\n%O\r\nJustifies:\r\n%O\r\nBut received:\r\n%O" ev infon' infon
        | _ -> failwithf "Fake evidence found on %O" msg
      | AndInfon(msgs) ->
        List.iter (fun (msg: ITerm) -> (smb :> IMailBox).Add msg from) msgs
      | _ -> smb.DoAdd(SaidInfon(from, msg), from, None)

    /// Remove from the mailbox (splitting Ands). The optional prefix
    /// is used to split ands recursively inside quotations
    member smb.Remove (msg: ITerm, ?prefix: ITerm list) =
      let prefix =  match prefix with
                    | None -> []
                    | Some prefix -> prefix
      match msg.Normalize() with
      | AndInfon(msgs) -> List.iter (fun msg -> (smb :> IMailBox).Remove(msg, prefix)) msgs
      | SaidInfon(ppal, msg) -> (smb :> IMailBox).Remove(msg, prefix @ [ppal])
      | JustifiedInfon(AndInfon(msgs), AndEvidence(evidences)) -> 
        if msgs.Length = evidences.Length then
          List.iter2 (fun msg ev -> (smb :> IMailBox).Remove(JustifiedInfon(msg, ev), prefix)) msgs evidences
      | _ -> 
        msgs.Remove(PrefixedInfon(prefix, msg)) |> ignore

    /// Called at the end of each round to eliminate messages
    member smb.Prune () = 
      msgs.Clear()

    /// Match a wire condition (infon) to messages in the mailbox. It returns a 
    /// subset of (possibly specialized) substitutions. 
    member smb.Matches (infon: ITerm) (from: ITerm) (substs: ISubstitution seq) =
      smb.DoMatches(infon, from, None, substs, [])

  /// Split ands recursively inside the message (even inside prefixes)
  member private smb.DoAdd (msg: ITerm, from: ITerm, evidence: ITerm option, ?prefix: ITerm list) =
    let prefix =  match prefix with
                  | None -> []
                  | Some prefix -> prefix
    match msg.Normalize() with
    | AndInfon(msgs) -> 
      match evidence with 
      | None -> 
        List.iter (fun msg -> smb.DoAdd(msg, from, None, prefix)) msgs
      | Some(AndEvidence(evidences)) when msgs.Length = evidences.Length ->
        List.iter2 (fun msg ev -> smb.DoAdd(msg, from, Some ev, prefix)) msgs evidences
      | _ -> failwithf "Malformed conjunction evidence on %O" msg
    | SaidInfon(ppal, msg) when evidence.IsNone -> smb.DoAdd(msg, from, evidence, prefix @ [ppal])
    | _ -> 
      msgs.[PrefixedInfon(prefix, msg)] <- (from, evidence)

  /// Performs the actual matching. The proofPattern is used to match only 
  /// justified infons that have a proof matching the pattern. The prefix 
  /// is used to match recursively inside quotations
  member private smb.DoMatches (infon: ITerm, from: ITerm, proofPattern: ITerm option, substs: ISubstitution seq, prefix: ITerm list) =
    match infon.Normalize() with
    | EmptyInfon -> substs
    | AsInfon(_) -> failwith "Trying to match asInfon(...) on wire"
    | AndInfon(infons) -> 
      match proofPattern with 
      | Some proofPattern -> 
        let vars = [for i in [1..infons.Length] -> smb.FreshVar Type.Evidence]
        match proofPattern.Unify (AndEvidence(vars)) with
        | Some subst' -> 
          let substs = seq { for subst in substs -> subst.ComposeWith subst' }
          List.fold2 (fun substs infon var -> smb.DoMatches(infon, from, Some var, substs, prefix)) substs infons vars
        | None -> seq []
      | None -> 
        List.fold (fun substs infon -> smb.DoMatches(infon, from, proofPattern, substs, prefix)) substs infons
    | SaidInfon(ppal, infon) ->
      smb.DoMatches(infon, from, proofPattern, substs, prefix @ [ppal])
    | JustifiedInfon(infon, ev) ->
      match proofPattern with 
      | None -> smb.DoMatches(infon, from, Some ev, substs, prefix)
      | Some p -> failwith "Trying to match nested justifications on wire"
    | infon -> 
      seq { for subst in substs do
              let infon = PrefixedInfon(prefix,infon)
              for kvp in msgs do
                match kvp.Key.UnifyFrom subst infon with
                | None -> ()
                | Some subst -> 
                  let (sender, ev) = kvp.Value
                  match sender.UnifyFrom subst from with
                  | None -> ()
                  | Some subst -> 
                    match proofPattern with
                    | None -> yield subst 
                    | Some proofPattern ->
                      match ev with
                      | None -> ()
                      | Some proof -> 
                        match proofPattern.UnifyFrom subst proof with
                        | None -> ()
                        | Some subst -> yield subst }

  member private smb.FreshVar (t: IType) =
    let ret = {Name = "Var#" + freshVarId.ToString(); Type = t}
    freshVarId <- freshVarId + 1
    Var(ret)

