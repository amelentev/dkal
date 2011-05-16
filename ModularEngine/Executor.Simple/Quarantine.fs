namespace Microsoft.Research.Dkal.Executor.Simple

open System.Collections.Generic

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast

/// Keeps a set of incoming infon MetaTerms until they are no longer needed
/// and/or become too old, and/or irrelevant
type Quarantine(logicEngine: ILogicEngine) =
  
  /// Keeps the infons that are stored as keys. The values are the (possibly
  /// not present) justifications for those infons
  let msgs = new Dictionary<ITerm, ITerm option>()

  /// Fresh variable id
  let mutable freshVarId = 0

  /// Move incoming messages to quarantine
  member q.Add (msg: ITerm, from: ITerm) =
    match msg with
    | JustifiedInfon(infon, ev) -> 
      match logicEngine.CheckJustification(ev) with
      | Some infon' -> 
        //when infon = infon' -> 
        match infon.Unify infon' with
        | Some subst -> 
          if subst.IsVariableRenaming then
            q.DoAdd(infon, Some ev)
          else
            failwithf "(Well-formed) evidence in justified message does not match the message contents.\r\nEvidence:\r\n%O\r\nJustifies:\r\n%O\r\nBut received:\r\n%O" ev infon' infon
        | None -> failwithf "(Well-formed) evidence in justified message does not match the message contents.\r\nEvidence:\r\n%O\r\nJustifies:\r\n%O\r\nBut received:\r\n%O" ev infon' infon
      | _ -> failwithf "Fake evidence found on %O" msg
    | AndInfon(msgs) ->
      List.iter (fun (msg: ITerm) -> q.Add(msg, from)) msgs
    | _ -> q.DoAdd(SaidInfon(from, msg), None)

  /// Split ands recursively inside the message (even inside prefixes)
  member private q.DoAdd (msg: ITerm, evidence: ITerm option, ?prefix: ITerm list) =
    let prefix =  match prefix with
                  | None -> []
                  | Some prefix -> prefix
    match msg.Normalize() with
    | AndInfon(msgs) -> 
      match evidence with 
      | None -> 
        List.iter (fun msg -> q.DoAdd(msg, None, prefix)) msgs
      | Some(AndEvidence(evidences)) when msgs.Length = evidences.Length ->
        List.iter2 (fun msg ev -> q.DoAdd(msg, Some ev, prefix)) msgs evidences
      | _ -> failwithf "Mailformed conjunction evidence on %O" msg
    | SaidInfon(ppal, msg) -> q.DoAdd(msg, evidence, prefix @ [ppal])
    | _ -> 
      msgs.[PrefixedInfon(prefix, msg)] <- evidence

  /// Remove from quarantine (splitting Ands). The optional prefix
  /// is used to split ands recursively inside quotations
  member q.Remove (msg: ITerm, ?prefix: ITerm list) =
    let prefix =  match prefix with
                  | None -> []
                  | Some prefix -> prefix
    match msg.Normalize() with
    | AndInfon(msgs) -> List.iter (fun msg -> q.Remove(msg, prefix)) msgs
    | SaidInfon(ppal, msg) -> q.Remove(msg, prefix @ [ppal])
    | JustifiedInfon(AndInfon(msgs), AndEvidence(evidences)) -> 
      if msgs.Length = evidences.Length then
        List.iter2 (fun msg ev -> q.Remove(JustifiedInfon(msg, ev), prefix)) msgs evidences
    | _ -> 
      msgs.Remove(PrefixedInfon(prefix, msg)) |> ignore

  /// Called at the end of each round to eliminate "old" messages
  member q.Prune () = 
    // TODO: implement some algorithm to remove unnecessary/old messages
    () 

  /// Match a wire condition (infon) to messages in quarantine. It returns a 
  /// subset of (possibly specialized) substitutions. 
  member q.Matches (infon: ITerm) (substs: ISubstitution seq) =
    q.DoMatches(infon, None, substs, [])
  
  /// Performs the actual matching. The proofPattern is used to match only 
  /// justified infons that have a proof matching the pattern. The prefix 
  /// is used to match recursively inside quotations
  member private q.DoMatches (infon: ITerm, proofPattern: ITerm option, substs: ISubstitution seq, prefix: ITerm list) =
    match infon.Normalize() with
    | EmptyInfon -> substs
    | AsInfon(_) -> failwith "Trying to match asInfon(...) on wire"
    | AndInfon(infons) -> 
      match proofPattern with 
      | Some proofPattern -> 
        let vars = [for i in [1..infons.Length] -> q.FreshVar Type.Evidence]
        match proofPattern.Unify (AndEvidence(vars)) with
        | Some subst' -> 
          let substs = seq { for subst in substs -> subst.ComposeWith subst' }
          List.fold2 (fun substs infon var -> q.DoMatches(infon, Some var, substs, prefix)) substs infons vars
        | None -> seq []
      | None -> 
        List.fold (fun substs infon -> q.DoMatches(infon, proofPattern, substs, prefix)) substs infons
    | SaidInfon(ppal, infon) ->
      q.DoMatches(infon, proofPattern, substs, prefix @ [ppal])
    | JustifiedInfon(infon, ev) ->
      match proofPattern with 
      | None -> q.DoMatches(infon, Some ev, substs, prefix)
      | Some p -> failwith "Trying to match nested justifications on wire"
    | infon -> 
      seq { for subst in substs do
              let substInfon = (PrefixedInfon(prefix,infon)).Apply subst
              for kvp in msgs do
                match substInfon.UnifyFrom subst (kvp.Key.Apply subst) with
                | None -> ()
                | Some subst -> 
                  match proofPattern with
                  | None -> yield subst 
                  | Some proofPattern ->
                    match kvp.Value with
                    | None -> ()
                    | Some proof -> 
                      match proofPattern.UnifyFrom subst proof with
                      | None -> ()
                      | Some subst -> yield subst }

  member private q.FreshVar (t: IType) =
    let ret = {Name = "Var#" + freshVarId.ToString(); Type = t}
    freshVarId <- freshVarId + 1
    Var(ret)

