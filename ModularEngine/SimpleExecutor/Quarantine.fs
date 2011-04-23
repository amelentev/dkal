namespace Microsoft.Research.Dkal.SimpleExecutor

open System.Collections.Generic

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast

/// Keeps a set of incoming infon MetaTerms until they are no longer needed
/// and/or become too old, and/or irrelevant
type Quarantine() =
  let msgs = new HashSet<ITerm>()

  /// Move incoming messages to quarantine and split Ands
  member q.Add (msg: ITerm) (from: ITerm) =
    match msg with
    | AndInfon(msgs) -> List.iter (fun msg -> q.Add msg from) msgs
    | _ -> msgs.Add (SaidInfon(from, msg)) |> ignore

  /// Called at the end of each round to eliminate "old" messages
  member q.Prune () = 
    // TODO: implement some algorithm to remove unnecessary/old messages
    () 

  /// Match a wire condition (infon) to messages in quarantine
  member q.Matches (infon: ITerm) =
    match Normalizer.normalize infon with
    | EmptyInfon -> [Substitution.Id]
    | AsInfon(_) -> failwith "Trying to match asInfon(...) on wire"
    | AndInfon(infons) -> 
      q.MatchesMany infons Substitution.Id
    | infon -> 
      q.MatchesMany [infon] Substitution.Id
      
  /// Match a list of wire conditions to messages in quarantine with an 
  /// initial substitution and returning all possible substitutions (if any)
  member private q.MatchesMany (infons: ITerm list) (subst: ISubstitution) =
    match infons with
    | [] -> [subst]
    | infon :: infons ->
      let substInfon = infon.Apply subst
      msgs |> Seq.toList 
           |> List.collect 
                (fun msg ->
                  match (msg.Apply subst).Unify substInfon with
                  | None -> []
                  | Some subst' -> q.MatchesMany infons (subst'.ComposeWith subst))
