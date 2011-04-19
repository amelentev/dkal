namespace Microsoft.Research.Dkal.SimpleExecutor

open System.Collections.Generic

open Microsoft.Research.Dkal.Ast

/// Keeps a set of incoming infon MetaTerms until they are no longer needed
/// and/or become too old, and/or irrelevant
type Quarantine() =
  let msgs = new HashSet<MetaTerm>()

  /// Move incoming messages to quarantine and split Ands
  member q.Add (msg: MetaTerm) (from: MetaTerm) =
    match msg with
    | AndInfon(msgs) -> List.iter (fun msg -> q.Add msg from) msgs
    | _ -> msgs.Add (App(primitives.["saidInfon"], [from; msg])) |> ignore

  /// Called at the end of each round to eliminate "old" messages
  member q.Prune () = 
    // TODO: implement some algorithm to remove unnecessary/old messages
    () 

  /// Match a wire condition (infon) to messages in quarantine
  member q.Matches (infon: MetaTerm) =
    match Normalizer.normalize infon with
    | EmptyInfon -> [Substitution.Id]
    | AsInfon(_) -> failwith "Trying to match asInfon(...) on wire"
    | AndInfon(infons) -> 
      q.MatchesMany infons Substitution.Id
    | infon -> 
      q.MatchesMany [infon] Substitution.Id
      
  /// Match a list of wire conditions to messages in quarantine with an 
  /// initial substitution and returning all possible substitutions (if any)
  member private q.MatchesMany (infons: MetaTerm list) (subst: Substitution) =
    match infons with
    | [] -> [subst]
    | infon :: infons ->
      let substInfon = subst.Apply infon
      msgs |> Seq.toList 
           |> List.collect 
                (fun msg ->
                  match Substitution.Unify (subst.Apply msg) substInfon with
                  | None -> []
                  | Some subst' -> q.MatchesMany infons (subst'.ComposeWith subst))
