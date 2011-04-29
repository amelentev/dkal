namespace Microsoft.Research.Dkal.Executor.Simple

open System.Collections.Generic

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast

/// Keeps a set of incoming infon MetaTerms until they are no longer needed
/// and/or become too old, and/or irrelevant
type Quarantine() =
  let msgs = new HashSet<ITerm>()

  /// Move incoming messages to quarantine and split Ands. The optional prefix
  /// is used to split ands recursively inside quotations
  member q.Add (msg: ITerm, ?prefix: ITerm list) =
    let prefix =  match prefix with
                  | None -> []
                  | Some prefix -> prefix
    match msg.Normalize() with
    | AndInfon(msgs) -> List.iter (fun msg -> q.Add(msg, prefix)) msgs
    | SaidInfon(ppal, msg) -> q.Add(msg, prefix @ [ppal])
    | _ -> 
      msgs.Add(PrefixedInfon(prefix, msg)) |> ignore

  /// Called at the end of each round to eliminate "old" messages
  member q.Prune () = 
    // TODO: implement some algorithm to remove unnecessary/old messages
    () 

  /// Match a wire condition (infon) to messages in quarantine. It returns a 
  /// subset of (possibly specialized) substitutions The optional prefix
  /// is used to match ands recursively inside quotations
  member q.Matches (infon: ITerm, substs: ISubstitution list, ?prefix: ITerm list) =
    let prefix =  match prefix with
                  | None -> []
                  | Some prefix -> prefix
    match infon.Normalize() with
    | EmptyInfon -> substs
    | AsInfon(_) -> failwith "Trying to match asInfon(...) on wire"
    | AndInfon(infons) -> 
      [ for subst in substs do
          yield! q.MatchesMany([for infon in infons -> infon.Apply subst], subst, prefix) ]
    | SaidInfon(ppal, infon) ->
      q.Matches(infon, substs, prefix @ [ppal])
    | infon -> 
      [ for subst in substs do
          yield! q.MatchesMany([infon.Apply subst], subst, prefix) ]
      
  /// Match a list of wire conditions to messages in quarantine with an 
  /// initial substitution and returning all possible substitutions (if any)
  member private q.MatchesMany (infons: ITerm list, subst: ISubstitution, prefix: ITerm list) =
    match infons with
    | [] -> [subst]
    | infon :: infons ->
      let substInfon = (PrefixedInfon(prefix,infon)).Apply subst
      msgs |> Seq.toList 
           |> List.collect 
                (fun msg ->
                  match (msg.Apply subst).Unify substInfon with
                  | None -> []
                  | Some subst' -> q.MatchesMany(infons, (subst'.ComposeWith subst), prefix))

  