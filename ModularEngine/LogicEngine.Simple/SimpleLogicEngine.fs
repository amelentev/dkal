namespace Microsoft.Research.Dkal.LogicEngine.Simple

open System.Collections.Generic

open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Substrate

/// The SimpleEngine uses backwards propagation to derive all possible 
/// Substitutions that will satisfy the given query. Each Substitution will 
/// have an accompanying list of side conditions to be checked against the 
/// substrate(s). Only those Substitutions that pass the side conditions are
/// returned
type SimpleLogicEngine() = 

  /// Stores the known facts
  let knowledge = new HashSet<ITerm>()

  interface ILogicEngine with
    member se.Start () = ()
    member se.Stop () = ()

    /// Split the infon into conjunctions and learn these recursively
    member se.Learn (infon: ITerm) = 
      match infon.Normalize() with
      | EmptyInfon -> false
      | AsInfon(_) -> failwith "Engine is trying to learn asInfon(...)"
      | AndInfon(infons) ->
        List.fold (fun ch i -> 
                    let ch' = (se :> ILogicEngine).Learn i
                    ch' || ch) false infons 
      | infon -> 
        knowledge.Add infon

    /// Split the infon into conjunctions and forget these recursively
    member se.Forget (infon: ITerm) =
      match infon.Normalize() with
      | EmptyInfon -> false
      | AsInfon(_) -> failwith "Engine is trying to forget asInfon(...)"
      | AndInfon(infons) ->
        List.fold (fun ch i -> 
                    let ch' = (se :> ILogicEngine).Forget i
                    ch' || ch) false infons 
      | infon -> 
        knowledge.Remove infon

    /// Obtain a list of Substitution with accompanying side conditions (AsInfon
    /// MetaTerms). Then return only those Substitutions that satisfy all their 
    /// side conditions.
    member se.Derive (target: ITerm, substs: ISubstitution list) = 
      [ for subst in substs do      
          for (subst, conds) in se.DoDerive [] (subst, []) (target.Normalize()) do
            for subst' in SubstrateDispatcher.Solve conds [subst] do
              yield subst' ]

  /// Given a prefix (list of principal MetaTerms) a current Substitution with 
  /// side conditions (AsInfo MetaTerms) and a target infon MetaTerm to derive
  /// this method will recursively derive the target infon depending on its
  /// structure.
  member private se.DoDerive (pref: ITerm list) ((subst, conds): ISubstitution * ISubstrateQueryTerm list) (infon: ITerm) = 
    match infon with
    | AndInfon(infons) -> 
      // In the case of conjunction we start with the current substitution and side conditions and 
      // continue accumulating these by calling recursively on each of the infons in the conjunction
      List.fold (fun substs infon -> List.collect (fun s -> se.DoDerive pref s infon) substs) [(subst, conds)] infons
    | EmptyInfon -> 
      // Empty infon is always satisfiable by the current substitution and side conditions
      [(subst, conds)]
    | SaidInfon(ppal, infon) ->
      // Said infons are handled recursively by pushing the principal term into the prefix
      se.DoDerive (ppal :: pref) (subst, conds) infon
    | Var(v) when subst.Contains v ->
      // If a variable is part of the current substitution it is applied and we call recursively
      se.DoDerive pref (subst, conds) (subst.Apply v)
    | AsInfon(exp) ->
      // AsInfon(...) is stored as a new side condition, unless it is inside a non-empty prefix
      if pref.IsEmpty then
        [(subst, conds @ [exp])]
      else
        failwith "asInfon(...) under prefix"
    | templ ->
      // For every other case we call se.InfonsWithPrefix(..) which will give us a list of
      // substitutions, each of which will have a list of infon MetaTerms (preconditions) that 
      // need to be  satisfied in order for that substitution to be returned. This is were the 
      // backwards chaining happens, since we recursively check all the preconditions one by one 
      // (with checkOne)
      let rec checkOne = function
          | (substConds, pre :: pres) -> 
            se.DoDerive [] substConds pre |> List.collect (fun s -> checkOne (s, pres))
          | (substConds, []) ->
            [substConds]
      se.InfonsWithPrefix subst pref templ 
        |> List.map (fun (s, ps) -> ((s, conds), ps))
        |> List.collect checkOne

  /// Given a current Substitution, a prefix (list of principal MetaTerms) and 
  /// a template infon MetaTerm to derive this method will return a list of 
  /// Substitutions that satisfy the given template, each of which will have a
  /// list of preconditions (infon MetaTerms) that need to be verified in order
  /// for that Substitution to be a real solution
  member se.InfonsWithPrefix (subst: ISubstitution) (pref: ITerm list) (template: ITerm) =
    let res = ref []
    let rec stripPrefix subst prefixUnif preconds suff = 
      let immediate = function
        | ([], i) ->
          let rec unifyAndSimpl (s: ISubstitution option) (ts: (ITerm * ITerm) list) = 
            match ts with
            | [] -> s
            | (a, b) :: ts ->
              match s with
                | None -> None
                | Some s -> unifyAndSimpl ((a.Apply s).UnifyFrom s (b.Apply s)) ts
          match unifyAndSimpl (Some subst) ((template, i) :: prefixUnif) with
            | Some subst ->
              res := (subst, preconds) :: !res
            | None -> ()
        | _ -> ()
             
      function
      | ((t1: ITerm) :: pref, SaidInfon (t2, i)) ->
        match (t1.Apply subst).UnifyFrom subst  (t2.Apply subst) with
          | Some subst -> 
            stripPrefix subst prefixUnif preconds (fun i -> suff (SaidInfon (t2, i))) (pref, i)
          | None ->
            stripPrefix subst ((t1, t2) :: prefixUnif) preconds (fun i -> suff (SaidInfon (t2, i))) (pref, i)
      | (pref, AndInfon(infons)) ->
        List.iter (fun infon -> 
                    stripPrefix subst prefixUnif preconds suff (pref, infon)) infons
      | (pref, ImpliesInfon(a, b)) as t ->
        immediate t
        stripPrefix subst prefixUnif (suff a :: preconds) suff (pref, b)
      | (pref, Var v) when subst.Contains v ->
        stripPrefix subst prefixUnif preconds suff (pref, subst.Apply v)
      | t -> immediate t
    
    List.iter (fun k -> stripPrefix subst [] [] (fun x -> x) (pref, k)) (knowledge |> Seq.toList)
    !res