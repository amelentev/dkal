namespace Microsoft.Research.Dkal.SimpleEngine

open System.Collections.Generic

open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Interfaces

type Prefix = 
| Said of MetaTerm

type SimpleEngine() = 
  let knowledge = new HashSet<MetaTerm>()

  interface IEngine with
    member se.Start () = ()
    member se.Stop () = ()

    member se.Knowledge () = knowledge |> Seq.toList

    member se.Derive (target: MetaTerm) = 
      se.DoDerive [] (Substitution.Id, []) (Normalizer.normalize target)

    member se.Learn (infon: MetaTerm) = 
      match Normalizer.normalize infon with
      | EmptyInfon -> false
      | AsInfon(_) -> failwith "Engine is trying to learn asInfon(...)"
      | AndInfon(infons) -> 
        List.exists (se :> IEngine).Learn infons
      | infon -> 
        knowledge.Add infon

    member se.AreConsistentActions (actions: MetaTerm list) = 
      // TODO: implement
      // TODO: maybe move to another module
      true
      
  member private se.DoDerive (pref: Prefix list) ((subst, conds): Substitution * MetaTerm list) (infon: MetaTerm) = 
    match infon with
    | AndInfon(infons) -> 
      List.fold (fun substs infon -> List.collect (fun s -> se.DoDerive pref s infon) substs) [(subst, conds)] infons
    | EmptyInfon -> [(subst, conds)]
    | SaidInfon(ppal, infon) ->
      se.DoDerive (Said ppal :: pref) (subst, conds) infon
    | Var(v) when subst.Contains v ->
      se.DoDerive pref (subst, conds) (subst.Apply <| Var(v))
    | AsInfon(exp, substrate) ->
      if pref.IsEmpty then
        [(subst, conds @ [infon])]
      else
        failwith "asInfon(...) under prefix"
    | templ ->
      let rec checkOne = function
          | (substConds, pre :: pres) -> 
            se.DoDerive [] substConds pre |> List.collect (fun s -> checkOne (s, pres))
          | (substConds, []) ->
            [substConds]
      se.InfonsWithPrefix subst pref templ 
        |> List.map (fun (s, ps) -> ((s, conds), ps))
        |> List.collect checkOne

  member se.InfonsWithPrefix (subst: Substitution) (pref: Prefix list) (template: MetaTerm) =
    let res = ref []
    let rec stripPrefix subst prefixUnif preconds suff = 
      let immediate = function
        | ([], i) ->
          let rec unifyAndSimpl s = function
            | [] -> s
            | (a, b) :: xs ->
              match s with
                | None -> None
                | Some s -> unifyAndSimpl (Substitution.UnifyFrom s (s.Apply a) (s.Apply b)) xs
          match unifyAndSimpl (Some subst) ((template, i) :: prefixUnif) with
            | Some subst ->
              res := (subst, preconds) :: !res
            | None -> ()
        | _ -> ()
             
      function
      | (Said t1 :: pref, SaidInfon (t2, i)) ->
        match Substitution.UnifyFrom subst (subst.Apply t1) (subst.Apply t2) with
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
        stripPrefix subst prefixUnif preconds suff (pref, subst.Apply (Var v))
      | t -> immediate t
    
    List.iter (fun k -> stripPrefix subst [] [] (fun x -> x) (pref, k)) (knowledge |> Seq.toList)
    !res