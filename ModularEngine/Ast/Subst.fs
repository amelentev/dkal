﻿namespace Microsoft.Research.Dkal.Ast

  type Substitution(subst : Variable -> MetaTerm) = 
    
    /// Returns a new Substitution that behaves like the identity
    static member Id = 
      new Substitution(fun v -> Var v)

    /// Returns a new Substitution that results from extending the current 
    /// Substitution so that it maps x to mt and leaves the rest unchanged
    member s.Extend (x: Variable, mt: MetaTerm) = 
      new Substitution(fun z -> if z = x then mt else subst z)
       
    /// Applies this Substitution to the given MetaTerm
    member s.Apply (mt: MetaTerm) = 
      match mt with
      | Var(v) -> subst v
      | App(f, mts) -> App(f, List.map s.Apply mts)
      | _ -> mt             
      
    /// Returns a new Substitution that results from first applying s' and 
    /// then applying the current Substitution
    member s.ComposeWith (s': Substitution) =
      new Substitution(fun z -> s.Apply(s'.Apply(Var z)))

    /// Returns true iff v is affected by this Substitution
    member s.Contains (v: Variable) =
      subst v <> Var v

    /// Returns a Substitution that makes mt1 and mt2 syntactically equal, 
    /// if possible; None otherwise
    static member Unify (mt1: MetaTerm) (mt2: MetaTerm) =
      Substitution.UnifyFrom (Substitution.Id) mt1 mt2

    /// Returns a Substitution that makes subst(mt1) and subst(mt2) syntactically 
    /// equal, if possible; None otherwise
    static member UnifyFrom (subst: Substitution) (mt1: MetaTerm) (mt2: MetaTerm) =
      let rec traverse (subst: Substitution option) mt1 mt2 = 
        match subst with
        | None -> None
        | Some s -> 
          match mt1, mt2 with
          | mt1, mt2 when mt1 = mt2 -> Some s 
          | Var v1, (mt2: MetaTerm) when not <| mt2.Vars.Contains(v1) -> 
            let substMt = s.Apply (Var v1)
            if substMt = mt2 then
              Some s
            elif substMt = Var v1 then
              Some (s.Extend (v1, mt2))
            else
              None
          | mt1, Var v2 -> traverse (Some s) (Var v2) mt1 
          | App(f1, mts1), App(f2, mts2) when f1 = f2 && mts1.Length = mts2.Length -> 
            List.fold2 traverse (Some s) mts1 mts2
          | _ -> None
      traverse (Some subst) mt1 mt2 