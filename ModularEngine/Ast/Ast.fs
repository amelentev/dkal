namespace Microsoft.Research.Dkal.Ast

  open System.Collections.Generic

  type Type = 
  | Bool
  | Int
  | Float
  | String
  | Principal
  | Infon

  type Constant = 
    | BoolConstant of bool
    | IntConstant of int
    | FloatConstant of float
    | StringConstant of string
  with 
    member c.Typ = 
      match c with
      | BoolConstant(_) -> Bool
      | IntConstant(_) -> Int
      | FloatConstant(_) -> Float
      | StringConstant(_) -> String

  type Variable = { Name: string; 
                    Typ: Type }

  type Function = { Name: string; 
                    RetTyp: Type; 
                    ArgsTyp: Type list;
                    Body: MetaTerm option }
  
  and Substitution = Dictionary<Variable, MetaTerm>

  and MetaTerm = 
    | App of Function * MetaTerm list
    | Const of Constant
    | Var of Variable
  with 

    /// Returns the type of the MetaTerm
    member mt.Typ() = 
      match mt with
      | App(f, mts) -> f.RetTyp
      | Const(c) -> c.Typ
      | Var({Name = _; Typ = t}) -> t

    /// Checks that the MetaTerm is correctly typed
    member mt.CheckTyp() = 
      match mt with
      | App(f, mts) -> 
        let foundTyp = List.map (fun (mt: MetaTerm) -> mt.CheckTyp()) mts
        if f.ArgsTyp = foundTyp then
          f.RetTyp
        else 
          let ets, fts = sprintf "%A" f.ArgsTyp, sprintf "%A" foundTyp
          failwith <| "Type error, found " + fts + " when expecting " + ets
      | Const(c) -> c.Typ
      | Var({Name = _; Typ = t}) -> t

    /// Returns a set of variables appearing in the MetaTerm
    member mt.Vars = 
      let ret = new HashSet<Variable>()
      let rec traverse mt =
        match mt with
        | App(f, mts) -> List.iter traverse mts
        | Var(v) -> ret.Add(v) |> ignore
        | _ -> ()
      ret

    /// Returns a Substitution that makes mt1 and mt2 syntactically equal, if possible; None otherwise
    member mt1.Unify (mt2: MetaTerm) =
      let rec traverse (subst: Substitution option) mt1 mt2 = 
        match subst with
        | None -> None
        | Some subst -> 
          match mt1, mt2 with
          | Var v1, (mt2: MetaTerm) when not <| mt2.Vars.Contains(v1) -> 
            let found, mt' = subst.TryGetValue v1
            if not found || mt' = mt2 then
              subst.[v1] <- mt2
              Some subst
            else
              None
          | mt1, Var v2 -> traverse (Some subst) (Var v2) mt1 
          | App(f1, mts1), App(f2, mts2) when f1 = f2 && mts1.Length = mts2.Length -> 
            List.fold2 traverse (Some subst) mts1 mts2
          | mt1, mt2 when mt1 = mt2 -> Some subst 
          | _ -> None
      traverse (Some <| new Substitution()) mt1 mt2

    /// Applies the given Substitution to the MetaTerm and returns another MetaTerm
    member mt.ApplySubstitution (subst: Substitution) =
      match mt with
      | App(f, mts) -> App(f, List.map (fun (mt: MetaTerm) -> mt.ApplySubstitution subst) mts)
      | Var(v) -> 
        let found, mt' = subst.TryGetValue v
        if found then
          mt'
        else
          Var(v)
      | c -> c

  type Assertion =
  | Knowledge of MetaTerm