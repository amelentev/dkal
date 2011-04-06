namespace Microsoft.Research.Dkal.Ast

  open System.Collections.Generic

  type Type = 
  | Bool
  | Principal
  | Infon
  | Substrate of System.Type
  with 
    static member Int = Substrate(typeof<int>)
    static member Float = Substrate(typeof<float>)
    static member String = Substrate(typeof<string>)
    override t.ToString() = 
      match t with
      | Substrate(typ) -> typ.Name
      | t -> sprintf "%A" t

  type Variable = { Name: string; 
                    Typ: Type }

  type TableDeclaration = { Name: string; 
                            Cols: Variable list }
  type RelationDeclaration = { Name: string; 
                               Args: Variable list }
  type Signature =  { Tables: TableDeclaration list;
                      Relations: RelationDeclaration list }

  type Constant = 
    | BoolConstant of bool
    | PrincipalConstant of string
    | SubstrateConstant of obj
  with 
    member c.Typ = 
      match c with
      | BoolConstant(_) -> Bool
      | PrincipalConstant(_) -> Principal
      | SubstrateConstant(c) -> Substrate(c.GetType())

  type Function = { Name: string; 
                    RetTyp: Type; 
                    ArgsTyp: Type list }
  
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
                      + " on " + (sprintf "%A" mt)
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
      traverse mt
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

  type Knowledge = { Fact: MetaTerm }
  type CommunicationRule =  { Trigger: MetaTerm;
                              Target: MetaTerm;
                              Content: MetaTerm }
  type Assertion =
  | Know of Knowledge
  | CommRule of CommunicationRule
  with 
    member a.Vars = 
      let ret = new HashSet<Variable>()
      match a with
      | Know(k) -> ret.UnionWith(k.Fact.Vars)
      | CommRule(c) ->  ret.UnionWith(c.Trigger.Vars)
                        ret.UnionWith(c.Target.Vars)
                        ret.UnionWith(c.Content.Vars)
      ret

  type Policy = { Assertions: Assertion list }

  type Assembly = { Signature: Signature; Policy: Policy }
