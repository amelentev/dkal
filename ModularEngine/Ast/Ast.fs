namespace Microsoft.Research.Dkal.Ast

  open System.Collections.Generic

  type Type = 
  | Bool
  | Principal
  | Infon
  | Action
  | Rule
  | Substrate
  | SubstrateElem of System.Type
  with 
    static member Int = SubstrateElem(typeof<int>)
    static member Float = SubstrateElem(typeof<float>)
    static member String = SubstrateElem(typeof<string>)
    override t.ToString() = 
      match t with
      | SubstrateElem(typ) -> typ.Name
      | t -> sprintf "%A" t

  type Variable = { Name: string; 
                    Typ: Type }

  type Constant = 
    | BoolConstant of bool
    | PrincipalConstant of string
    | SubstrateElemConstant of obj
  with 
    member c.Typ = 
      match c with
      | BoolConstant(_) -> Bool
      | PrincipalConstant(_) -> Principal
      | SubstrateElemConstant(c) -> SubstrateElem(c.GetType())

  type Function = { Name: string; 
                    RetTyp: Type; 
                    ArgsTyp: Type list }
  
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
        | App(f, mts) -> 
            List.iter (fun mt -> 
                         match mt with 
                         | App(f, _) when f.Name = "rule" -> ()
                         | _ -> traverse mt) mts
        | Var(v) -> ret.Add(v) |> ignore
        | _ -> ()
      traverse mt
      ret

  type TableDeclaration = { Name: string; 
                            Cols: Variable list }
  type RelationDeclaration = { Name: string; 
                               Args: Variable list }
  type SubstrateDeclaration = { Name: string;
                                Decl: MetaTerm }
  type Signature =  { Substrates: SubstrateDeclaration list;
                      Tables: TableDeclaration list;
                      Relations: RelationDeclaration list }
 
  type Policy = { Rules: MetaTerm list }

  type Assembly = { Signature: Signature; Policy: Policy }

