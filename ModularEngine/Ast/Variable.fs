namespace Microsoft.Research.Dkal.Ast

open Microsoft.Research.Dkal.Interfaces

/// Variables are typed
type Variable = 
  { Name: string; Type: IType }
  interface IVar with
    member v.Name = v.Name
    member v.Type = v.Type
    member v.Vars = [v]
    member v.Apply s = s.Apply v
    member v.Normalize () = v :> ITerm
    member v.UnifyFrom s t = 
      match (v :> IVar), t with
      | v1, v2 when (v1 :> ITerm) = v2 -> Some s
      | v, t when not(List.exists (fun v' -> v = v') t.Vars) -> Some (s.Extend (v, t))
      | _ -> None
    member v.Unify t = 
      (v :> ITerm).UnifyFrom (Substitution.Id) t
  override v.ToString() = v.Name    
