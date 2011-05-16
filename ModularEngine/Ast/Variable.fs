namespace Microsoft.Research.Dkal.Ast

open Microsoft.Research.Dkal.Interfaces

/// Variables are typed
type Variable = 
  { Name: string; Type: IType }
  interface IVar with
    member v.Name = v.Name
    member v.Type = v.Type
    member v.Vars = [v]
    member v.BoundVars = []
    member v.Apply s = s.Apply v
    member v.Normalize () = v :> ITerm
    member v.UnifyFrom s t = 
      match (v :> IVar), t with
      | v1, v2 when (v1 :> ITerm).Apply(s) = v2.Apply(s) -> Some s
      | v, t when not(List.exists (fun v' -> v = v') t.Vars) -> 
        if s.DomainContains v then
          v.Apply(s).UnifyFrom s t
        else
          Some <| (Substitution.Id.Extend(v, t.Apply(s))).ComposeWith s
          //Some (s.Extend (v, t.Apply(s)))
      | _ -> None
    member v.Unify t = 
      (v :> ITerm).UnifyFrom (Substitution.Id) t
  override v.ToString() = v.Name    
