namespace Microsoft.Research.Dkal.Ast

open Microsoft.Research.Dkal.Interfaces

/// Constants are implicitly typed (they have the type of the wrapped
/// element)
type Constant<'t when 't: equality>(value: 't) = 
  interface IConst<'t> with
    member c.Vars = []
    member c.Type = Type.Substrate(value.GetType()) :> IType
    member c.Apply s = c :> ITerm
    member c.Normalize () = c :> ITerm
    member c.UnifyFrom s t =
      match (c :> ITerm), t with
      | c1, c2 when c1 = c2 -> Some s
      | c1, (:? IVar as v2) -> (v2 :> ITerm).UnifyFrom s c1
      | _ -> None
    member c.Unify t = 
      (c :> ITerm).UnifyFrom (Substitution.Id) t
    member c.Value = value
  member c.Value = (c :> IConst<_>).Value
  override c.Equals (o: obj) =
    match o with
    | :? Constant<'t> as c' -> c.Value.Equals c'.Value
                                 && (c :> ITerm).Type.Equals (c' :> ITerm).Type
    | _ -> false
  override c.GetHashCode() = ((c :> ITerm).Type, c.Value).GetHashCode()
  override c.ToString() = sprintf "%A" value

/// Principal constants
type PrincipalConstant(name: string) =
  inherit Constant<string>(name)
  interface IConst<string> with
    override pc.Type = Type.Principal
  member pc.Name = name
  override pc.ToString() = name

