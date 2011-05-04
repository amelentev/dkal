namespace Microsoft.Research.Dkal.Substrate.Basic

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast

open System.Collections.Generic

type BasicSubstrateTerm(left: ITerm, right: ITerm) =

  member bst.Left = left
  member bst.Rigth = right

  interface ISubstrateQueryTerm with
    member bst.Namespace = BasicPrimitives.BasicNamespace

    member bst.Type = Type.SubstrateQuery

    member bst.Vars = new HashSet<_>(left.Vars @ right.Vars) |> Seq.toList

    member bst.Apply (s: ISubstitution) =
      new BasicSubstrateTerm(left.Apply s, right.Apply s) :> ITerm

    member bst.Normalize () = 
      new BasicSubstrateTerm(left.Normalize(), right.Normalize()) :> ITerm

    member bst.UnifyFrom (s: ISubstitution) (t: ITerm) = 
      match t with 
      | :? BasicSubstrateTerm as bst' -> 
        match bst.Left.UnifyFrom s bst'.Left with
        | Some s' -> bst.Rigth.UnifyFrom s' bst.Rigth
        | _ -> None
      | _ -> None

    member bst.Unify (t: ITerm) = (bst :> ITerm).UnifyFrom Substitution.Id t

