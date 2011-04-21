namespace Microsoft.Research.Dkal.Substrate

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast

/// Dispatch queries to substrate across substrate implementations
type SubstrateDispatcher() =

  /// return ISubstrate for subst label
  static member GetSubstrate (subst : MetaTerm) : ISubstrate =
    new DummySubstrate() :> ISubstrate
    //TODO: match subst with
    //    Sql(Const(SubstrateElemConstant(str))) -> new SqlSubstrate(str.ToString()) :> ISubstrate
    //  | _ -> failwith("unrecorgnized substrate")

  /// queries : asInfon queries
  /// substs  : seq of substitutions to check
  /// return  : seq of resolved substitutions (more specialized than substs)
  static member Solve (queries: MetaTerm seq) (substs: Substitution seq) =
    let groupedq = queries |> Seq.groupBy (function 
        AsInfon(q, s) -> s
      | _ -> failwith("Not asInfon"))
    groupedq |> Seq.fold (fun res (substr, qs) ->
      SubstrateDispatcher.GetSubstrate(substr).Solve qs res
    ) substs