namespace Microsoft.Research.Dkal.Substrate.Basic

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Tree

open System.Collections.Generic

type BasicSubstrate() =

  let evalf (f:Function) (args:list<obj>) =
    let cmp (a:obj) b =
      let aa = a :?> System.IComparable
      aa.CompareTo(b)
    match f.Name, args with
    | BasicPrimitives.And, args ->
      box (not (args |> List.exists (fun x -> x.Equals(false))))
    | BasicPrimitives.Or, args ->
      box (args |> List.exists (fun x -> x.Equals(true)))
    | BasicPrimitives.Not, [a] ->
      box (a.Equals(false))
    | BasicPrimitives.Divide, [a; b] ->
      box ((a:?>int) / (b:?>int))
    | BasicPrimitives.Plus, args ->
      let (a: seq<int>) = args |> Seq.cast
      box (a |> Seq.sum)
    | BasicPrimitives.Minus, args ->
      let (a: list<int>) = args |> Seq.cast |> Seq.toList
      box (a.Head - List.sum a)
    | BasicPrimitives.Uminus, [a] ->
      box -(a :?> int)
    | BasicPrimitives.Eq, [a; b] ->
      box (a.Equals(b))
    | BasicPrimitives.Gt, [a; b] ->
      box (cmp a b > 0)
    | BasicPrimitives.Gte, [a; b] ->
      box (cmp a b >= 0)
    | BasicPrimitives.Lt, [a; b] ->
      box (cmp a b < 0)
    | BasicPrimitives.Lte, [a; b] ->
      box (cmp a b <= 0)
    | BasicPrimitives.Neq, [a; b] ->
      box (not (a.Equals(b)))
    | BasicPrimitives.Times, [a; b] ->
      box ((a:?>int)*(b:?>int))
    | _ -> failwithf "unknown function %A on args %A" f args

  let rec evalute (s: ISubstitution) (expr: ITerm) =
    let eval = evalute s
    match expr with
      | :? IVar as v -> eval (s.Apply(v))
      | :? IConst as c -> c
      | App(f, args) ->
        let args = args |> List.map eval |> List.map (fun x -> x.Value)
        Constant (evalf f args) :> IConst
      | _ as t -> failwithf "unknown term %A" t

  let solve11 (q: BasicSubstrateTerm) (s: ISubstitution) =
    let r = evalute s q.Right
    match q.Left with
    | :? IVar as var ->
      [s.Extend(var, r)]
    | :? IConst as c ->
      if c.Equals(r) then [s]
      else []
    | _ -> failwithf "unknown left part: %A" q.Left

  interface ISubstrate with

    member bs.Namespaces = new HashSet<_>([BasicPrimitives.BasicNamespace])

    member bs.Update _ = failwith "Basic substrate does not support updates"

    member bs.AreConsistentUpdates _ = failwith "Basic substrate does not support updates"

    member bs.RequiredVars (query: ISubstrateQueryTerm) = 
      match query with
      | :? BasicSubstrateTerm as bst -> bst.Right.Vars
      | _ -> failwithf "Basic substrate does not understand term %O" query

    member bs.Solve (queries: ISubstrateQueryTerm seq) (substs: ISubstitution seq) =
      let queries: BasicSubstrateTerm seq = queries |> Seq.cast
      let solve1Many substs (query: BasicSubstrateTerm) =
        substs |> Seq.collect (solve11 query)
      queries |> Seq.fold solve1Many substs