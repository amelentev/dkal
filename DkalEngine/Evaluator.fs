namespace Microsoft.Research.DkalEngine

open System
open System.IO
open System.Text
open Microsoft.FSharp.Text
open Microsoft.Research.DkalEngine.Ast
open Microsoft.FSharp.Collections

type Evaluator() =
  let interpretations = dict()
  let mutable trace = false
  let dictFromList l =
    let d = dict()
    l |> List.iter d.Add
    d
  
  let intRels = 
    dictFromList
      [ "<=", (fun a b -> a <= b);
        ">=", (fun a b -> a >= b);
        "<", (fun a b -> a < b);
        ">", (fun a b -> a > b);
      ]
  let intOps =
    dictFromList
      [ "+", (fun a b -> a + b);
        "-", (fun a b -> a - b);        
      ]
  
  let (|F|) = function
    | ({ name = n } : Function) -> F n
    
  member this.AddRewrite (lhs:Term) (rhs:Term) =
    if trace then System.Console.WriteLine ("ADD RULE: {0} -> {1}", lhs, rhs)
    interpretations.[lhs.Canonical()] <- rhs
    
  member this.SetTrace f = trace <- f
  
  member this.Eval t = 
    let res = 
      match t with
        | Term.App (pos, fn, args) ->
          let args = List.map this.Eval args
          let canonical = (Term.App (fakePos, fn, args)).Canonical()
          match fn.name, args with
            | op, [Term.Const (p, Const.Int a); Term.Const (_, Const.Int b)] when intOps.ContainsKey op ->
              Term.Const (pos, Const.Int (intOps.[op] a b))
            | op, [Term.Const (p, Const.Int a); Term.Const (_, Const.Int b)] when intRels.ContainsKey op ->
              Term.Const (pos, Const.Bool (intRels.[op] a b))
            | ("&&"|"||" as op), [Term.Const (p, Const.Bool a); Term.Const (_, Const.Bool b)] ->
              Term.Const (pos, Const.Bool (if op = "&&" then a && b else a || b))
            | "==", [a; b] when a.Canonical() = b.Canonical() ->
              Term.Const (pos, Const.Bool true)
            | _ when interpretations.ContainsKey canonical ->
              interpretations.[canonical]
            | _ -> Term.App (pos, fn, args)
        | t -> t
    if trace then System.Console.WriteLine ("EVAL: {0} -> {1}", t, res)
    res
    