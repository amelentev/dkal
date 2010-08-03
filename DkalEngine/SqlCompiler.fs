// *********************************************************
//
//    Copyright (c) Microsoft. All rights reserved.
//    This code is licensed under the Apache License, Version 2.0.
//    THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
//    ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
//    IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
//    PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.
//
// *********************************************************

namespace Microsoft.Research.DkalEngine

open System
open System.IO
open System.Text
open Microsoft.FSharp.Text

open Microsoft.Research.DkalEngine.Util
open Microsoft.Research.DkalEngine.Ast

module SqlCompiler =
  type SqlOp =
    {
      name : string
      infix : bool
    }
  
  let sqlOps = dict()
  
  let addSqlOp dkalName sqlName =
    sqlOps.Add (dkalName, { name = sqlName; infix = true })
    
  let addPrefixSqlOp dkalName sqlName =
    sqlOps.Add (dkalName, { name = sqlName; infix = false })
  
  type TableId =
    {
      scope : int
      name : string
    }
    
  type Expr =
    | Column of TableId * string
    | Var of Var
    | Const of Const
    | Op of SqlOp * list<Expr>
    
    member this.Map f =
      let rec aux t =
        match f t with
          | Some t -> t
          | None ->
            match t with
              | Expr.Column _
              | Expr.Var _
              | Expr.Const _ -> t
              | Expr.Op (op, exprs) ->
                Expr.Op (op, List.map aux exprs)
      aux this
    
    member this.IsGround() =
      let hasVar = ref false
      let aux t =
        if !hasVar then Some t
        else
          match t with
            | Expr.Var _ -> hasVar := true; Some t
            | _ -> None
      this.Map aux |> ignore
      not !hasVar
                        
    override this.ToString() =
      let sb = new StringBuilder()
      let wr (s:obj) = sb.Append s |> ignore
      let rec pr = function
        | Expr.Column (t, c) ->
          wr t.name
          wr "@"
          wr t.scope
          wr "."
          wr c
        | Expr.Const c -> wr (c.ToString())
        | Expr.Op ({ name = n }, []) -> wr n
        | Expr.Op ({ name = n; infix = true }, [a1; a2]) ->
          wr "("
          pr a1
          wr " "
          wr n
          wr " "
          pr a2
          wr ")"
        | Expr.Op (op, args) ->
          wr op.name
          wr "("
          for t in args do
            pr t
            wr ", "
          sb.Length <- sb.Length - 2
          wr ")"
        | Expr.Var v -> wr (v.ToString())          
      pr this
      sb.ToString()
    
    member this.Subst (dict:Dict<_,_>) =
      this.Map (function Expr.Var v when dict.ContainsKey v.id -> Some (dict.[v.id]) | _ -> None)

  type CompiledQuery = Expr * list<Var*Expr>
  
  let init() =
    addSqlOp "&&" "AND"
    addSqlOp "||" "OR"
    addSqlOp "+" "+"
    addSqlOp "-" "-"
    addSqlOp "*" "*"
    addSqlOp "/" "/"
    addSqlOp "<=" "<="
    addSqlOp ">=" ">="
    addSqlOp "<" "<"
    addSqlOp ">" ">"
    addSqlOp "==" "="
    addSqlOp "!=" "<>"
    addPrefixSqlOp "true" "1=1"
    addPrefixSqlOp "false" "0=1"
  
  do init()
  
  let sqlEq (a, b) = Op (sqlOps.["=="], [a;b])
  let sqlTrue = Op (sqlOps.["true"], [])
  let sqlAnd a b = 
    if a = sqlTrue then b
    elif b = sqlTrue then a
    else Op (sqlOps.["&&"], [a;b])
  
  let sqlMultiAnd = List.fold sqlAnd sqlTrue
    
  type LocalCtx = 
    {
      currentScope : int
      mutable pendingEqs : list<Expr>      
      mutable bindings : Map<int, Expr>
    }
    
  let err (t:Term) msg =
    raise (SyntaxError (fakePos, "SQL compilation error: " + msg + " at '" + t.ToString() + "'"))
    
  let simplify expr =
  
    let eqs = ref []
    let rec findEqs (expr:Expr) =
      match expr with
        | Expr.Op (op, [a; b]) when op = sqlOps.["&&"] ->
          sqlAnd (findEqs a) (findEqs b)
        | Expr.Op (op, [Expr.Var _ as c1; c2])
        | Expr.Op (op, [c2; Expr.Var _ as c1]) when op = sqlOps.["=="] ->
          eqs := (c1, c2) :: !eqs
          sqlTrue
        | t -> t
    let expr = findEqs expr
    
    let bindings = ref []
    let repl = dict()
    let rec loop (workSet:list<Expr*Expr>) =
      let did = ref false
      let checkGrnd rest = function
        | (Expr.Var v, def:Expr) when def.IsGround() && not (repl.ContainsKey v.id) ->
          did := true
          repl.Add (v.id, def)
          bindings := (v, def) :: !bindings
          rest
        | p -> p :: rest
      let subst (a:Expr, b:Expr) =
        match a.Subst repl, b.Subst repl with
          | Expr.Var _ as a, b -> (a, b)
          | a, (Expr.Var _ as b) -> (b, a)
          | a, b -> (a, b)
      let workSet = List.fold checkGrnd [] workSet |> List.map subst
      if !did then
        loop workSet
      else
        sqlMultiAnd (expr.Subst repl :: List.map sqlEq workSet), !bindings
        
    loop !eqs
  
  let compile (opts:Options) nextId theTerms =
    let nextScope = ref 1
    let fresh (v:Var) =      
      let id = nextId()
      { v with id = id; name = v.name + "#" + id.ToString() }
    let rec comp top (localCtx:LocalCtx) term = 
      let res =
        match term with
          | Term.Const (Const.Column (t, c)) ->
            Expr.Column ({ scope = localCtx.currentScope; name = t }, c)
          | Term.Const c -> Expr.Const c
          | Term.Var v ->
            match localCtx.bindings.TryFind v.id with
              | Some r -> r
              | None when localCtx.currentScope <> 0 ->
                let expr = Expr.Var (fresh v)
                localCtx.bindings <- localCtx.bindings.Add (v.id, expr)
                expr
              | None -> Expr.Var v
          | Term.App (fn, args) as t ->
            let args = List.map (comp false localCtx) args
            match fn.body with
              | :? Term as body ->
                let resV = fresh fn.retType
                let bindings = 
                  List.zip (fn.retType :: fn.argTypes) (Expr.Var resV :: args) |>
                    (Map.empty |> List.fold (fun acc (v, expr) -> acc.Add (v.id, expr)))
                let newCtx = { currentScope = !nextScope
                               pendingEqs = []
                               bindings = bindings
                               }
                incr nextScope
                let res = comp false newCtx body
                if opts.Trace >= 2 then
                  log ("Body " + body.ToString() + " =====> " + res.ToString())
                localCtx.pendingEqs <- res :: newCtx.pendingEqs @ localCtx.pendingEqs
                Expr.Var resV
              | _ when sqlOps.ContainsKey fn.name ->
                Expr.Op (sqlOps.[fn.name], args)
              | _ -> err t ("no translation to SQL for '" + fn.name + "'")
      if top then
        let res = List.fold sqlAnd res localCtx.pendingEqs
        localCtx.pendingEqs <- []
        res
      else
        res
    let initCtx = { currentScope = 0
                    pendingEqs = []
                    bindings = Map.empty }

    let trace = opts.Trace
    if trace >= 1 then
      log ("Query " + String.concat ", " (theTerms |> List.map (fun s -> s.ToString())))
    let body = List.map (comp true initCtx) theTerms |> sqlMultiAnd
    if trace >= 1 then
      log ("  Compiled " + body.ToString())
    let body, bindings = body |> simplify
    if trace >= 1 then  
      log ("  Simplified " + body.ToString() + " @ " + String.concat ", " (List.map (fun (v:Var,e:Expr) -> v.ToString() + " -> " + e.ToString()) bindings))
    body, bindings
    
  let execQuery (sql:SqlConnector, comm:ICommunicator, opts:Options, cc:CompiledQuery, subst:Subst, vars:list<Var>) =
    if cc = (sqlTrue, []) then
      seq [subst]
    else
      let tables = dict()
      let tableList = vec()
      let parms = vec()
      let sb = StringBuilder()
      let pr (o:obj) = sb.Append o |> ignore
      let parm (v:obj) =
        match v with
          | :? int as i ->
            pr (i.ToString())
          | _ ->
            let id = parms.Count
            parms.Add (v:obj)
            pr "@p__"
            pr id
      let rec print = function
        | Expr.Column (t, c) ->
          if not (tables.ContainsKey t) then
            let name = "t__" + tables.Count.ToString()
            tables.Add (t, name)
            tableList.Add (t.name + " AS " + name)
          pr tables.[t]
          pr "."
          pr c
        | Expr.Const (Const.Bool true) -> print sqlTrue
        | Expr.Const (Const.Bool false) -> pr "NOT "; print sqlTrue
        | Expr.Const (Const.Int i) -> parm i
        | Expr.Const (Const.Principal p) ->
          parm (comm.PrincipalId p)
        | Expr.Const (Const.Column _) -> failwith "impossible"
        | Expr.Var v ->
          match subst.TryFind v.id with
            | Some (Term.Const c) ->
              print (Expr.Const c)
            | Some t ->
              failwith ("substitution maps " + v.name + " to term " + t.ToString() + " not constant")
            | None ->
              failwith ("unbound variable in query: " + v.name)
              
        | Expr.Op (op, [tr;a]) 
        | Expr.Op (op, [a;tr]) when op = sqlOps.["&&"] && tr = sqlTrue -> print a
        
        | Expr.Op (op, [a;b]) when op.infix ->
          pr "("
          print a
          pr " "
          pr op.name
          pr " "
          print b
          pr ")"
        | Expr.Op (op, []) -> pr op.name
        | Expr.Op (_, _) -> failwith "impossible"
      
      let bound, unbound = snd cc |> List.partition (fun (v, _) -> subst.ContainsKey v.id)
      let expr = bound |> List.fold (fun sofar (v, expr) -> sqlAnd sofar (sqlEq (expr, Expr.Var v))) (fst cc)
      print expr
      let whereClause = sb.ToString()
      sb.Length <- 0
      pr "SELECT DISTINCT "
      
      let resExprs = dict()
      unbound |> List.iter (fun (v, e) -> resExprs.Add (v.id, e))      
      
      let needed = dict()
      let resSubst = vec()
      let rec need (v:Var) =
        if not (needed.ContainsKey v) then
          needed.Add (v, true)
          match resExprs.TryGetValue v.id with
            | true, expr ->
              print expr
              pr ", "
              resSubst.Add v
            | _ ->
              let expr = (Term.Var v).Apply subst
              //log ("expand " + v.ToString() + " into " + expr.ToString())
              expr.Vars() |> List.iter need
      List.iter need vars
      // add something, so if there is no columns we still get the Boolean result
      pr "1"
      if tableList.Count > 0 then
        pr " FROM "
        pr (String.concat ", " tableList)
      pr " WHERE "
      pr whereClause
      
      let addToSubst rd (idx, subst:Subst) var =
        let constVal = sql.ReadVar (comm.PrincipalById, rd, var, idx)
        (idx + 1, subst.Add (var.id, Term.Const constVal))
      
      sql.ExecQuery (sb.ToString(), parms, opts.Trace >= 1) |>
        Seq.map (fun rd -> Seq.fold (addToSubst rd) (0, subst) resSubst |> snd)
