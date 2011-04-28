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

namespace Microsoft.Research.Dkal.Substrate.Sql

open System
open System.IO
open System.Text
open System.Collections.Generic
open System.Linq
open Microsoft.FSharp.Text

open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Interfaces

module SqlCompiler =
  // glue to old SqlCompiler
  type Dict<'A,'B> = System.Collections.Generic.Dictionary<'A,'B>
  type Vec<'A> = System.Collections.Generic.List<'A>
  let dict() = new Dict<_,_>()
  let vec() = new Vec<_>()
  type Options = 
    {
      Trace : int
    }
  let log (msg:string) = System.Console.WriteLine msg

  // old SqlCompiler
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
    | Var of Variable
    | Const of Constant
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
      this.Map (function Expr.Var v when dict.ContainsKey v.Name -> Some (dict.[v.Name]) | _ -> None)

  type CompiledQuery = Expr * list<Variable*Expr>
  
  let init() =
    // old functions: TODO remove after backward testing is done
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
    //addPrefixSqlOp "not" "NOT"
    addPrefixSqlOp "true" "1"
    addPrefixSqlOp "false" "0"
    addPrefixSqlOp "int_null" "NULL"
    addPrefixSqlOp "string_null" "NULL"
    addPrefixSqlOp "double_null" "NULL"
   
   
    // new functions
    addSqlOp "eq" "="
    addSqlOp "neq" "<>"
    addSqlOp "and" "AND"
    addSqlOp "or" "OR"
    addPrefixSqlOp "not" "NOT"
    addSqlOp "gt" ">"
    addSqlOp "gte" ">="
    addSqlOp "lt" "<"
    addSqlOp "lte" "<="
    addSqlOp "plus" "+"
    addSqlOp "minus" "-"
    addPrefixSqlOp "uminus" "-"
    addSqlOp "times" "*"
    addSqlOp "divide" "/"
  
  do init()
  
  let sqlEq (a, b) = Op (sqlOps.["=="], [a;b])
  let sqlNeq (a, b) = Op (sqlOps.["!="], [a;b])
  let sqlTrue = Op (sqlOps.["true"], [])
  let sqlFalse = Op (sqlOps.["false"], [])
  let sqlAnd a b = 
    if a = sqlTrue then b
    elif b = sqlTrue then a
    else Op (sqlOps.["&&"], [a;b])
  
  let sqlMultiAnd : (Expr seq -> Expr) = Seq.fold sqlAnd sqlTrue
    
  type LocalCtx = 
    {
      currentScope : int
      mutable pendingEqs : list<Expr>      
      mutable bindings : Map<string, Expr>
    }

  let err (t:ITerm) (msg:string) : unit =
    failwith ("SQL compilation error: " + msg + " at '" + t.ToString() + "'")

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
        | (Expr.Var v, def:Expr) when def.IsGround() && not (repl.ContainsKey v.Name) ->
          did := true
          repl.Add (v.Name, def)
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
  
  let compile (opts:Options) nextId (theTerms:ITerm seq) =
    let nextScope = ref 1
    let fresh (v:Variable) : Variable =
      let id = nextId()
      { v with Name = v.Name + "#" + id.ToString() }
    let rec comp top (localCtx:LocalCtx) (term:ITerm) = 
      let res =
        match term with
          | ActivePatterns.Const(c) -> Expr.Const c
          | ActivePatterns.Var(v) ->
            match localCtx.bindings.TryFind v.Name with
              | Some r -> r
              | None when localCtx.currentScope <> 0 ->
                let expr = Expr.Var (fresh v)
                localCtx.bindings <- localCtx.bindings.Add (v.Name, expr)
                expr
              | None -> Expr.Var v
          | ActivePatterns.App(fn, [p]) when fn.Name="ppalName" ->
            comp false localCtx p
          | ActivePatterns.App (fn, args) as t ->
            let args = List.map (comp false localCtx) args
            if sqlOps.ContainsKey fn.Name then
              Expr.Op (sqlOps.[fn.Name], args)
            else if fn.Name.Contains(".") then
              //old:| Term.Const (Const.Column (t, c)) ->
              //      Expr.Column ({ scope = localCtx.currentScope; name = t }, c)
              let i = fn.Name.IndexOf('.')
              let t = fn.Name.Substring(0, i)
              let c = fn.Name.Substring(i+1)
              Expr.Column({ scope = localCtx.currentScope; name = t }, c)
            else
              log("warning: unmapped operation '" + fn.Name + "'")
              let op = {name=fn.Name; infix=(args.Length>1)} : SqlOp
              Expr.Op (op, args)
              //failwith ("no translation to SQL for '" + fn.Name + "'")
            (*old: match fn.body with
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
                if fn.retType.typ = Type.Bool then
                  sqlNeq (Expr.Var resV, sqlFalse)
                else
                  Expr.Var resV
              | _ when sqlOps.ContainsKey fn.name ->
                Expr.Op (sqlOps.[fn.name], args)
              | _ -> err t ("no translation to SQL for '" + fn.name + "'")*)
          | _ as t -> failwithf "unknown term %A" t
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
//    let pp = new SimplePrettyPrinter() :> IPrettyPrinter
    if trace >= 1 then
      log ("Query " + String.concat ", " (theTerms |> Seq.map (fun s -> s.ToString())))
    let body = Seq.map (comp true initCtx) theTerms |> sqlMultiAnd
    if trace >= 1 then
      log ("  Compiled " + body.ToString())
    let body, bindings = body |> simplify
    if trace >= 1 then  
      log ("  Simplified " + body.ToString() + " @ " + String.concat ", " (List.map (fun (v:Variable,e:Expr) -> v.ToString() + " -> " + e.ToString()) bindings))
    let rec boolenize = function
      | Expr.Op(op, exprs) when op.name="AND" ->
        let exprs1 = exprs |> List.map (function
          | Expr.Column(t,c) as col ->
            sqlEq(col, sqlTrue)
          | Expr.Const(SubstrateConstant(x)) as ec when x.GetType() = typeof<bool> ->
            sqlEq(ec, sqlTrue)
          | t -> boolenize t
        )
        Expr.Op(op, exprs1)
      | Expr.Op(op, exprs) ->
        Expr.Op(op, exprs |> List.map boolenize)
      | t -> t
    let body1 = body |> boolenize
    if trace >= 1 then  
      log ("  Boolenized " + body1.ToString())

    body1, bindings
    
  let execQuery (sql:SqlConnector, opts:Options, cc:CompiledQuery, subst:ISubstitution, vars:list<IVar>) =    
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
        | Expr.Const (True) -> print sqlTrue
        | Expr.Const (False) -> pr "NOT "; print sqlTrue
        | Expr.Const (Principal p) ->
          parm p
        | Expr.Const (SubstrateConstant o) -> parm o
        | Expr.Var v ->
          match subst.Apply (v) with
            | ActivePatterns.Const c ->
              print (Expr.Const c)
            | ActivePatterns.Var v ->
              failwith ("unbound variable in query: " + v.Name)
            | t ->
              failwith ("substitution maps " + v.Name + " to term " + t.ToString() + " not constant")
              
        | Expr.Op (op, [tr;a]) 
        | Expr.Op (op, [a;tr]) when op = sqlOps.["&&"] && tr = sqlTrue -> print a
        | Expr.Op (op, a1::atl) when op.infix ->
          pr "("
          print a1
          pr " "
          atl |> List.iter (fun a -> pr op.name; pr " "; print a)
          pr ")"
        | Expr.Op (op, [a]) when not op.infix -> 
          pr (op.name + " (")
          print a
          pr ")"
        | Expr.Op (op, []) -> pr op.name
        | Expr.Op (op, es) -> failwith ("impossible " + op.name + " " + es.Length.ToString())
        | _ as t -> failwithf "impossible %A" t
      
      let bound, unbound = snd cc |> List.partition (fun (v, _) -> subst.Contains v)
      let expr = bound |> List.fold (fun sofar (v, expr) -> sqlAnd sofar (sqlEq (expr, Expr.Var v))) (fst cc)
      print expr
      let whereClause = sb.ToString()
      sb.Length <- 0
      pr "SELECT "
      
      let resExprs = dict()
      unbound |> List.iter (fun (v:Variable, e:Expr) -> resExprs.Add (v.Name, e))      
      
      let needed = dict()
      let resSubst = vec()

      let rec need (v:IVar) =
        if not (needed.ContainsKey v) then
          needed.Add (v, true)
          match resExprs.TryGetValue v.Name with
            | true, expr ->
              print expr
              pr ", "
              resSubst.Add v
            | _ ->
              let expr = subst.Apply(v)
              log ("expand " + v.ToString() + " into " + expr.ToString())
              expr.Vars.AsEnumerable() |> Seq.iter need
      List.iter need vars
      // add something, so if there is no columns we still get the Boolean result
      pr "1"
      if tableList.Count > 0 then
        pr " FROM "
        pr (String.concat ", " tableList)
      pr " WHERE "
      let whereClause = if whereClause = "1" then "1 > 0" else whereClause
      pr whereClause
      
      let addToSubst rd (idx, subst:ISubstitution) (var : IVar) =
        let constVal = sql.ReadVar (rd, var, idx)
        (idx + 1, subst.Extend(var, constVal))
      sql.ExecQuery (sb.ToString(), parms, opts.Trace >= 1) |>
        Seq.map (fun rd -> Seq.fold (addToSubst rd) (0, subst) resSubst |> snd)