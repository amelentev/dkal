namespace Microsoft.Research.DkalEngine

open System
open System.IO
open System.Text
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Text
open Microsoft.Research.DkalEngine.Util

module Ast =
  //
  // The real final AST
  //
  
  type Type =
    {
      id : int
      name : string
    }
    
    override this.ToString() =
      this.name
  
  type Var =
    { id : int; name : string; mutable typ : Type }
    
    override this.ToString() = this.name

  [<ReferenceEquality; NoComparison>]
  type Function =
    {
      id : int
      name : string
      retType : Var
      argTypes : list<Var>
      mutable body : obj
    }
    
    member this.WriteAsInfix (sb:StringBuilder) pr lst =
      let wr (s:obj) = sb.Append s |> ignore
      let rec aux pos lst =
        if pos >= this.name.Length then
          if lst = [] then ()
          else failwith "more arguments than stars"
        else
          match this.name.[pos] with
            | '-' -> 
              wr ' '; aux (pos + 1) lst
            | '*' ->
              match lst with
                | x :: xs ->                
                  wr "("; pr x; wr ")"
                  aux (pos + 1) xs
                | [] -> failwith "more stars than arguments"
            | ch ->
              wr ch; aux (pos + 1) lst

      if this.name.IndexOf '*' < 0 then
        wr this.name
        if lst <> [] then
          wr "("
          for t in lst do
            pr t
            wr ", "
          sb.Length <- sb.Length - 2
          wr ")"
      else
        aux 0 lst
                  
  let private nextId =
    let curr = ref 0
    function () -> incr curr; !curr
  
  let private infon : Type = { id = nextId(); name = "*infon*" }
  let private assertion : Type = { id = nextId(); name = "*assertion*" }
  let private unbound : Type = { id = nextId(); name = "*assertion*" }
  let private principal : Type = { id = nextId(); name = "principal" }
  let private intType : Type = { id = nextId(); name = "int" }
  let private boolType : Type = { id = nextId(); name = "bool" }
  
  type Type with
    static member Infon = infon
    static member Assertion = assertion
    static member Unbound = unbound
    static member Principal = principal
    static member Int = intType
    static member Bool = boolType
  
  let globalFunctions = dict() // accessed from ParsingCtx
  let private mkVar tp =
    let id = nextId()
    ({ id = id; name = "global#" + id.ToString(); typ = tp} : Var)
  let private addGlobalFunction retType name argTypes =
    let fn = { retType = mkVar retType; name = name; argTypes = List.map mkVar argTypes; id = nextId(); body = null }
    globalFunctions.Add (fn.name, fn)
    fn
    
  let private infonAnd = addGlobalFunction Type.Infon "Infon.&&" [Type.Infon; Type.Infon]
  let private infonFollows = addGlobalFunction Type.Infon "Infon.==>" [Type.Infon; Type.Infon]
  let private infonSaid = addGlobalFunction Type.Infon "Infon.said" [Type.Principal; Type.Infon]
  let private infonImplied = addGlobalFunction Type.Infon "Infon.implied" [Type.Principal; Type.Infon]
  let private infonEmpty = addGlobalFunction Type.Infon "Infon.empty" []
  let private infonAsInfon = addGlobalFunction Type.Infon "asInfon" [Type.Bool]
  
  type Function with
    static member And = infonAnd
    static member Follows = infonFollows
    static member Said = infonSaid
    static member Implied = infonImplied
    static member Empty = infonEmpty
    static member AsInfon = infonAsInfon
  
  [<StructuralEquality; NoComparison>]
  type Principal =
    { internal_id : int; name : string; typ : Type } 
    
    override this.ToString() = this.name
  
  type TermDesc =
    { pos : Pos; typ : Type }
  
  type Const =
    | Principal of Principal
    | Int of int
    | Bool of bool
    | Column of string * string
    
    override this.ToString() =
      match this with
        | Int i -> i.ToString()
        | Principal p -> p.ToString()
        | Column (a, b) -> a + "." + b
        | Bool true -> "true"
        | Bool false -> "false"
  
  // YUCK!!!
  let private termToStringCallback = ref (fun (sb:StringBuilder, t:obj) -> failwith "should be assigned"; ())
  
  [<StructuralEquality; NoComparison>]
  type Term =
    | App of Pos * Function * list<Term>
    | Const of Pos * Const
    | Var of Pos * Var
    
    member this.Map f =
      let rec aux t = 
        match f t with
          | Some r -> r
          | None ->
            match t with
              | Term.App (pos, fn, args) -> Term.App (pos, fn, List.map aux args)
              | Const _
              | Var _ -> t
      aux this
      
    member this.Pos =
      match this with
        | App (p, _, _) -> p
        | Var (p, _) -> p
        | Const (p, _) -> p
        
    member this.Type =
      match this with
        | App (_, f, _) -> f.retType.typ
        | Var (_, v) -> v.typ
        | Const (_, Const.Int _) -> Type.Int
        | Const (_, Const.Principal _) -> Type.Principal
        | Const (_, Const.Bool _) -> Type.Bool
        | Const (_, Const.Column _) -> Type.Unbound
        
    member this.Vars() =
      let vars = dict()
      let varList = ref []
      let add (v:Var) =
        if vars.ContainsKey v.id then ()
        else
          varList := v :: !varList
          vars.Add (v.id, true)
      this.Map (function Term.Var (_, v) -> add v; None | _ -> None) |> ignore
      !varList |> List.rev
        
    override this.ToString() =
      let sb = new StringBuilder()
      !termToStringCallback (sb, (this :> obj))
      sb.ToString()
    
    static member True = Const (fakePos, Const.Bool true)
      
  type PrincipalTerm = Term  
  type Infon = Term
  
  let (===) = LanguagePrimitives.PhysicalEquality
  
  let (|InfonAnd|_|) = function
    | App (p, fn, [a; b]) when fn === Function.And -> Some (InfonAnd (p, a, b))
    | _ -> None
  
  let (|InfonFollows|_|) = function
    | App (p, fn, [a; b]) when fn === Function.Follows -> Some (InfonFollows (p, a, b))
    | _ -> None
  
  let (|InfonSaid|_|) = function
    | App (p, fn, [a; b]) when fn === Function.Said -> Some (InfonSaid (p, a, b))
    | _ -> None
  
  let (|InfonImplied|_|) = function
    | App (p, fn, [a; b]) when fn === Function.Implied -> Some (InfonImplied (p, a, b))
    | _ -> None
  
  let (|InfonEmpty|_|) = function
    | App (p, fn, []) when fn === Function.Empty -> Some (InfonEmpty)
    | _ -> None
  
  let (|AsInfon|_|) = function
    | App (p, fn, [a]) when fn === Function.AsInfon -> Some (AsInfon (p, a))
    | _ -> None
  
  type Term with
    static member And (p, a, b) = App (p, Function.And, [a; b])
    static member Follows (p, a, b) = App (p, Function.Follows, [a; b])
    static member Said (p, a, b) = App (p, Function.Said, [a; b])
    static member Implied (p, a, b) = App (p, Function.Implied, [a; b])
    static member Empty (p) = App (p, Function.Empty, [])
    
    member this.IsEmpty =
      match this with
        | InfonEmpty -> true
        | _ -> false

  let private infonToString (sb:StringBuilder, t_:obj) =
    let wr (s:obj) = sb.Append s |> ignore
    let rec pr = function
      | InfonFollows (_, InfonSaid (_, p, a), a') when a = a' ->
        wr "("
        wr (p.ToString())
        wr " tdonS "
        pr a
        wr ")"
      | InfonFollows (_, InfonImplied (_, p, a), a') when a = a' ->
        wr "("
        wr (p.ToString())
        wr " tdonI "
        pr a
        wr ")"
      | InfonFollows (_, a, b) ->
        wr "("
        pr a
        wr " ==> "
        pr b
        wr ")"
      | InfonAnd (_, a, b) ->
        wr "("
        pr a
        wr " && "
        pr b
        wr ")"
      | InfonSaid (_, p, i) ->
        wr (p.ToString())
        wr " said "
        pr i          
      | InfonImplied (_, p, i) ->
        wr (p.ToString())
        wr " implied "
        pr i
      | InfonEmpty -> wr "empty"
      | App (_, f, args) ->
        f.WriteAsInfix sb pr args
      | Var (_, v) -> wr (v.name)
      | Const (_, p) -> wr p
    pr (t_ :?> Term)

  do termToStringCallback := infonToString

  
  [<StructuralEquality; NoComparison>]
  type AssertionInfo =
    { origin : Pos; principal : Principal; }

  [<StructuralEquality; NoComparison>]
  type Knows =
    { 
      ai : AssertionInfo
      infon : Infon
    }
  
  type Communication =
    {
      ai : AssertionInfo
      target : PrincipalTerm
      message : Infon
      proviso : Infon
      trigger : Infon
      certified : bool
    }
    
  type Filter =
    {
      ai : AssertionInfo
      source : PrincipalTerm
      message : Infon
      proviso : Infon
      trigger : Infon
    }

  [<StructuralEquality; NoComparison>]       
  type Message =
    {
      source : Principal
      target : Principal
      message : Infon
      proviso : Infon
      certified : bool
    }
    
  type Assertion =
    | Knows of Knows
    | SendTo of Communication
    | ReceiveFrom of Filter
    | Query of Knows
    
    override this.ToString() =
      match this with
        | Knows k ->
          k.ai.principal.ToString() + " : " + k.infon.ToString()
        | Query k ->
          k.ai.principal.ToString() + " ? " + k.infon.ToString()
        | SendTo c ->
          c.ai.principal.ToString() + " to " + c.target.ToString() + " : [" + c.message.ToString() + " <-- " + c.proviso.ToString() + "] <== " + c.trigger.ToString()
        | ReceiveFrom f ->
          f.ai.principal.ToString() + " from " + f.source.ToString() + " : [" + f.message.ToString() + " <-- " + f.proviso.ToString() + "] <== " + f.trigger.ToString()
            
        
  //
  // Unification and substitutions
  //
  
  type Subst = Map<int, Term>
  
  type Term with
    member this.Apply (s:Subst) =
      this.Map
        (function 
                | Term.Var (_, v) when s.ContainsKey v.id -> 
                  Some (s.[v.id].Apply s) 
                | _ -> None)
      
  type AugmentedSubst =
    {
      subst : Subst
      assumptions : list<Term>
    }
    
    static member Empty = { subst = Map.empty; assumptions = [] }
    static member NoAssumptions s = { subst = s; assumptions = [] }
  
  let substToString (s:Subst) =
    s |> Map.fold (fun acc k v -> (k.ToString() + " -> " + v.ToString()) :: acc) [] |> List.rev |> String.concat ", " 
    
  let rec unifyList unify s = function
    | [] -> s
    | x :: xs ->
      match s with
        | None -> None
        | Some s -> unifyList unify (unify s x) xs

  let rec occursTerm (v:Var) = function
    | Term.App (_, _, args) ->
      List.exists (occursTerm v) args
    | Term.Const _ -> false
    | Term.Var (_, v') -> v.id = v'.id
    
  let rec unifyTerms (subst:Subst) = function
    | (Term.Var (_, v1), Term.Var (_, v2)) when v1.id = v2.id -> Some subst
    | (Term.Var (_, v), t) ->
      if subst.ContainsKey v.id then
        unifyTerms subst (subst.[v.id], t)
      else
        if occursTerm v t then None
        else Some (subst.Add (v.id, t))
    | (t, (Term.Var _ as t')) ->
      unifyTerms subst (t', t)
    | (Term.App (_, f, args), Term.App (_, f', args')) when f.id = f'.id ->
      unifyList unifyTerms (Some subst) (List.zip args args')
    | (Term.Const (_, p1), Term.Const (_, p2)) when p1 = p2 -> Some subst
    | _ -> None
    
    
  //
  // Canonical form
  //
    
  let canonicalVariables = dict()
   
  type Term with
    member this.Canonical() =
      let currentSubst = dict()
      let rec aux = function
        | Term.App (_, f, args) ->
          Term.App (fakePos, f, List.map aux args)
        | Term.Const (_, c) -> Term.Const (fakePos, c)
        | Term.Var (_, v) ->
          if not (currentSubst.ContainsKey v.id) then
            let id = currentSubst.Count + 100
            let key = (v.typ, id)
            if not (canonicalVariables.ContainsKey key) then
              canonicalVariables.Add (key, ({ id = id; typ = v.typ; name = "V#" + id.ToString() } : Var))
            let repl = canonicalVariables.[key]
            currentSubst.[v.id] <- Term.Var (fakePos, repl)
          currentSubst.[v.id]
      aux this
  
    static member Canonicalize lst =
      match (Term.App (fakePos, Function.Empty, lst)).Canonical() with
        | Term.App (_, _, lst) -> lst
        | _ -> failwith "impossible"

  type Message with
    member this.Canonical() =
      match Term.Canonicalize [this.message; this.proviso] with
        | [m; p] ->
          { this with message = m; proviso = p }
        | _ -> failwith "impossible"
 
