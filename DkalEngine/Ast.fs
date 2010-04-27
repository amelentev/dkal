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

  type PP =
    | PString of string
    | PBlock of int * list<PP>

    member this.Append str =
      match this with
        | PString s -> PString (s + str)
        | PBlock (n, lst) ->
          match List.rev lst with
            | x :: xs ->
              PBlock (n + str.Length, List.rev (x.Append str :: xs))
            | [] -> PString str          

    member this.Prepend (str:string) =
      match this with
        | PString s -> PString (str + s)
        | PBlock (n, x :: xs) ->
          PBlock (n + str.Length, x.Prepend str :: xs)
        | PBlock (_, []) -> PString str

    member this.Length =
      match this with
        | PString s -> s.Length
        | PBlock (n, _) -> n

    member this.Print (sb:System.Text.StringBuilder) margin =
      let wr (s:string) = sb.Append s |> ignore
      let rec wrpp = function
        | PP.PString s -> wr s
        | PP.PBlock (_, l) ->
          match List.rev l with
            | x :: xs ->
              for x in List.rev xs do
                wrpp x
                wr " "
              wrpp x
            | [] -> () 
      let rec line ind = function
        | PP.PString s ->
          wr (String(' ', ind))
          wr s
          wr "\n"
        | PP.PBlock (len, x :: xs) as pp ->
          if len + ind > margin then
            line ind x
            List.iter (line (ind + 2)) xs
          else
            wr (String(' ', ind))
            wrpp pp
            wr "\n"
        | PP.PBlock (_, []) -> ()
      line 0 this
    
    static member Block lst =
      let lst = lst |> List.filter (function PString "" -> false | _ -> true)
      PBlock (List.map (fun (s:PP) -> s.Length) lst |> List.sum, lst)

  [<ReferenceEquality; NoComparison>]
  type Function =
    {
      id : int
      name : string
      retType : Var
      argTypes : list<Var>
      mutable body : obj
    }
    
    member this.WriteAsInfix (lst:list<PP>) =
      if this.name.IndexOf '*' < 0 then
        match lst with
          | [a; b] when not (this.name |> Seq.exists Char.IsLetterOrDigit) ->
            PP.Block [a.Prepend "("; PP.PString this.name; b.Append ")"]
          | [] -> PP.PString this.name
          | _ ->
            match List.rev lst with
              | x :: xs ->
                match List.rev (x.Append ")" :: (xs |> List.map (fun x -> x.Append ","))) with
                  | x :: xs -> PP.Block (PP.PString this.name :: x.Prepend "(" :: xs)
                  | [] -> failwith ""
              | [] -> failwith ""
      else
        let words = ("-" + this.name + "-").Split '*' |> Seq.toList
        if words.Length - 1 <> lst.Length then
          failwith ("wrong args " + this.name)
        else
          let toPP (s:string) = PP.PString (s.Trim '-')
          let par (p:PP) = (p.Prepend "(").Append ")"
          let args = List.fold2 (fun acc a b -> toPP b :: par a :: acc) [toPP (List.head words)] lst (List.tail words) 
          PP.Block (List.rev args)
                  
  let private nextId =
    let curr = ref 0
    function () -> incr curr; !curr
  
  let private infon : Type = { id = nextId(); name = "*infon*" }
  let private assertion : Type = { id = nextId(); name = "*assertion*" }
  let private unbound : Type = { id = nextId(); name = "*unbound*" }
  let private evidence : Type = { id = nextId(); name = "*evidence*" }
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
    static member Evidence = evidence
  
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
  let private infonCertified = addGlobalFunction Type.Infon "Infon.cert" [Type.Infon; Type.Evidence]
  
  // the int parameter is a placeholder for the actual cryptographic signature
  let private evSignature = addGlobalFunction Type.Evidence "Ev.sign" [Type.Principal; Type.Infon; Type.Int]
  let private evMp = addGlobalFunction Type.Evidence "Ev.mp" [Type.Evidence; Type.Evidence]
  
  type Function with
    static member And = infonAnd
    static member Follows = infonFollows
    static member Said = infonSaid
    static member Implied = infonImplied
    static member Empty = infonEmpty
    static member AsInfon = infonAsInfon
    static member Cert = infonCertified
    static member EvSignature = evSignature
    static member EvMp = evMp
  
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
  
  let (|InfonCert|_|) = function
    | App (p, fn, [a; b]) when fn === Function.Cert -> Some (InfonCert (p, a, b))
    | _ -> None
  
  type Term with
    static member And (p, a, b) = App (p, Function.And, [a; b])
    static member Follows (p, a, b) = App (p, Function.Follows, [a; b])
    static member Said (p, a, b) = App (p, Function.Said, [a; b])
    static member Implied (p, a, b) = App (p, Function.Implied, [a; b])
    static member Empty (p) = App (p, Function.Empty, [])
    static member Cert (p, i, e) = App (p, Function.Cert, [i; e])
    
    member this.IsEmpty =
      match this with
        | InfonEmpty -> true
        | _ -> false

  let private infonToString (sb, (t_:obj)) =
    let par pp = ((PP.Block pp).Append ")").Prepend "("
    let s s = PString s
    let rec pr = function
      | InfonFollows (_, InfonSaid (_, p, a), a') when a = a' ->
        par [s (p.ToString()); s "tdonS"; pr a]
      | InfonFollows (_, InfonImplied (_, p, a), a') when a = a' ->
        par [s (p.ToString()); s "tdonI"; pr a]
      | InfonFollows (_, a, b) ->
        par [pr a; s "==>"; pr b]
      | InfonAnd (_, a, b) ->
        par [pr a; s "&&"; pr b]
      | InfonSaid (_, p, i) ->
        PP.Block [s (p.ToString()); s "said"; pr i]
      | InfonImplied (_, p, i) ->
        PP.Block [s (p.ToString()); s "implied"; pr i]
      | InfonEmpty -> s "empty"
      | App (_, f, args) ->
        f.WriteAsInfix (List.map pr args)
      | Var (_, v) -> s (v.name)
      | Const (_, p) -> s (p.ToString())
    (pr (t_ :?> Term)).Print sb 90
    if sb.Chars (sb.Length - 1) = '\n' then
      sb.Length <- sb.Length - 1

  do termToStringCallback := infonToString

  type Term with
    member this.Sanitize () =
      let vars = dict()
      let aux = function
        | Term.App (x, f, [p; _; _]) when f === Function.EvSignature ->
          Some (Term.App (x, f, [p]))
        | Term.Var (p, v) ->
          if not (vars.ContainsKey v.id) then
            vars.[v.id] <- Term.Var (p, { v with name = String((char)((int)'A' + vars.Count), 1) })
          Some (vars.[v.id])
        | _ -> None
      this.Map aux

  
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
    }
    
    member this.IsCertified =
      match this.message with
        | InfonCert _ -> true
        | _ -> false
    
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
 
