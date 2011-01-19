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
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Text
open Microsoft.Research.DkalEngine.Util

module Ast =
  //
  // The real final AST
  //
  
  type PP = SExpr.PP
  type SX = SExpr.SX

  type Type =
    {
      id : int
      name : string
    }
    
    override this.ToString() =
      this.name + this.id.ToString()

    member this.ToSX() =
      SX.App (fakePos, this.name, [])
  
  type Var =
    { id : int; name : string; mutable typ : Type }
    
    override this.ToString() = this.name
    member this.ToSXRef() = SX.Var (fakePos, this.name)
    member this.ToSXDecl() = SX.App (fakePos, this.typ.name, [this.ToSXRef()])

  [<ReferenceEquality; NoComparison>]
  type Function =
    {
      id : int
      name : string
      retType : Var
      argTypes : list<Var>
      mutable body : obj
    }

    member this.IsFree = this.body = null
    
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

                  
  let (===) = LanguagePrimitives.PhysicalEquality

  let private nextId =
    let curr = ref 0
    function () -> incr curr; !curr
  
  let private infon : Type = { id = nextId(); name = "infon" }
  let private assertion : Type = { id = nextId(); name = "*assertion*" }
  let private unbound : Type = { id = nextId(); name = "*unbound*" }
  let private evidence : Type = { id = nextId(); name = "*evidence*" }
  let private principal : Type = { id = nextId(); name = "principal" }
  let private intType : Type = { id = nextId(); name = "int" }
  let private boolType : Type = { id = nextId(); name = "bool" }
  let private textType : Type = { id = nextId(); name = "text" }
  let private floatType : Type = { id = nextId(); name = "float" }
  
  type Type with
    static member Infon = infon
    static member Assertion = assertion
    static member Unbound = unbound
    static member Principal = principal
    static member Int = intType
    static member Bool = boolType
    static member Float = floatType
    static member Text = textType
    static member Evidence = evidence
  
  let globalFunctions = dict() // accessed from ParsingCtx
  let private mkVar tp =
    let id = nextId()
    ({ id = id; name = "global#" + id.ToString(); typ = tp} : Var)
  let private addGlobalFunction retType name argTypes =
    let fn = { retType = mkVar retType; name = name; argTypes = List.map mkVar argTypes; id = nextId(); body = null }
    globalFunctions.Add (fn.name, fn)
    fn
    
  let private infonAnd = addGlobalFunction Type.Infon "and" [Type.Infon; Type.Infon]
  let private infonFollows = addGlobalFunction Type.Infon "follows" [Type.Infon; Type.Infon]
  let private infonSaid = addGlobalFunction Type.Infon "said" [Type.Principal; Type.Infon]
  let private infonImplied = addGlobalFunction Type.Infon "implied" [Type.Principal; Type.Infon]
  let private infonEmpty = addGlobalFunction Type.Infon "empty" []
  let private infonAsInfon = addGlobalFunction Type.Infon "asInfon" [Type.Bool]  
  let private infonCertified = addGlobalFunction Type.Infon "justified" [Type.Infon; Type.Evidence]
  let private substrateEq = addGlobalFunction Type.Bool "==" [Type.Unbound; Type.Unbound]  
  
  // the int parameter is a placeholder for the actual cryptographic signature
  let private evSignature = addGlobalFunction Type.Evidence "Ev.signedBy" [Type.Principal; Type.Infon; Type.Int]
  let private evMp = addGlobalFunction Type.Evidence "Ev.mp" [Type.Evidence; Type.Evidence]
  let private evAsInfon = addGlobalFunction Type.Evidence "Ev.asInfon" [Type.Infon]
  let private evAnd = addGlobalFunction Type.Evidence "Ev.and" [Type.Evidence; Type.Evidence]
  
  type Function with
    static member And = infonAnd
    static member Follows = infonFollows
    static member Said = infonSaid
    static member Implied = infonImplied
    static member Empty = infonEmpty
    static member AsInfon = infonAsInfon
    static member Eq = substrateEq
    static member Cert = infonCertified
    static member EvSignature = evSignature
    static member EvMp = evMp
    static member EvAsInfon = evAsInfon
    static member EvAnd = evAnd
  
  /// Principals are fully identified by name. New principals should be created only using
  /// ParsingCtx.LookupOrAddPrincipal method.
  [<StructuralEquality; NoComparison>]
  type Principal =
    { internal_id : int; name : string; typ : Type } 
    
    member this.Name = this.name
    override this.ToString() = this.name // + ":" + this.internal_id.ToString()
  
  type Const =
    | Principal of Principal
    | Int of int
    | Bool of bool
    | Float of float
    | Text of string
    | Column of string * string
    
    override this.ToString() =
      match this with
        | Int i -> i.ToString()
        | Principal p -> p.ToString()
        | Column (a, b) -> a + "." + b
        | Text s -> "\"" + s + "\""
        | Bool true -> "true"
        | Bool false -> "false"
        | Float f -> f.ToString()
  
  // YUCK!!!
  let private termToStringCallback = ref (fun (sb:StringBuilder, t:obj) -> failwith "should be assigned"; ())
  
  /// Represent substrate terms as well as infons.
  [<StructuralEquality; NoComparison>]
  type Term =
    | App of Function * list<Term>
    | Const of Const
    | Var of Var
    
    member this.Map f =
      let rec aux t = 
        match f t with
          | Some r -> r
          | None ->
            match t with
              | Term.App (fn, args) -> Term.App (fn, List.map aux args)
              | Const _
              | Var _ -> t
      aux this
        
    member this.Type =
      match this with
        | App (f, _) -> f.retType.typ
        | Var (v) -> v.typ
        | Const (Const.Int _) -> Type.Int
        | Const (Const.Principal _) -> Type.Principal
        | Const (Const.Bool _) -> Type.Bool
        | Const (Const.Text _) -> Type.Text
        | Const (Const.Float _) -> Type.Float
        | Const (Const.Column _) -> Type.Unbound
        
    member this.Vars() =
      let vars = dict()
      let varList = ref []
      let add (v:Var) =
        if vars.ContainsKey v.id then ()
        else
          varList := v :: !varList
          vars.Add (v.id, true)
      this.Map (function Term.Var v -> add v; None | _ -> None) |> ignore
      !varList |> List.rev
        
    override this.ToString() =
      tempStringBuilder (fun sb -> !termToStringCallback (sb, (this :> obj)))
    
    static member True = Const (Const.Bool true)

    member this.ToSX () =
      match this with
        | App (f, args) -> SX.App (fakePos, f.name, args |> List.map (fun t -> t.ToSX()))
        | Var (v) -> SX.Var (fakePos, v.name)
        | Const (Const.Int k) -> SX.Int (fakePos, k)
        | Const (Const.Principal p) -> SX.App (fakePos, p.name, [])
        | Const (Const.Bool true) -> SX.App (fakePos, "true", [])
        | Const (Const.Bool false) -> SX.App (fakePos, "false", [])
        | Const (Const.Float f) -> SX.Float (fakePos, f)
        | Const (Const.Text s) -> SX.String (fakePos, s)
        | Const (Const.Column (t,c)) -> SX.App (fakePos, t + "." + c, [])
      
    member this.CheckSum () =
      let sha = new System.Security.Cryptography.SHA1CryptoServiceProvider()
      let bytes = System.Text.Encoding.UTF8.GetBytes(this.ToString())
      let hash = sha.ComputeHash bytes
      hash |> Seq.map (fun b -> b.ToString("X2")) |> String.concat ""

    member this.ShortCheckSum () = "#" + this.CheckSum().Substring(0, 6)
      
    member this.ToPrettyString () = 
      let indentLines (s: string) = 
        let lines = s.Split([|"\n"|], System.StringSplitOptions.None)
        String.concat "\n" (Seq.map (fun s -> "    " + s) lines) 

      match this with
      | App (f, args) -> match f.name, args with
                         | "asInfon", [t] -> "asInfon(" + t.ToPrettyString() + ")"
                         | "justified", [t; _] -> t.ToPrettyString()
                         | "and", [t1; t2] -> t1.ToPrettyString() + "\n" + t2.ToPrettyString()
                         | "follows", [t1; t2] -> "if\n" + indentLines (t1.ToPrettyString()) + "\nthen\n" + indentLines (t2.ToPrettyString())
                         | "said", [t1; t2] 
                         | "implied", [t1; t2] 
                         | "<", [t1; t2] 
                         | ">", [t1; t2] 
                         | "<=", [t1; t2] 
                         | ">=", [t1; t2]
                         | "==", [t1; t2] -> 
                            t1.ToPrettyString() + " " + f.name + " " + t2.ToPrettyString()
                         | "&&", [t1; t2] 
                         | "||", [t1; t2] 
                         | "-", [t1; t2] 
                         | "+", [t1; t2] 
                         | "*", [t1; t2] 
                         | "/", [t1; t2] -> 
                            "(" + t1.ToPrettyString() + ") " + f.name + " (" + t2.ToPrettyString() + ")"
                         | "not", [t] -> 
                            "not(" + t.ToPrettyString() + ")"
                         | fn, ts when f.body = null -> 
                            let fParts = fn.Split([|"-*-";"*-";"-*"|], System.StringSplitOptions.None)
                            let fParts = Array.map (fun (s: string) -> s.Replace("-"," ")) fParts
                            if ts.Length <> fParts.Length - 1 then
                              failwith ("Incorrect amount of arguments for: " + fn)
                            elif ts.Length > 0 then
                              let ret = ref ""
                              Seq.iteri (fun (i: int) (t: Term) -> ret.Value <- ret.Value + fParts.[i] + " " + (t.ToPrettyString()) + " ") ts
                              ret.Value <- ret.Value + fParts.[fParts.Length - 1]
                              ret.Value
                            else
                              fParts.[0]
                         | fn, ts -> 
                           fn + "(" + (String.concat ", " (List.map (fun (t: Term) -> t.ToPrettyString()) ts)) + ")"
      | Var (v) -> v.name
      | Const (Const.Int k) -> k.ToString()
      | Const (Const.Principal p) -> p.name
      | Const (Const.Bool b) -> b.ToString().ToLower()
      | Const (Const.Float f) -> f.ToString()
      | Const (Const.Text s) -> "\"" + s + "\""
      | Const (Const.Column (t,c)) -> t + "." + c

  type PrincipalTerm = Term  
  type Infon = Term

  let optimizeSX = SX.OptimizeList

  type Function with
    member this.ToSX() =
      let app (a, n) = SX.App (fakePos, a, n)
      let pref = [this.retType.typ.ToSX(); SX.Var (fakePos, this.name)]
      if this.body = null then
        app ("fun", pref @ (this.argTypes |> List.map (fun v -> v.typ.ToSX())))
      else
        app ("macro", pref @ (this.argTypes |> List.map (fun v -> v.ToSXDecl())) @ [(this.body :?> Term).ToSX()])

  
  
  let (|InfonAnd|_|) = function
    | App (fn, [a; b]) when fn === Function.And -> Some (InfonAnd (a, b))
    | _ -> None
  
  let (|InfonFollows|_|) = function
    | App (fn, [a; b]) when fn === Function.Follows -> Some (InfonFollows (a, b))
    | _ -> None
  
  let (|InfonSaid|_|) = function
    | App (fn, [a; b]) when fn === Function.Said -> Some (InfonSaid (a, b))
    | _ -> None
  
  let (|InfonImplied|_|) = function
    | App (fn, [a; b]) when fn === Function.Implied -> Some (InfonImplied (a, b))
    | _ -> None
  
  let (|InfonEmpty|_|) = function
    | App (fn, []) when fn === Function.Empty -> Some (InfonEmpty)
    | _ -> None
  
  let (|AsInfon|_|) = function
    | App (fn, [a]) when fn === Function.AsInfon -> Some (AsInfon a)
    | _ -> None
  
  let (|InfonCert|_|) = function
    | App (fn, [a; b]) when fn === Function.Cert -> Some (InfonCert (a, b))
    | _ -> None
  
  type Term with
    static member And (a, b) = App (Function.And, [a; b])
    static member Follows (a, b) = App (Function.Follows, [a; b])
    static member Said (a, b) = App (Function.Said, [a; b])
    static member Implied (a, b) = App (Function.Implied, [a; b])
    static member Empty = App (Function.Empty, [])
    static member Cert (i, e) = App (Function.Cert, [i; e])
    
    member this.IsEmpty =
      match this with
        | InfonEmpty -> true
        | _ -> false

  let private infonToString (sb, (t_:obj)) =
    let par pp = ((PP.Block pp).Append ")").Prepend "("
    let s s = PP.PString s
    let rec pr = function
      | InfonFollows (InfonSaid (p, a), a') when a = a' ->
        par [s (p.ToString()); s "tdonS"; pr a]
      | InfonFollows (InfonImplied (p, a), a') when a = a' ->
        par [s (p.ToString()); s "tdonI"; pr a]
      | InfonFollows (a, b) ->
        par [pr a; s "==>"; pr b]
      | InfonAnd (a, b) ->
        par [pr a; s "&&"; pr b]
      | InfonSaid (p, i) ->
        PP.Block [s (p.ToString()); s "said"; pr i]
      | InfonImplied (p, i) ->
        PP.Block [s (p.ToString()); s "implied"; pr i]
      | InfonEmpty -> s "empty"

      | App (f, [p; m; sgn]) when f === Function.EvSignature ->
        par [s (sgn.ShortCheckSum() + " signed by"); pr p]
      | App (f, [AsInfon (p)]) when f === Function.EvAsInfon ->
        par [s "check"; pr p]

      | App (f, args) ->
        f.WriteAsInfix (List.map pr args)
      | Var v -> s (v.name)
      | Const p -> s (p.ToString())
    (pr (t_ :?> Term)).Print 90 sb
    if sb.Chars (sb.Length - 1) = '\n' then
      sb.Length <- sb.Length - 1

  do termToStringCallback := infonToString

  type Term with
    member this.Sanitize () =
      let vars = dict()
      let aux = function
        | Term.Var v ->
          if not (vars.ContainsKey v.id) then
            vars.[v.id] <- Term.Var { v with name = String((char)((int)'A' + vars.Count), 1) }
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
    member this.ToPrettyString() = 
      let indentLines (s: string) = 
        let lines = s.Split([|"\n"|], System.StringSplitOptions.None)
        String.concat "\n" (Seq.map (fun s -> "    " + s) lines) 

      this.ai.principal.name + " knows\n" +
        indentLines (this.infon.ToPrettyString()) + "\n"
  
  type CommKind =
    | Processed
    | Certified
    | CertifiedSay

  type Communication =
    {
      ai : AssertionInfo
      target : PrincipalTerm
      message : Infon
      proviso : Infon
      trigger : Infon
      certified : CommKind
    }

    member this.ToPrettyString() = 
      let indentLines (s: string) = 
        let lines = s.Split([|"\n"|], System.StringSplitOptions.None)
        String.concat "\n" (Seq.map (fun s -> "    " + s) lines)

      "if " + this.ai.principal.name + " knows\n" +
        indentLines (this.trigger.ToPrettyString()) + "\n" +
        "then\n" +
        indentLines ("say to " + this.target.ToPrettyString() + "\n") +
        indentLines (indentLines (this.message.ToPrettyString())) + "\n"

    member this.ToSX() =
      let app (n, a) = SX.App (fakePos, n, a)
      let target() =        
        let args = [this.target.ToSX(); this.message.ToSX()]
        match this.certified with
          | Certified -> app ("to", args)
          | CertifiedSay -> app ("say", args)
          | Processed -> app ("say*", args)
      match this.proviso with
        | InfonEmpty ->           
          app ("send", [this.trigger.ToSX(); target()])
        | _ ->
          failwith "provisional communication not supported for SExpressions"
    
  type Filter =
    {
      ai : AssertionInfo
      source : PrincipalTerm
      message : Infon
      proviso : Infon
      trigger : Infon
    }

  /// A message to be sent over wire, or recived over wire. It usually involves serialization
  /// using Serializer class (for message and proviso), and using ICommunicator PrincipalId methods
  /// (for source and target).
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

  /// A communication assertion, usually corresponds to a single statement in
  /// the policy file.
  type Assertion =
    | Knows of Knows
    | SendTo of Communication
    | ReceiveFrom of Filter
    
    member this.AssertionInfo =
      match this with
        | Knows k -> k.ai
        | SendTo c -> c.ai
        | ReceiveFrom f -> f.ai

    member this.ToPrettyString() = 
      match this with
      | Knows k -> k.ToPrettyString()
      | SendTo c -> c.ToPrettyString()
      | _ -> failwith "Filters not supported for pretty strings"

    member this.ToSX() =
      let app (n, a) = SX.App (fakePos, n, a)
      match this with
        | Knows k -> app ("knows", [k.infon.ToSX()])
        | SendTo c -> c.ToSX()
        | ReceiveFrom _ -> failwith "filters not supported"
          
    override this.ToString() =
      match this with
        | Knows k ->
          k.ai.principal.ToString() + " : " + k.infon.ToString()
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
                | Term.Var v when s.ContainsKey v.id -> 
                  Some (s.[v.id].Apply s) 
                | _ -> None)
      
  let substToString (s:Subst) =
    s |> Map.fold (fun acc k v -> (k.ToString() + " -> " + v.ToString()) :: acc) [] |> List.rev |> String.concat ", " 
    
  type AugmentedSubst =
    {
      subst : Subst
      assumptions : list<Term>
    }
    
    static member Empty = { subst = Map.empty; assumptions = [] }
    static member NoAssumptions s = { subst = s; assumptions = [] }
    override this.ToString() =
      substToString this.subst + " ::: " + l2s this.assumptions
  
  let rec unifyList unify s = function
    | [] -> s
    | x :: xs ->
      match s with
        | None -> None
        | Some s -> unifyList unify (unify s x) xs

  let rec occursTerm (v:Var) = function
    | Term.App (_, args) ->
      List.exists (occursTerm v) args
    | Term.Const _ -> false
    | Term.Var v' -> v.id = v'.id
    
  let rec unifyTerms (subst:Subst) = function
    | (Term.Var v1, Term.Var v2) when v1.id = v2.id -> Some subst
    | (Term.Var v, t) ->
      if subst.ContainsKey v.id then
        unifyTerms subst (subst.[v.id], t)
      else
        if occursTerm v t then None
        else Some (subst.Add (v.id, t))
    | (t, (Term.Var _ as t')) ->
      unifyTerms subst (t', t)
    | (Term.App (f, args), Term.App (f', args')) when f.name = f'.name ->
      unifyList unifyTerms (Some subst) (List.zip args args')
    | (Term.Const (Principal p1), Term.Const (Principal p2)) when p1.name = p2.name -> Some subst
    | (Term.Const p1, Term.Const p2) when p1 = p2 -> Some subst
    | _ -> None
    
    
  //
  // Canonical form
  //
    
  let canonicalVariables = dict()
   
  type Term with
    member this.Canonical() =
      let currentSubst = dict()
      let rec aux = function
        | Term.App (f, args) ->
          Term.App (f, List.map aux args)
        | Term.Const c -> Term.Const c
        | Term.Var v ->
          if not (currentSubst.ContainsKey v.id) then
            let id = currentSubst.Count + 100
            let key = (v.typ, id)
            if not (canonicalVariables.ContainsKey key) then
              canonicalVariables.Add (key, ({ id = id; typ = v.typ; name = "V#" + id.ToString() } : Var))
            let repl = canonicalVariables.[key]
            currentSubst.[v.id] <- Term.Var repl
          currentSubst.[v.id]
      aux this
  
    static member Canonicalize lst =
      match (Term.App (Function.Empty, lst)).Canonical() with
        | Term.App (_, lst) -> lst
        | _ -> failwith "impossible"

  type Message with
    member this.Canonical() =
      match Term.Canonicalize [this.message; this.proviso] with
        | [m; p] ->
          { this with message = m; proviso = p }
        | _ -> failwith "impossible"
 
