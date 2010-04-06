namespace Microsoft.Research.DkalEngine

open System
open System.IO
open System.Text
open Microsoft.FSharp.Text
open Microsoft.FSharp.Collections

open Microsoft.Research.DkalEngine.Util
open Microsoft.Research.DkalEngine.Ast

type GQueue<'a> = Collections.Generic.Queue<'a>

type Pref =
  | Said of PrincipalTerm
  | Implied of PrincipalTerm

type SubstSet =
  { substs : list<Subst> }
  
  static member Empty = { substs = [] }
  static member Single s = { substs = [s] }
  member this.All = this.substs
  member this.Add (ss:SubstSet) = { substs = this.substs @ ss.substs }

type Binding =
  {
    formal : Var
    actual : Term
  }

type IViewHooks =
  abstract Recieved : Message -> unit
  abstract Send : Message -> unit
  abstract Knows : Knows -> unit
  abstract QueryResults : Infon * seq<seq<Binding>> -> unit
  abstract SyntaxError : Pos * string -> unit
  abstract Loaded : string -> unit
  
type Action = delegate of unit -> unit
  
type Engine =
  {
    mutable sql : option<SqlConnector>
    mutable comm : option<SqlCommunicator>
    mutable me : option<Principal>
    ctx : PreAst.Context
    sentItems : Dict<Message, bool>
    hooks : IViewHooks
    pending : GQueue<Action>
    mutable trace : int
    mutable infonstrate : list<Knows>
    mutable filters : list<Filter>
    mutable communications : list<Communication>
    mutable nextId : int   
    mutable die : bool
  }
  
  static member Make (trace, hooks) =
    let ctx = PreAst.Context.Make()
    ctx.trace <- trace
    { me = None; ctx = ctx; hooks = hooks;
      infonstrate = []; filters = []; communications = []; nextId = 0;
      sentItems = dict()
      trace = trace
      sql = None
      comm = None
      pending = new GQueue<_>()
      die = false
      }

  member this.Load filename =
    try
      let ctx = this.ctx
      let prelude = Tokenizer.fromString Prelude.text
      let toks = prelude @ Tokenizer.fromFile filename
      Parser.addStandardRules ctx
      let toks = Parser.addRules ctx toks
      let toks = Parser.applyRules ctx toks
      //System.Console.WriteLine (Tok.Block (fakePos, toks))
      Resolver.resolveFunctions ctx
      let assertions = List.map (Resolver.resolve ctx) toks |> List.concat
      let me = ctx.principals.[ctx.options.["me"]]
      
      this.me <- Some me
      this.sql <- Some (SqlConnector(ctx.options.["private_sql"]))
      this.comm <- Some (SqlCommunicator(ctx, me))
            
      this.AddDefaultFilter()
      this.Populate assertions
      
      this.hooks.Loaded filename
            
    with SyntaxError (pos, s) ->
      this.hooks.SyntaxError (pos, s)
  
  member this.SetTrace v =
    this.ctx.trace <- v
    this.trace <- v

  member this.Populate assertions =
    let myId = this.me.Value.internal_id
    let addAssertion = function
      | Knows k when k.ai.principal.internal_id = myId ->
        this.infonstrate <- k :: this.infonstrate
      | SendTo c when c.ai.principal.internal_id = myId ->
        this.communications <- c :: this.communications
      | ReceiveFrom f when f.ai.principal.internal_id = myId ->
        this.filters <- f :: this.filters
      | _ -> ()
    List.iter addAssertion assertions
    
  member private this.NextId () =
    this.nextId <- this.nextId - 1
    this.nextId
  
  member private this.FreshVar tp =
    let id = this.NextId()
    ({ typ = tp; id = id; name = tp.name + "#" + id.ToString() } : Var)

  member private this.Freshen (infon:Infon) =
    let subst = ref Map.empty
    let repl (v:Var) =
      if not ((!subst).ContainsKey v.id) then
        let id = this.NextId()
        subst := (!subst).Add (v.id, { v with id = id; name = v.name + "#" + id.ToString() })      
      (!subst).[v.id]
    infon.Map (function Term.Var (p, v) -> Some (Term.Var (p, repl v)) | _ -> None)
    
  member private this.FreshenList infons =
    match this.Freshen (Term.App (fakePos, Function.Empty, infons)) with
      | Term.App (_, _, l) -> l
      | _ -> failwith "cannot happen"
      
  member private this.FakeAI () =
    { origin = fakePos; principal = this.me.Value }
    
  member this.AddDefaultFilter () =
    let src = this.FreshVar Type.Principal
    let msg = this.FreshVar Type.Infon
    let proviso = this.FreshVar Type.Infon
    let filter =
      {
        ai = this.FakeAI()
        source = Term.Var (fakePos, src)
        message = Infon.Var (fakePos, msg)
        proviso = Infon.Var (fakePos, proviso)
        trigger = Infon.Empty fakePos
      }
    this.filters <- filter :: this.filters
  
  member private this.InfonsWithPrefix subst pref template =
    let res = ref []
    let rec stripPrefix subst prefixUnif preconds suff = 
      let simpl s (t:Term) = t.Apply s
      
      let immediate = function
        | ([], i) ->
          let rec unifyAndSimpl s = function
            | [] -> s
            | (a, b) :: xs ->
              match s with
                | None -> None
                | Some s -> unifyAndSimpl (unifyTerms s (simpl s a, simpl s b)) xs
        
          if this.trace >= 2 then
            System.Console.Write ("immediate result " + String.concat ", " (prefixUnif |> List.map (fun (a, b) -> String.Format ("{0} =?= {1}", a, b)) ))
          match unifyAndSimpl (Some subst) ((template, i) :: prefixUnif) with
            | Some subst ->
              if this.trace >= 2 then
                System.Console.WriteLine (" YES")
              res := (subst, preconds) :: !res
            | None ->
              if this.trace >= 2 then
                System.Console.WriteLine (" NO")
        | _ -> ()
             
      function
      | (Pref.Implied t1 :: pref, InfonSaid (p, t2, i))
      | (Pref.Implied t1 :: pref, InfonImplied (p, t2, i)) ->
        match unifyTerms subst (simpl subst t1, simpl subst t2) with
          | Some subst ->
            stripPrefix subst prefixUnif preconds (fun i -> suff (Infon.Implied (p, t2, i))) (pref, i)
          | None -> 
            stripPrefix subst ((t1, t2) :: prefixUnif) preconds (fun i -> suff (Infon.Implied (p, t2, i))) (pref, i)
      | (Pref.Said t1 :: pref, InfonSaid (p, t2, i)) ->
        match unifyTerms subst (simpl subst t1, simpl subst t2) with
          | Some subst -> 
            stripPrefix subst prefixUnif preconds (fun i -> suff (Infon.Said (p, t2, i))) (pref, i)
          | None ->
            stripPrefix subst ((t1, t2) :: prefixUnif) preconds (fun i -> suff (Infon.Said (p, t2, i))) (pref, i)
      | (pref, InfonAnd (_, a, b)) ->
        stripPrefix subst prefixUnif preconds suff (pref, a)
        stripPrefix subst prefixUnif preconds suff (pref, b)
      | (pref, InfonFollows (_, a, b)) as t ->
        immediate t
        stripPrefix subst prefixUnif (suff a :: preconds) suff (pref, b)
      | (pref, Infon.Var (_, v)) when subst.ContainsKey v.id ->
        stripPrefix subst prefixUnif preconds suff (pref, subst.[v.id])
      | t -> immediate t
    
    List.iter (fun k -> stripPrefix subst [] [] (fun x -> x) (pref, this.Freshen k.infon)) this.infonstrate
    !res
      
  
  member this.DoDerive pref (subst:AugmentedSubst) infon =
    if this.trace > 0 then
      System.Console.WriteLine ("derive: {0} under {1}", infon, substToString subst.subst)
    
    let sum f lst =
      List.fold (fun acc e -> f e @ acc) [] lst
      
    match infon with
      | InfonAnd (_, i1, i2) ->
        (this.DoDerive pref subst i1) |> sum (fun s1 -> this.DoDerive pref s1 i2)
      | InfonEmpty _ -> [subst]
      | InfonSaid (_, p, i) ->
        this.DoDerive (Pref.Said p :: pref) subst i
      | InfonImplied (_, p, i) ->
        this.DoDerive (Pref.Implied p :: pref) subst i
      | Infon.Var (_, v) when subst.subst.ContainsKey v.id ->
        this.DoDerive pref subst subst.subst.[v.id]
      | AsInfon (_, e) ->
        if pref <> [] then
          failwith "asInfon(...) under said/implied prefix"
        [{ subst with assumptions = e :: subst.assumptions }]
      | templ ->
        let rec checkOne = function
          | (subst, precond :: rest) ->
            this.DoDerive [] subst precond |> sum (fun s -> checkOne (s, rest))
          | (subst, []) -> [subst]
        this.InfonsWithPrefix subst.subst pref templ |> List.map (fun (s, p) -> ({subst with subst = s }, p)) |> sum checkOne

  member private this.Send msg =
    if this.sentItems.ContainsKey msg then
      ()
    else
      this.sentItems.Add (msg, true)
      this.hooks.Send msg
      this.comm.Value.SendMessage msg
      
  member this.Derive s (infon:Term) =
    let vars = (infon.Apply s).Vars()
    let checkAssumptions (augS:AugmentedSubst) =
      let apply (t:Term) = t.Apply augS.subst
      let sqlExpr = SqlCompiler.compile this.ctx this.NextId (augS.assumptions |> List.map apply)
      SqlCompiler.execQuery (this.sql.Value, this.comm.Value, sqlExpr, augS.subst, vars) |> Seq.toList      
    { substs = this.DoDerive [] (AugmentedSubst.NoAssumptions s) infon |> List.map checkAssumptions |> List.concat }
    
  member this.Listen (msg:Message) =    
    this.hooks.Recieved msg
    let src = Term.Const (fakePos, Const.Principal msg.source)
    let msg =
      match this.FreshenList [msg.message; msg.proviso] with
        | [m;p] -> { msg with message = m ; proviso = p }
        | _ -> failwith "impossible"
    let matches (filter:Filter) =
      let subst =
        unifyList unifyTerms (Some Map.empty) [(src, filter.source); 
                                               (msg.message, filter.message);
                                               (msg.proviso, filter.proviso)]
      let apply s =
        let t = msg.message.Apply s
        let infon =
          match msg.proviso.Apply s with
            | InfonEmpty ->
              if msg.certified then t
              else Infon.Said (fakePos, src, t)
            | proviso ->
              // certified implication, not clear if it should be here, but the example requires it
              if msg.certified then 
                Infon.Follows (fakePos, proviso, t)
              else
                Infon.Follows (fakePos, proviso, Infon.Implied (fakePos, src, t))
        { ai = this.FakeAI(); infon = infon }
          
      match subst with
        | Some s ->
          (this.Derive s filter.trigger).All |> List.map apply
        | None -> []
    let newInfons = this.filters |> List.map matches |> List.concat
    List.iter this.hooks.Knows newInfons
    this.infonstrate <- newInfons @ this.infonstrate
    
  member this.Listen () =
    match this.comm.Value.CheckForMessage() with
      | Some m -> this.Listen m
      | None -> ()
    
  member this.Me = this.me
  
  member this.Talk () =
    let runCommFor (comm:Communication) (subst:Subst) =
      match comm.target.Apply subst with
        | Term.Const (_, Const.Principal p) ->
          let msg = ({ source = this.me.Value
                       target = p
                       message = comm.message.Apply subst
                       proviso = comm.proviso.Apply subst
                       certified = comm.certified
                     } : Message).Canonical()
          this.Send msg
        | t ->
          System.Console.WriteLine ("attempting broadcast message: " + t.ToString())
          
    this.communications |> List.iter (fun comm -> (this.Derive Map.empty comm.trigger).All |> List.iter (runCommFor comm))
  
  member this.ParseInfon s = 
    try
      let toks = Tokenizer.fromString s
      let toks = Parser.applyRules this.ctx toks
      match toks with
        | [t]
        | [t; PreAst.Tok.NewLine _] -> Some (Resolver.resolveInfon this.ctx t)
        | [PreAst.Tok.NewLine _]
        | [] -> raise (SyntaxError (fakePos, "infon expected"))
        | _ -> raise (SyntaxError (fakePos, "only one infon expected"))
    with 
      | SyntaxError (pos, s) ->
        this.hooks.SyntaxError (pos, s)
        None
              
  member this.Ask (i:string) =
    match this.ParseInfon i with
      | Some i -> 
        let bind (s:Subst) =
          i.Vars() |> Seq.map (fun v -> { formal = v; actual = s.[v.id].Apply s })
        this.hooks.QueryResults (i, (this.Derive Map.empty i).All |> Seq.map bind)
      | None -> ()
    
  member this.Add i =
    match this.ParseInfon i with
      | Some i -> this.infonstrate <- { ai = this.FakeAI(); infon = i } :: this.infonstrate
      | None -> ()
  
  member this.Invoke a = 
    let wrapped () =
      try a()
      with e ->
        this.hooks.SyntaxError (fakePos, "exception: " + e.Message)
    lock this.pending (fun () -> this.pending.Enqueue (fun () -> wrapped ()))
  
  member this.AsyncDie () = this.Invoke (fun () -> this.die <- true)
  member this.AsyncAsk i = this.Invoke (fun () -> this.Ask i)
  member this.AsyncAdd i = this.Invoke (fun () -> this.Add i)
  member this.AsyncLoad n = this.Invoke (fun () -> this.Load n)
  
  member this.EventLoop () =
    let doYield () = System.Threading.Thread.Sleep 1000
    let rec loop () =
      let act =
        lock this.pending (fun () -> if this.pending.Count > 0 then Some (this.pending.Dequeue()) else None)
      match act with
        | Some a -> a.Invoke()
        | None when this.sql.IsNone -> doYield()
        | None ->
          let cnt = this.sentItems.Count
          this.Talk()
          if cnt = this.sentItems.Count then
            match this.comm.Value.CheckForMessage() with
              | Some msg -> this.Listen msg
              | None ->
                doYield()
      if not this.die then loop ()
    loop ()
    