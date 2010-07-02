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


type Action = delegate of unit -> unit

/// DKAL execution engine. Corresponds to a infostrate and substrate of a single principal.
type Engine =
  {
    mutable sql : option<SqlConnector>
    mutable me : option<Principal>
    mutable worker : option<System.Threading.Thread>
    mutable comm : option<ICommunicator>
    sentItems : Dict<Message, bool>
    pending : GQueue<unit -> unit>
    options : Options
    mutable infonstrate : list<Knows>
    mutable filters : list<Filter>
    mutable communications : list<Communication>
    mutable nextId : int
    mutable finish : bool   
  }
  
  /// Create a new engine instance.
  static member Config (opts:Options) =
    let this =
      { me = None; worker = None;
        infonstrate = []; filters = []; communications = []; nextId = 0;
        comm = None;
        sentItems = dict()
        sql = None
        pending = new GQueue<_>()
        options = opts
        finish = false
        }
    this

  member private this.Comm = this.comm.Value

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
    infon.Map (function Term.Var v -> Some (Term.Var (repl v)) | _ -> None)
    
  member private this.FreshenList infons =
    match this.Freshen (Term.App (Function.Empty, infons)) with
      | Term.App (_, l) -> l
      | _ -> failwith "cannot happen"
      
  member private this.FakeAI () =
    { origin = fakePos; principal = this.me.Value }
    
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
          let trace = this.options.Trace
          if trace >= 2 then
            System.Console.Write ("immediate result " + String.concat ", " (prefixUnif |> List.map (fun (a, b) -> String.Format ("{0} =?= {1}", a, b)) ))
          match unifyAndSimpl (Some subst) ((template, i) :: prefixUnif) with
            | Some subst ->
              if trace >= 2 then
                System.Console.WriteLine (" YES")
              res := (subst, preconds) :: !res
            | None ->
              if trace >= 2 then
                System.Console.WriteLine (" NO")
        | _ -> ()
             
      function
      | (Pref.Implied t1 :: pref, InfonSaid (t2, i))
      | (Pref.Implied t1 :: pref, InfonImplied (t2, i)) ->
        match unifyTerms subst (simpl subst t1, simpl subst t2) with
          | Some subst ->
            stripPrefix subst prefixUnif preconds (fun i -> suff (Infon.Implied (t2, i))) (pref, i)
          | None -> 
            stripPrefix subst ((t1, t2) :: prefixUnif) preconds (fun i -> suff (Infon.Implied (t2, i))) (pref, i)
      | (Pref.Said t1 :: pref, InfonSaid (t2, i)) ->
        match unifyTerms subst (simpl subst t1, simpl subst t2) with
          | Some subst -> 
            stripPrefix subst prefixUnif preconds (fun i -> suff (Infon.Said (t2, i))) (pref, i)
          | None ->
            stripPrefix subst ((t1, t2) :: prefixUnif) preconds (fun i -> suff (Infon.Said (t2, i))) (pref, i)
      | (pref, InfonAnd (a, b)) ->
        stripPrefix subst prefixUnif preconds suff (pref, a)
        stripPrefix subst prefixUnif preconds suff (pref, b)
      | (pref, InfonFollows (a, b)) as t ->
        immediate t
        stripPrefix subst prefixUnif (suff a :: preconds) suff (pref, b)
      | (pref, Infon.Var v) when subst.ContainsKey v.id ->
        stripPrefix subst prefixUnif preconds suff (pref, subst.[v.id])
      | t -> immediate t
    
    List.iter (fun k -> stripPrefix subst [] [] (fun x -> x) (pref, this.Freshen k.infon)) this.infonstrate
    !res
  
  member this.DeriveCertification subst = 
    let streight goal =
      let aux = function
        | { infon = InfonCert (inf, pr) } -> this.TryDerive subst pr (goal, inf)
        | _ -> []
      List.collect aux this.infonstrate
    function
    | AsInfon (t) as goal ->
      [({subst with assumptions = t :: subst.assumptions}, Term.App (Function.EvAsInfon, [goal]))]
    | InfonAnd (i1, i2) as goal ->
      let left (subst, prl) =
        let aux (subst, prr) =
          (subst, Term.App (Function.EvAnd, [prl; prr]))
        this.DeriveCertification subst i2 |> List.map aux
      streight goal @ (this.DeriveCertification subst i1 |> List.collect left)
    | goal -> streight goal

  member this.TryDerive (subst:AugmentedSubst) pr =
    let streight (goal, premise) = 
      if this.options.Trace > 0 then
        System.Console.WriteLine ("streight compare: {0} vs {1}", goal, premise)
      match unifyTerms subst.subst (goal, premise) with
        | Some s -> [({ subst with subst = s }, pr)]
        | None -> 
          if this.options.Trace > 0 then
            System.Console.WriteLine ("NOPE")
          []
    function
    (*
    | (InfonSaid (p1, i1), InfonSaid (p2, i2), pr)
    | (InfonImplied (p1, i1), InfonSaid (p2, i2), pr)
    | (InfonImplied (p1, i1), InfonImplied (p2, i2), pr) ->
      match unifyTerms subst t1 t2 with
        | Some subst -> None          
        | None -> None
    *)
    | (goal, InfonFollows (i1, i2)) as p ->
      let v = this.FreshVar Type.Evidence
      let derivePremise (subst, (goalPr:Term)) =
        if this.options.Trace > 0 then
          System.Console.WriteLine ("[ looking for premise: {0} vs {1} -> {2}", goal, i1, i2)
        let updateProof (subst, premisePr) =
          if this.options.Trace > 0 then
            System.Console.WriteLine ("found premise")
          let repl = Term.App (Function.EvMp, [premisePr; pr])
          (subst, goalPr.Apply (Map.add v.id repl Map.empty))
        let tmp = this.DeriveCertification subst i1 
        if this.options.Trace > 0 then
          System.Console.WriteLine ("] done looking, {0}", tmp.Length)
        tmp |> List.map updateProof
      streight p @ List.collect derivePremise (this.TryDerive subst (Term.Var v) (goal, i2))
    | p -> streight p

  member this.DoDerive pref (subst:AugmentedSubst) infon =
    if this.options.Trace > 0 then
      System.Console.WriteLine ("{0}: [ derive: {1} under {2}", this.me.Value.name, infon, substToString subst.subst)
    
    let sum f lst =
      List.fold (fun acc e -> f e @ acc) [] lst
    
    let res = 
      match infon with
        | InfonAnd (i1, i2) ->
          (this.DoDerive pref subst i1) |> sum (fun s1 -> this.DoDerive pref s1 i2)
        | InfonEmpty _ -> [subst]
        | InfonSaid (p, i) ->
          this.DoDerive (Pref.Said p :: pref) subst i
        | InfonImplied (p, i) ->
          this.DoDerive (Pref.Implied p :: pref) subst i
        | Infon.Var v when subst.subst.ContainsKey v.id ->
          this.DoDerive pref subst subst.subst.[v.id]
        | AsInfon e ->
          if pref <> [] then
            failwith "asInfon(...) under said/implied prefix"
          [{ subst with assumptions = e :: subst.assumptions }]
        | InfonCert (inf, ev) when pref = [] ->
          let unifyEv (subst:AugmentedSubst, pr) =
            match unifyTerms subst.subst (ev, pr) with
              | Some s -> [{ subst with subst = s }]
              | None -> 
                if this.options.Trace > 0 then
                  System.Console.WriteLine ("the requested evidence: {0} doesn't match the actual {1}", ev, pr)                 
                []
          this.DeriveCertification subst inf |> List.collect unifyEv
        | templ ->
          let rec checkOne = function
            | (subst, precond :: rest) ->
              this.DoDerive [] subst precond |> sum (fun s -> checkOne (s, rest))
            | (subst, []) -> [subst]
          this.InfonsWithPrefix subst.subst pref templ |> List.map (fun (s, p) -> ({subst with subst = s }, p)) |> sum checkOne

    if this.options.Trace > 0 then
      System.Console.WriteLine ("result:{0} ] {1}", res.Length, l2s res)

    res

  member private this.Send msg =
    if this.sentItems.ContainsKey msg then
      ()
    else
      this.sentItems.Add (msg, true)
      this.Comm.SendMessage msg
      
  member this.Derive s (infon:Term) =
    let vars = (infon.Apply s).Vars()
    let checkAssumptions (augS:AugmentedSubst) =
      let apply (t:Term) = t.Apply augS.subst
      let sqlExpr = SqlCompiler.compile this.options this.NextId (augS.assumptions |> List.map apply)
      SqlCompiler.execQuery (this.sql.Value, this.Comm, this.options, sqlExpr, augS.subst, vars) |> Seq.toList      
    { substs = this.DoDerive [] (AugmentedSubst.NoAssumptions s) infon |> List.collect checkAssumptions }
  
  member private this.DoListen (msg:Message) =  
    let src = Term.Const (Const.Principal msg.source)
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
        let infons =
          match t with
            | InfonCert (_, e) ->
              match msg.proviso.Apply s with
                | InfonEmpty ->
                  let acc = vec()
                  match this.EvidenceCheck acc e with
                    | Some _ ->
                      [acc.[acc.Count - 1]; acc.[acc.Count - 2]]
                      //Seq.toList acc
                    | _ ->
                      this.Comm.Warning ("fake certified infon")
                      []
                | _ ->
                  this.Comm.Warning ("certified infon with proviso")
                  []
            | _ ->
              match msg.proviso.Apply s with
                | InfonEmpty ->
                  [Infon.Said (src, t)]
                | proviso ->
                  [Infon.Follows (proviso, Infon.Implied (src, t))]
        List.map (fun infon -> { ai = this.FakeAI(); infon = infon }) infons
          
      match subst with
        | Some s ->
          (this.Derive s filter.trigger).All |> List.collect apply
        | None -> []
    let newInfons = this.filters |> List.collect matches
    List.iter this.Comm.Knows newInfons
    this.infonstrate <- newInfons @ this.infonstrate
    
  member this.Me = this.me
  
  member private this.DoTalk () =
    let runCommFor (comm:Communication) (subst:Subst) =
      match comm.target.Apply subst with
        | Term.Const (Const.Principal p) ->
          let msg = ({ source = this.me.Value
                       target = p
                       message = comm.message.Apply subst
                       proviso = comm.proviso.Apply subst
                     } : Message).Canonical()
          this.Send msg
        | t ->
          System.Console.WriteLine ("attempting broadcast message: " + t.ToString())
          
    this.communications |> List.iter (fun comm -> (this.Derive Map.empty comm.trigger).All |> List.iter (runCommFor comm))
  
  /// First close the Engine, and then start a new substrate connection and a worker thread(s).
  member this.Reset () =
    this.Close()
    this.sql <- Some (SqlConnector this.options.PrivateSql)
    let t = System.Threading.Thread this.Work
    this.worker <- Some t
    t.Start()
  
  /// Add an infon to the infostrate.
  member this.AddInfon i =
    this.Invoke (fun () ->
       this.infonstrate <- { ai = this.FakeAI(); infon = i } :: this.infonstrate)
  
  /// Add a policy assertion to the infonstrate.
  member this.AddAssertion (a:Assertion) = 
    this.Invoke (fun () ->
      this.me <- Some a.AssertionInfo.principal
      let a = a 
              |> this.PullOutFunctions
              |> this.HandleCertifications 
      match a with
        | Knows k ->
          this.infonstrate <- k :: this.infonstrate
        | SendTo c ->
          this.communications <- c :: this.communications
        | ReceiveFrom f ->
          this.filters <- f :: this.filters      
      )
  

  /// Add the default, permissive filter to the infonstrate.
  member this.AddDefaultFilter () =
    this.Invoke (fun () ->
      let src = this.FreshVar Type.Principal
      let msg = this.FreshVar Type.Infon
      let proviso = this.FreshVar Type.Infon
      let filter =
        {
          ai = this.FakeAI()
          source = Term.Var src
          message = Infon.Var msg
          proviso = Infon.Var proviso
          trigger = Infon.Empty
        }
      this.filters <- filter :: this.filters)
  
  /// Add incomming message to the infostrate. Will call comm.Knows() for each
  /// new infons learned. Will call Talk() at the end.
  member this.Listen (comm, msg:Message) =
    this.Invoke (fun () ->
      this.comm <- Some comm
      this.DoListen msg
      this.DoTalk ()
      this.Comm.RequestFinished ())

  /// See if some messages should be sent, and if so, sends them using comm.Send().
  member this.Talk (comm) =
    this.Invoke (fun () ->
      this.comm <- Some comm
      this.DoTalk ()
      this.Comm.RequestFinished ())

  /// Given an infon, possibly with free variables, return all the possible values
  /// for these variables using comm.QueryResults(i, results). 
  member this.Ask (comm, i:Infon) =
    this.Invoke (fun () ->
      this.comm <- Some comm
      let bind (s:Subst) =
        i.Vars() |> Seq.map (fun v -> { formal = v; actual = s.[v.id].Apply s })
      this.Comm.QueryResults (i, (this.Derive Map.empty i).All |> Seq.map bind)
      this.Comm.RequestFinished ())
  
  /// Wait for all pending requests to finish, stop the worker thread and clean the state.
  member this.Finish () =
    this.Invoke (fun () ->
      this.finish <- true)
    match this.worker with
      | Some w ->
        w.Join()
        this.worker <- None
      | None -> ()
    this.Close()

  /// Block until all pending requests are finished.
  member this.CheckPoint () =
    let o = ref false
    this.Invoke (fun () ->
      lock o (fun () ->
        o := true
        System.Threading.Monitor.Pulse o))
    lock o (fun () ->
      if !o then ()
      else System.Threading.Monitor.Wait o |> ignore)

  member private this.Invoke a = 
    if this.worker.IsNone then failwith "not yet started"
    let wrapped () =
      try a()
      with e ->
        this.Comm.ExceptionHandler e
    lock this.pending (fun () ->
      this.pending.Enqueue wrapped
      System.Threading.Monitor.Pulse this.pending)
  
  member private this.Work () =
    while not this.finish do
      let act = 
        lock this.pending (fun () ->
          while this.pending.Count = 0 do
            System.Threading.Monitor.Wait this.pending |> ignore
          this.pending.Dequeue())
      act()

  /// Kill all the worker threads. Close the substrate SQL connection. Clear infostrate and the current policy.
  member this.Close () =
    match this.worker with
      | Some w ->
        w.Abort()
        w.Join()
        this.worker <- None
      | None -> ()          
    this.sql |> Option.iter (fun s -> s.Close())
    this.sql <- None
    this.me <- None
    this.pending.Clear()
    this.sentItems.Clear()
    this.comm <- None
    this.infonstrate <- []
    this.filters <- []
    this.communications <- []
    this.nextId <- 0


  //
  // Evidential things
  // 
  
  // return true iff signature is the signature of principal under infon
  member private this.SignatureCheck principal infon signature =
    // TODO
    true
  
  member private this.MakeSignature (infon:Infon) =
    // TODO
    Term.Const (Const.Int 42)
    
  member private this.FinalOutcome = function
    | InfonFollows (_, i) -> this.FinalOutcome i
    | i -> i
  
  member private this.IsMe = function
    | Term.Const (Const.Principal p) -> p = this.me.Value
    | _ -> false
  
  member private this.CanSign principal infon =
    match this.FinalOutcome infon with
      | InfonSaid (p, _)
      | InfonImplied (p, _) -> principal = p
      | _ -> false
  
  member private this.EvidenceCheck (acc:Vec<_>) (ev:Term) =
    let ret inf =
      acc.Add inf
      acc.Add (Infon.Cert (inf, ev))
      Some inf
      
    match ev with
      | App (f, [p; inf; sign]) when f === Function.EvSignature ->
        if this.SignatureCheck p inf sign && this.CanSign p inf then
          ret inf
        else
          this.Comm.Warning ("spoofed signature")
          None
      | App (f, [a; b]) when f === Function.EvMp ->
        match this.EvidenceCheck acc a, this.EvidenceCheck acc b with
          | Some i1, Some (InfonFollows (i1', i2)) when i1 = i1' ->
            ret i2
          | _ ->
            this.Comm.Warning ("malformed mp")
            None
      | App (f, [a; b]) when f === Function.EvAnd ->
        match this.EvidenceCheck acc a, this.EvidenceCheck acc b with
          | Some i1, Some i2 ->
            ret (Infon.And (i1, i2))
          | _ ->
            this.Comm.Warning ("malformed and")
            None
      | App (f, [AsInfon (_) as t]) when f === Function.EvAsInfon ->
        // TODO check it
        this.Comm.Warning ("asInfon evidence: " + t.ToString())
        ret t
      | _ ->
        this.Comm.Warning ("unhandled evidence constructor")
        None                        

  member private this.HandleCertifications = function
    | Assertion.SendTo ({ certified = Certified|CertifiedSay } as comm) ->
      let comm =
        match comm.proviso with
          | InfonEmpty ->
            let msg = 
              if comm.certified = CertifiedSay then
                Infon.Said (Infon.Const (Const.Principal this.Me.Value), comm.message)
              else comm.message            
            match this.FinalOutcome msg with
               | InfonSaid (p, _)
               | InfonImplied (p, _) when this.IsMe p ->
                 { comm with message = Infon.Cert (msg, App (Function.EvSignature, [p; msg; this.MakeSignature msg])) }
               | _ ->
                 let v = this.FreshVar Type.Evidence
                 let msg = Infon.Cert (msg, Term.Var v)
                 { comm 
                   with message = msg
                        trigger = Infon.And (msg, comm.trigger) }
          | _ ->
            this.Comm.Warning ("certified provisional communication not supported at this time")
            comm
      Assertion.SendTo { comm with certified = Processed }
    | t -> t

  member private this.PullOutFunctions asrt =
    let pulledOut = dict()
    let premises = vec()
    let rec aux = function
      | AsInfon _ as t -> t
      | Term.App (f, args) as t ->
        if f.IsFree then
          Term.App (f, List.map aux args)
        else
          match pulledOut.TryGetValue t with
            | true, v -> v
            | false, _ ->
              let v = Term.Var (this.FreshVar t.Type)
              pulledOut.Add (t, v)
              premises.Add (Term.App (Function.AsInfon, [Term.App (Function.Eq, [v; t])]))
              v
      | t -> t
    let addPremises t =
      Seq.fold (fun acc p -> Infon.Follows (p, acc)) t premises
    let addSides t =
      Seq.fold (fun acc p -> Infon.And (p, acc)) t premises

    match asrt with
      | Assertion.Knows k ->
        Assertion.Knows { k with infon = aux k.infon |> addPremises }
      | Assertion.SendTo k ->
        let k = { k with trigger = aux k.trigger 
                         target = aux k.target
                         message = aux k.message
                         proviso = aux k.proviso }
        Assertion.SendTo { k with trigger = k.trigger |> addSides }
      | a -> a

  member this.Certify = function
    | InfonSaid (p, _)
    | InfonImplied (p, _) as msg ->
      Infon.Cert (msg, App (Function.EvSignature, [p; msg; this.MakeSignature msg]))
    | _ -> failwith "invalid input"
        