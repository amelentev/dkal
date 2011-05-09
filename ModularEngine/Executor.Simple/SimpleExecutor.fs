﻿namespace Microsoft.Research.Dkal.Executor.Simple

open System.Collections.Generic
open System.Threading
open NLog

open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Substrate

/// The SimpleExecutor runs an endless loop. On each iteration, every rule is 
/// processed to see if it has to be applied. All necessary changes are saved
/// until the end of the iteration. If the set of changes is consistent, they
/// all get applied and a new iteration starts.
type SimpleExecutor(router: IRouter, engine: ILogicEngine, stepbystep: bool) = 
  let log = LogManager.GetLogger("Executor.Simple")
  
  /// The inbox holds the messages that arrive from the router but still
  /// haven't been moved to Quarantine
  let inbox = new Queue<ITerm * ITerm>()

  /// The quarantine holds all incoming messages of relevance. We need to
  /// call quarantine.Prune() in order to remove old/unnecessary messages
  let quarantine = new Quarantine()

  /// The rules set contains all the current rules that came from installed
  /// policies or were added as a consequence of install actions in other rules
  let rules = new HashSet<ITerm>()
  
  /// Keeps the messages that were sent until a fixed-point is reached, no 
  /// message is sent twice in the same epoch (from fixed-point a to fixed-point
  /// b)
  let sentMessages = new HashSet<ITerm * ITerm>()

  /// Used by the worker thread to wait for messages when there is no processing
  /// to be performed.
  let notEmpty = new AutoResetEvent(false)

  /// The worker thread executes an endless loop of rounds. See se.ExecuteRound
  let mutable worker: option<Thread> = None

  /// Flag that's set to true when the executor needs to stop so that the worker
  /// thread will read it and terminate
  let mutable finish: bool = false

  /// Callback to invoke whenever we reach a fixed-point
  let mutable fixedPointCb: unit -> unit = fun _ -> ()

  /// Callback to invoke whenever we wake up after a fixed-point
  let mutable wakeUpCb: unit -> unit = fun _ -> ()


  do
    router.Receive(fun msg from -> 
                      lock inbox (fun () ->
                      inbox.Enqueue((msg, from))
                      notEmpty.Set() |> ignore))

  interface IExecutor with
    
    member se.InstallRule (r: ITerm) =
      match r with
      | SeqRule(rs) -> List.fold (fun change rule -> (se :> IExecutor).InstallRule rule || change) false rs
      | _ -> rules.Add(r)

    member se.UninstallRule (r: ITerm) =
      match r with
      | SeqRule(rs) -> List.fold (fun change rule -> (se :> IExecutor).UninstallRule rule || change) false rs
      | _ -> rules.Remove(r)

    member se.FixedPointCallback (cb: unit -> unit) = 
      fixedPointCb <- cb
        
    member se.WakeUpCallback (cb: unit -> unit) = 
      wakeUpCb <- cb

    member se.Start () = 
      // Start communications
      router.Start()

      // Start engine
      engine.Start()

      // Start worker thread
      finish <- false
      let t = new Thread(se.Work)
      worker <- Some t
      t.Start()

    member se.Stop () =
      // Stop worker thread
      finish <- true
      match worker with 
      | Some worker -> 
        lock inbox (fun () -> notEmpty.Set() |> ignore)
        worker.Join()
      | None -> ()
      
      // Stop engine
      engine.Stop()

      // Stop Communications
      router.Stop()

  member private se.Work() =
    try
      while not finish do
        // Execute a round
        let changing = se.ExecuteRound()

        // If there were no changes clear the sent message set
        // and wait for at least one new message to arrive
        if not(changing) then
          sentMessages.Clear()
          fixedPointCb()
          notEmpty.WaitOne() |> ignore
          wakeUpCb()

        // Move messages (if any) to quarantine
        lock inbox (fun () -> 
                    while inbox.Count > 0 do
                      let msg, from = inbox.Dequeue()
                      log.Debug("{0}: ---- GOT FROM {1}: {2} ---", router.Me, from, msg)
                      quarantine.Add(msg, from))
    with e -> 
      log.Error("{0}: {1}", router.Me, e.Message)
      fixedPointCb()

  member private se.ExecuteRound() =
    log.Debug("{0}: ------------- round start -------------------", router.Me)
    if stepbystep then
      System.Console.ReadLine() |> ignore
    
    // To store the changes to be applied at the end of the round
    let actions = new HashSet<ITerm>()

    let rec traverse rule = 
      match rule with
      | Rule(condition, action) ->
        for subst in se.SolveCondition condition [Substitution.Id] do
          actions.Add (action.Apply subst) |> ignore
      | RuleOnce(condition, action) ->
        for subst in se.SolveCondition condition [Substitution.Id] do
          actions.Add (action.Apply subst) |> ignore
          actions.Add(UninstallAction(rule)) |> ignore // XXX this is removing the rule several times (performance)
      | EmptyRule -> ()
      | SeqRule (rules) ->
        List.iter traverse rules
      | _ -> failwithf "Expecting rule when executing round, found %O" rule

    // Traverse rules
    for rule in rules do
      traverse rule

    // Check consistency and apply changes
    let actions = Seq.toList actions
    let changed = if ConsistencyChecker.AreConsistentActions actions then
                    se.ApplyActions actions
                  else
                    failwith <| "Found inconsistent set of actions"

    // Prune messages from quarantine
    quarantine.Prune()

    // Return true if some change was computed
    changed
    
  /// Given a set list of action terms, each of the actions is applied.
  /// Returns true iff at least one of the actions produced a change.
  member private se.ApplyActions (actions: ITerm list) = 
    let substrateUpdates = new HashSet<ISubstrateUpdateTerm>()
    let messagesToSend = new HashSet<ITerm * ITerm>()
    let mutable changed = false
    
    let rec allActions a = 
      match a with
      | SeqAction(actions) -> List.collect allActions actions
      | _ -> [a]

    let actions = List.collect allActions actions

    for action in actions do
      let changes = 
        match action with
        | Learn(infon) -> engine.Learn infon
        | Forget(infon) -> engine.Forget infon
        | Send(ppal, infon) -> 
          messagesToSend.Add (infon, ppal) |> ignore; false
        | Say(ppal, infon) -> 
          messagesToSend.Add (SaidInfon(PrincipalConstant(router.Me), infon), ppal) |> ignore; false
        | Install(rule) -> (se :> IExecutor).InstallRule rule
        | Uninstall(rule) -> (se :> IExecutor).UninstallRule rule
        | Drop(i) -> quarantine.Remove i; false
        | Apply(su) -> 
          substrateUpdates.Add su |> ignore; false
        | _ -> failwithf "Unrecognized action %O" action
      changed <- changed || changes        

    let changedFromSubstrateUpdates = SubstrateDispatcher.Update substrateUpdates
    let changedFromMessages = se.Send messagesToSend
    changed || changedFromSubstrateUpdates || changedFromMessages

  /// Given a condition term and a list of substitutions, it returns a subset of
  /// substitutions that satisfy the condition, possibly specialized.
  member private se.SolveCondition (condition: ITerm) (substs: ISubstitution list) =
    match condition with
    | EmptyCondition -> substs
    | WireCondition(i) -> 
      quarantine.Matches(i, substs)
    | KnownCondition(i) ->
      engine.Derive(i, substs)
    | SeqCondition(conds) ->
      List.fold (fun substs cond -> se.SolveCondition cond substs) substs conds
    | _ -> failwithf "Unrecognized condition %O" condition

  /// Sends the given messages by invoking the IRouter implementation, unless the 
  /// messages have already been sent in this epoch. Messages to the same destination
  /// are grouped into one big message
  member private se.Send (messages: HashSet<ITerm * ITerm>) =
    let needSending = Seq.filter (fun m -> not <| sentMessages.Contains m) messages
    let groupedByDestination = Seq.groupBy (fun (infon, ppal) -> ppal) needSending
    for (ppal, msgs) in groupedByDestination do
      let bigMsg = AndInfon([for (msg, ppal) in msgs -> msg]).Normalize()
      router.Send bigMsg ppal
    sentMessages.UnionWith needSending
    (needSending |> Seq.toList).IsEmpty |> not
