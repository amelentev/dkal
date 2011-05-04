namespace Microsoft.Research.Dkal.Executor.Simple

open System.Collections.Generic
open System.Threading

open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Substrate

/// The SimpleExecutor runs an endless loop. On each iteration, every rule is 
/// processed to see if it has to be applied. All necessary changes are saved
/// until the end of the iteration. If the set of changes is consistent, they
/// all get applied and a new iteration starts.
type SimpleExecutor(router: IRouter, engine: ILogicEngine) = 
  
  /// The inbox holds the messages that arrive from the router but still
  /// haven't been moved to Quarantine
  let inbox = new Queue<ITerm>()

  /// The quarantine holds all incoming messages of relevance. We need to
  /// call quarantine.Prune() in order to remove old/unnecessary messages
  let quarantine = new Quarantine()

  /// The rules set contains all the current rules that came from installed
  /// policies or were added as a consequence of install actions in other rules
  let rules = new HashSet<ITerm>()
  
  /// Used by the worker thread to wait for messages when there is no processing
  /// to be performed.
  let notEmpty = new AutoResetEvent(false)

  /// The worker thread executes an endless loop of rounds. See se.ExecuteRound
  let mutable worker: option<Thread> = None

  /// Flag that's set to true when the executor needs to stop so that the worker
  /// thread will read it and terminate
  let mutable finish: bool = false

  interface IExecutor with
    
    member se.InstallRule (r: ITerm) =
      match r with
      | SeqRule(rs) -> List.fold (fun change rule -> (se :> IExecutor).InstallRule rule || change) false rs
      | _ -> rules.Add(r)

    member se.UninstallRule (r: ITerm) =
      match r with
      | SeqRule(rs) -> List.fold (fun change rule -> (se :> IExecutor).UninstallRule rule || change) false rs
      | _ -> rules.Remove(r)

    member se.Start () = 
      // Start communications
      router.Receive(fun msg -> 
                      lock inbox (fun () ->
                        inbox.Enqueue(msg)
                        notEmpty.Set() |> ignore))
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
    while not finish do
      // Execute a round
      let changing = se.ExecuteRound()

      // If there were no changes wait for at least one new message to arrive
      if not(changing) then
        notEmpty.WaitOne() |> ignore

      // Move messages (if any) to quarantine
      lock inbox (fun () -> 
                  while inbox.Count > 0 do
                    let msg = inbox.Dequeue()
                    printfn "<<<<<<\r\n<<<<<< GOT %O\r\n<<<<<<" msg
                    quarantine.Add msg)

  member private se.ExecuteRound() =
    printfn "------------------------------------------------------------------------"
    System.Console.ReadLine() |> ignore
    
    // To store the changes to be applied at the end of the round
    let actions = new HashSet<ITerm>()

    let rec traverse rule = 
      match rule with
      | Rule(condition, action) ->
          for subst in se.SolveCondition condition [Substitution.Id] do
            actions.Add (action.Apply subst) |> ignore
      | EmptyRule -> ()
      | SeqRule (rules) ->
        List.iter traverse rules
      | _ -> failwith <| "Expecting rule when executing round"

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
    let mutable changed = false
    let substrateUpdates = new HashSet<ISubstrateUpdateTerm>()
    for action in actions do
      let changes = 
        match action with
        | SeqAction(actions) -> se.ApplyActions actions
        | Learn(infon) -> engine.Learn infon
        | Forget(infon) -> engine.Forget infon
        | Send(ppal, infon) -> router.Send infon ppal; false
        | Say(ppal, infon) -> router.Send (SaidInfon(PrincipalConstant(router.Me), infon)) ppal; false
        | Install(rule) -> (se :> IExecutor).InstallRule rule
        | Uninstall(rule) -> (se :> IExecutor).UninstallRule rule
        | Apply(su) -> 
          substrateUpdates.Add su |> ignore; false
        | _ -> failwithf "Unrecognized action %O" action
      changed <- changed || changes        
    let changedFromSubstrateUpdates = SubstrateDispatcher.Update substrateUpdates
    changed || changedFromSubstrateUpdates

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
