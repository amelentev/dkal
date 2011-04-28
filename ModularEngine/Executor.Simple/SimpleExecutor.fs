namespace Microsoft.Research.Dkal.Executor.Simple

open System.Collections.Generic
open System.Threading

open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Interfaces

/// The SimpleExecutor runs an endless loop. On each iteration, every rule is 
/// processed to see if it has to be applied. All necessary changes are saved
/// until the end of the iteration. If the set of changes is consistent, they
/// all get applied and a new iteration starts.
type SimpleExecutor(router: IRouter, engine: IEngine) = 
  
  /// The inbox holds the messages that arrive from the router but still
  /// haven't been moved to Quarantine
  let inbox = new Queue<ITerm * ITerm>()

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
      rules.Add(r) |> ignore

    member se.Start () = 
      // Start communications
      router.Receive(fun msg from -> 
                      lock inbox (fun () ->
                        inbox.Enqueue((msg, from))
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
                    let msg, from = inbox.Dequeue()
                    printfn "GOT FROM %O:\r\n %O" from msg
                    quarantine.Add msg from)

  member private se.ExecuteRound() =
    printfn "------------------------------------------------------------------------"
    System.Console.ReadLine() |> ignore
    
    // To store the changes to be applied at the end of the round
    let actions = new HashSet<ITerm>()

    // Traverse rules
    for rule in rules do
      match rule with
      | Rule(cs, cw, a) -> 
        for subs in quarantine.Matches cw do
          for subs' in engine.Derive <| cs.Apply subs do
            actions.Add((a.Apply subs).Apply subs') |> ignore
      | _ -> failwith <| "Expecting rule when executing round"

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
    
  /// Given a set list of action MetaTerms, each of the actions is applied.
  /// Returns true iff at least one of the actions produced a change.
  member private se.ApplyActions (actions: ITerm list) = 
    let mutable changed = false
    for action in actions do
      let changes = 
        match action with
        | Seq(a1, a2) -> se.ApplyActions [a1; a2]
        | Learn(infon) -> engine.Learn infon
        | Forget(infon) -> engine.Forget infon
        | Send(ppal, infon) -> router.Send infon ppal; false
        | Install(rule) -> rules.Add(rule)
        | Uninstall(rule) -> rules.Remove(rule)
        | _ -> failwith <| "Unrecognized action"
      changed <- changed || changes        
    changed
