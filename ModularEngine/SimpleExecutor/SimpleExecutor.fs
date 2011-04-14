namespace Microsoft.Research.Dkal.SimpleExecutor

open System.Collections.Generic
open System.Threading

open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Interfaces

type SimpleExecutor(router: IRouter, engine: IEngine) = 
  
  let inbox = new Queue<MetaTerm * MetaTerm>()
  let quarantine = new HashSet<MetaTerm>()
  let rules = new List<MetaTerm>()
  
  let notEmpty = new AutoResetEvent(false)

  let mutable worker: option<Thread> = None
  let mutable finish: bool = false

  interface IExecutor with
    member se.InstallPolicy (p: Policy) =
      for rule in p.Rules do
        rules.Add(rule) |> ignore

    member se.Start () = 
      // Start engine
      engine.Start()

      // Start worker thread
      finish <- false
      let t = new Thread(se.Work)
      worker <- Some t
      t.Start()

      // Start communications
      router.Receive(fun msg from -> 
                      lock inbox (fun () ->
                        inbox.Enqueue((msg, from))
                        notEmpty.Set() |> ignore))
      router.Start()

    member se.Stop () =
      // Stop Communications
      router.Stop()
      
      // Stop worker thread
      finish <- true
      match worker with 
      | Some worker -> 
        lock inbox (fun () -> notEmpty.Set() |> ignore)
        worker.Join()
      | None -> ()
      
      // Stop engine
      engine.Stop()


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
                    // Move incoming messages to quarantine
                    quarantine.Add (App(primitives.["saidInfon"], [from; msg])) |> ignore)

  member private se.ExecuteRound() =
    // To store the changes to be applied at the end of the round
    let actions = new HashSet<MetaTerm>()

    // Calculate a list from the quarantine infons
    let quarantineList = Seq.toList quarantine

    // Traverse rules
    for rule in rules do
      match rule with
      | Rule(cs, cw, a) -> 
        if engine.Derive cs [] && engine.Derive cw quarantineList then
          actions.Add(a) |> ignore
      | _ -> failwith <| "Expecting rule when executing round"

    // Check consistency and apply changes
    let actions = Seq.toList actions
    let changed = if engine.AreConsistentActions actions then
                    se.ApplyActions actions
                  else
                    failwith <| "Found inconsistent set of actions"

    // Prune messages from quarantine
    // TODO

    // Return true if some change was computed
    changed
    
  member private se.ApplyActions (actions: MetaTerm list) = 
    let mutable changed = false
    for action in actions do
      match action with
      | Seq(a1, a2) -> 
        changed <- se.ApplyActions [a1; a2]
      | Learn(infon) -> 
        changed <- engine.Learn infon
      | Send(ppal, infon) -> 
        router.Send infon ppal
      | _ -> failwith <| "Unrecognized action"
    changed
