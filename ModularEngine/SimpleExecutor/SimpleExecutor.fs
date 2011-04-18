namespace Microsoft.Research.Dkal.SimpleExecutor

open System.Collections.Generic
open System.Threading

open Microsoft.Research.Dkal.Substrate
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Interfaces

type SimpleExecutor(router: IRouter, engine: IEngine) = 
  
  let inbox = new Queue<MetaTerm * MetaTerm>()
  let quarantine = new Quarantine()
  let rules = new List<MetaTerm>()
  
  let notEmpty = new AutoResetEvent(false)

  let mutable worker: option<Thread> = None
  let mutable finish: bool = false

  interface IExecutor with
    member se.InstallPolicy (p: Policy) =
      for rule in p.Rules do
        rules.Add(rule) |> ignore

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
                    printfn "GOT FROM %A:\r\n %A" from msg
                    quarantine.Add msg from)

  member private se.ExecuteRound() =
    System.Console.ReadLine() |> ignore
    
    // To store the changes to be applied at the end of the round
    let actions = new HashSet<MetaTerm>()

    // Traverse rules
    for rule in rules do
      match rule with
      | Rule(cs, cw, a) -> 
        for subs in quarantine.Matches cw do
          for subs', conds in engine.Derive <| subs.Apply cs do
            if List.forall 
              (fun cond -> 
                match subs'.Apply cond with
                | AsInfon(exp, substrateDecl) -> 
                  (SubstrateFactory.Substrate substrateDecl).Solve exp
                | _ -> failwith <| "Unrecognized condition to check") conds then
                  actions.Add(subs'.Apply <| subs.Apply a) |> ignore
      | _ -> failwith <| "Expecting rule when executing round"

    // Check consistency and apply changes
    let actions = Seq.toList actions
    let changed = if engine.AreConsistentActions actions then
                    se.ApplyActions actions
                  else
                    failwith <| "Found inconsistent set of actions"

    // Prune messages from quarantine
    quarantine.Prune()

    printfn "knowledge: %A" <| engine.Knowledge()

    // Return true if some change was computed
    changed
    
  member private se.ApplyActions (actions: MetaTerm list) = 
    let mutable changed = false
    for action in actions do
      match action with
      | Seq(a1, a2) -> 
        let changes = se.ApplyActions [a1; a2]
        changed <- changed || changes
      | Learn(infon) -> 
        let changes = engine.Learn infon
        changed <- changed || changes
      | Send(ppal, infon) -> 
        router.Send infon ppal
      | _ -> failwith <| "Unrecognized action"
    changed
