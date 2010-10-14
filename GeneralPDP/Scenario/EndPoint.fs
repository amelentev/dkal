namespace Microsoft.Research.GeneralPDP.Scenario

open Basics
open Message
open EndPointImageFactory

open System
open System.Drawing
open System.Threading
open System.Collections.Generic
open Microsoft.Msagl.Drawing

module EndPoint =
 
  [<AbstractClassAttribute>]
  type EndPoint(id: EndPointId) = 

    let mutable color: EPColor = StdColor

    let mutable finish: bool = false
    let mutable worker: option<Thread> = None
    let pending: Queue<unit -> unit> = new Queue<_>()

    let mutable sc: option<IScenario> = None
    let mutable inbox: Queue<IMessage> = new Queue<_>()
    let mutable outbox: Queue<IMessage> = new Queue<_>()

    let doSend () = 
      let m = outbox.Dequeue()
      match sc with
      | None -> failwith "Can't send until endpoint is added to an scenario"
      | Some(sc) -> sc.Route(m)

    let doReceive (proc: IMessage -> unit) = 
      let m = inbox.Dequeue()
      proc m

    abstract StartUp: unit -> unit
    abstract CleanUp: unit -> unit
    abstract Process: IMessage -> unit
    abstract Image: Image option
    abstract ApplyStyle: Node -> unit
    abstract Description: string

    member ep.Worker = worker

    member ep.Scenario = sc

    member ep.Fail(msg: String) = 
      failwith (id + ": " + msg)

    member private ep.Work() =
      while not finish do
        let act = 
          lock pending (fun () ->
            while pending.Count = 0 do
              Monitor.Wait pending |> ignore
            pending.Dequeue())
        act()

    member private ep.Invoke a = 
      if worker.IsNone then failwith (id + " not yet started")
      let wrapped () =
        try a()
        with e ->
          printfn "Exception in EndPoint %s: %O" id e
      lock pending (fun () ->
        pending.Enqueue wrapped
        Monitor.Pulse pending)

    interface IEndPoint with
      member ep.PutInScenario(sc': IScenario) =  
        sc <- Some sc'
  
      member ep.Start() = 
        let t = Thread(ep.Work)
        worker <- Some t
        t.Start()
        ep.StartUp()

      member ep.CheckPoint() =
        let o = ref false
        ep.Invoke (fun () ->
          lock o (fun () ->
            o := true
            Monitor.Pulse o))
        lock o (fun () ->
          if !o then ()
          else Monitor.Wait o |> ignore)

      member ep.Finish () =
        ep.Invoke (fun () ->
          finish <- true)
        match worker with
          | Some w ->
            w.Join()
            worker <- None
          | None -> ()
        ep.CleanUp()

      member ep.Send (m: IMessage) =
        outbox.Enqueue(m)
        ep.Invoke doSend

      member ep.Receive (m: IMessage) =
        inbox.Enqueue(m)
        ep.Invoke (fun _ -> doReceive ep.Process)

      member ep.Process (m: IMessage) = ep.Process(m)

      member ep.Image = ep.Image

      member ep.Color with get() = color and set(c) = color <- c

      member ep.ApplyStyle (n: Node) = ep.ApplyStyle(n)

      member ep.Id = id

      member ep.Description = ep.Description

    member ep.Send (m: IMessage) = (ep :> IEndPoint).Send(m)
    member ep.Receive (m: IMessage) = (ep :> IEndPoint).Receive(m)
    member ep.Id = (ep :> IEndPoint).Id










