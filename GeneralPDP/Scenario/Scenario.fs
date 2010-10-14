namespace Microsoft.Research.GeneralPDP.Scenario

open Basics
open Message
open EndPoint

open System
open System.Collections.Generic

module Scenario =
  
  type Scenario(scenarioName: string) = 
    let name = scenarioName
    let endPoints = new Dictionary<EndPointId, IEndPoint>()

    let viewers = new HashSet<IScenarioViewer>()
    
    let printingLock = new Object()
 
    interface IScenario with
      member sc.Name = name
  
      member sc.SuscribeViewer(v: IScenarioViewer) = 
        v.AssignScenario(sc)
        viewers.Add(v) |> ignore
        for ep in endPoints do
          v.NotifyNewEndPoint(ep.Value)

      member sc.UnsuscribeViewer(v: IScenarioViewer) = 
        viewers.Remove(v) |> ignore
  
      member sc.AddEndPoint(ep: IEndPoint) = 
        ep.PutInScenario(sc)
        endPoints.Add(ep.Id, ep)
        Seq.iter (fun (v: IScenarioViewer) -> v.NotifyNewEndPoint ep) viewers

      member sc.Route(m: IMessage) =
        if endPoints.ContainsKey(m.Receiver) then
          lock printingLock 
            (fun () -> Seq.iter (fun (v: IScenarioViewer) -> v.NotifyNewMessage m) viewers)
          endPoints.[m.Receiver].Receive(m)
        else
          failwith ("Message target is not in scenario: " + m.Receiver) 

    member sc.Name = 
      (sc :> IScenario).Name
    member sc.SuscribeViewer(v: IScenarioViewer) = 
      (sc :> IScenario).SuscribeViewer(v)
    member sc.UnsuscribeViewer(v: IScenarioViewer) = 
      (sc :> IScenario).UnsuscribeViewer(v)
    member sc.AddEndPoint(ep: IEndPoint) = 
      (sc :> IScenario).AddEndPoint(ep)
    member sc.Route(m: IMessage) = 
      (sc :> IScenario).Route(m)

    member sc.Start() =
      for ep in endPoints do
        ep.Value.Start()

    member sc.CheckPoint() =
      for ep in endPoints do 
        ep.Value.CheckPoint()

    member sc.Finish() =
      for ep in endPoints do 
        ep.Value.Finish()

