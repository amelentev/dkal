namespace Microsoft.Research.GeneralPDP.Scenario

open Basics
open Message
open EndPoint
open Microsoft.Research.GeneralPDP.DKAL.Engine.Interface
open Microsoft.Research.GeneralPDP.DKAL.Engine.Basics
open Microsoft.Research.DkalEngine.Ast
open EndPointImageFactory

open Microsoft.Msagl.Drawing

open System.Drawing
open System.Collections.Generic

module DkalEndPoint =

  type DkalEndPoint(id: EndPointId, ?dkalPolicy: DkalPolicy) = 
    inherit EndPoint(id)

    let trustCommRulesFrom = new HashSet<EndPointId>()

    // options to pass to dkal interface
    let mutable dispatcher = None
    let mutable learning = None

    let mutable dkalEngineInterface: DkalEngineInterface option = None

    member ep.AddTrustRulesFrom (from: EndPointId) = 
      trustCommRulesFrom.Add(from) |> ignore

    override ep.Process(m: IMessage) = 
      match m.Content with
      | InfonContent(infon) -> 
                  let sender = dkalEngineInterface.Value.PrincipalByName m.Sender
                  let receiver = dkalEngineInterface.Value.Me
                  dkalEngineInterface.Value.ReceiveMessage ({source= sender;
                                                             target= receiver;
                                                             message= infon;
                                                             proviso= Term.Empty})
      | DkalPolicyContent(dp) -> 
          if trustCommRulesFrom.Contains(m.Sender) then
            match dkalEngineInterface with
            | Some dkalEngineInterface -> 
                dkalEngineInterface.ClearRules()
                for a in dp.Assertions do
                  dkalEngineInterface.AddRule a
            | None -> failwith "DKAL engine interface has not been setup yet" 
          else
            failwith ("I don't trust communication rules from: " + m.Sender)
      | _ -> "I don't understand content " + m.Content.ToString() |> ep.Fail

    override ep.StartUp() =
      ep.SetUpEngine(dkalPolicy)

    override ep.CleanUp() = 
      ep.StopEngine()

    override ep.Image = Some (image (ep :> IEndPoint).Color EmptyDrawing)

    override ep.ApplyStyle (n: Node) = 
      n.LabelText <- ep.Description

    member ep.SetDispatcher (d: string) = 
      dispatcher <- Some d

    member ep.SetLearning (l: string) = 
      learning <- Some l

    member private ep.SetUpEngine(dkalPolicy: DkalPolicy option) =
      let deliverDkalInfonMessage = 
        fun (im: Message) -> ep.Send({sender = id;
                                      receiver = im.target.name;
                                      content = InfonContent(im.message)})
      ep.StopEngine()
      dkalEngineInterface <- Some(DkalEngineInterface(id, deliverDkalInfonMessage))
      
      // set engine options
      match dispatcher with
      | None -> ()
      | Some d -> dkalEngineInterface.Value.SetDispatcher d
      match learning with
      | None -> ()
      | Some l -> dkalEngineInterface.Value.SetLearning l

      // add assertions and launch
      match dkalPolicy with
      | Some pcy -> 
          List.iter dkalEngineInterface.Value.AddRule pcy.Assertions
      | None -> ()
      dkalEngineInterface.Value.Talk() 

    member private ep.StopEngine() = 
      match dkalEngineInterface with
      | None -> ()
      | Some dkalEngineInterface -> 
          dkalEngineInterface.CheckPoint()
          dkalEngineInterface.Finish()

    override ep.Description = ep.Id + ": DKAL"

    member ep.CommRules () = 
      match dkalEngineInterface with
      | None -> []
      | Some dei -> dei.CommRules()

    member ep.Knows () = 
      match dkalEngineInterface with
      | None -> []
      | Some dei -> dei.Knows()