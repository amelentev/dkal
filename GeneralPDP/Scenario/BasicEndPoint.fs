namespace Microsoft.Research.GeneralPDP.Scenario

open Basics
open Message
open EndPoint
open EndPointImageFactory

open Microsoft.Msagl.Drawing

open System.Drawing

module BasicEndPoint = 

  type BasicEndPoint(id: EndPointId) =
    inherit EndPoint(id)

    override ep.Process(m: IMessage) = ()

    override ep.StartUp() = ()

    override ep.CleanUp() = ()

    override ep.ApplyStyle (n: Node) = 
      n.LabelText <- ep.Description

    override ep.Image = Some (image (ep :> IEndPoint).Color EmptyDrawing)

    override ep.Description = ep.Id
