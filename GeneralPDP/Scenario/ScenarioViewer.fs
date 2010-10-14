namespace Microsoft.Research.GeneralPDP.Scenario

open Basics
open Message
open BasicEndPoint
open EnforcementEndPoint
open PolicyRepositoryEndPoint
open XacmlEndPoint
open DkalEndPoint
open XacmlToDkalEndPoint
open DkalToXacmlEndPoint
open Microsoft.Research.DkalEngine.Ast
open Microsoft.Research.DkalEngine.Util
open Microsoft.Research.GeneralPDP.Utils.GUI
open Microsoft.Research.GeneralPDP.XACML.Parsing
open Microsoft.Research.GeneralPDP.DKAL.Engine.ParsingCtxFactory

open Microsoft.Msagl.Drawing
open Microsoft.Msagl.GraphViewerGdi

open System
open System.Drawing
open System.Threading
open System.IO
open System.Collections.Generic
open System.Windows.Forms

module ScenarioViewer =

  type VoidDelegate = delegate of unit -> unit

  type ScenarioViewer() = 

    // GUI elements
    let form: Form = new Form()
    let graphViewer: GViewer = new GViewer() 
    let rstButton: Button = new Button()
    let bckButton: Button = new Button()
    let fwdButton: Button = new Button()
    let nowButton: Button = new Button()
    let stepLabel: System.Windows.Forms.Label = new System.Windows.Forms.Label()    
    let statusLabel: ToolStripLabel = new ToolStripLabel()
    let msgHeaderLabel: Label = new Label()    
    let descrLabel: TextBox = new TextBox()    
    let contextMenu = new ContextMenu()

    // font for graph labels
    let graphFont = descrLabel.Font

    // graph and geometry graph
    let mutable graph: Graph = null
    let mutable geomGraph: Microsoft.Msagl.GeometryGraph = null

    // reference to scenario being displayed
    let mutable scenario: IScenario option = None

    // reference to object under mouse
    let mutable underMouse: IViewerObject = null

    // application thread that holds the window while active
    let mutable appThread: Thread option = None

    // edges in order to keep track of graph state over time
    let edges: ResizeArray<Edge> = ResizeArray<_>()
    let mutable highlightedEdge: Edge option = None
    let mutable currIndex = -1

    // nodes to keep track of extra attributes (such as label)
    let nodes: Dictionary<EndPointId, IEndPoint> = new Dictionary<_,_>()

    // Draw images in nodes
    (*let getNodeBoundary (image: Image) (node: Microsoft.Msagl.Drawing.Node) =
      let textSize = TextRenderer.MeasureText(node.LabelText, graphFont)
      let width = (float) (Math.Max(image.Width, textSize.Width + 25))
      let height = (float) (image.Height + textSize.Height)
      Microsoft.Msagl.Splines.CurveFactory.CreateBox(width, height, new Microsoft.Msagl.Point())*)
    
    let addSegmentToPath (seg: Microsoft.Msagl.Splines.ICurve) (p: System.Drawing.Drawing2D.GraphicsPath ref) =
      let pointF (p: Microsoft.Msagl.Point) =
        new PointF((float32) p.X, (float32) p.Y)
      let radiansToDegrees = (180.0 / Math.PI)
      if seg <> null then
        match seg with
        | :? Microsoft.Msagl.Splines.LineSegment as line -> 
          p.Value.AddLine(pointF(line.Start), pointF(line.End))
        | :? Microsoft.Msagl.Splines.CubicBezierSegment as cb ->
          p.Value.AddBezier(pointF(cb.B(0)), pointF(cb.B(1)), pointF(cb.B(2)), pointF(cb.B(3)))
        | :? Microsoft.Msagl.Splines.Ellipse as ellipse ->
          p.Value.AddArc((float32)(ellipse.Center.X - ellipse.AxisA.Length), (float32)(ellipse.Center.Y - ellipse.AxisB.Length),
              (float32)(2.0 * ellipse.AxisA.Length), (float32)(2.0 * ellipse.AxisB.Length), (float32)(ellipse.ParStart * radiansToDegrees),
              (float32)((ellipse.ParEnd - ellipse.ParStart) * radiansToDegrees))
        | _ -> ()

    let fillTheGraphicsPath (iCurve: Microsoft.Msagl.Splines.ICurve) =
      match iCurve with
      | :? Microsoft.Msagl.Splines.Curve as curve ->
        let path = new System.Drawing.Drawing2D.GraphicsPath()
        for seg in curve.Segments do
          addSegmentToPath seg (ref path)
        path
      | _ -> failwith "Expecting curve"
    
    let drawNode (image: Image) (node: Microsoft.Msagl.Drawing.Node) (graphics: obj) =
      match graphics with
      | :? Graphics as g -> 
          // flip the image around its center
          use m = g.Transform
          use saveM = m.Clone()
          use m2 = new System.Drawing.Drawing2D.Matrix(1.0F, 0.0F, 0.0F, -1.0F, 0.0F, 2.0F * (float32) node.Attr.GeometryNode.Center.Y)
          m.Multiply(m2)
          g.Transform <- m
          let path: Drawing2D.GraphicsPath = fillTheGraphicsPath(node.Attr.GeometryNode.BoundaryCurve)
          g.SetClip(path)
          let slackWidth = node.Attr.GeometryNode.Width - (float) image.Width
          let topLeft = new PointF((float32) (slackWidth / (float) 2.0 + node.Attr.GeometryNode.Center.X - node.Attr.GeometryNode.Width / (float) 2.0), 
                                (float32) (node.Attr.GeometryNode.Center.Y - node.Attr.GeometryNode.Height / (float) 2.0))
          g.DrawImage(image, topLeft)
          let stringSize = g.MeasureString(node.LabelText, statusLabel.Font)
          let stringSlackWidth = node.Attr.GeometryNode.Width - (float) stringSize.Width
          let labelTopLeft = new PointF((float32) (stringSlackWidth / (float) 2.0 + node.Attr.GeometryNode.Center.X - node.Attr.GeometryNode.Width / (float) 2.0), 
                                (float32) (node.Attr.GeometryNode.Center.Y + node.Attr.GeometryNode.Height / (float) 2.0) - 16.0F)
          let labelRect = new RectangleF(labelTopLeft, new SizeF((float32) node.Attr.GeometryNode.Width, (float32) node.Attr.GeometryNode.Height))
          g.DrawString(node.LabelText, statusLabel.Font, new SolidBrush(Color.Black), labelRect)
          g.Transform <- saveM
          true // returning false would enable the default rendering
      | _ -> failwith "Expecting graphics"

    let applyNodeAttributes(ep: IEndPoint, graphNode: Node) =
        match graphNode with
        | null -> ()
        | graphNode -> 
              ep.ApplyStyle graphNode
              match ep.Image with
              | None -> ()
              | Some img -> 
                  graphNode.Attr.Shape <- Shape.DrawFromGeometry
                  graphNode.DrawNodeDelegate <- new DelegateToOverrideNodeRendering(drawNode img)
                  //graphNode.NodeBoundaryDelegate <- new DelegateToSetNodeBoundary(getNodeBoundary img)

    let reDraw() = 
      // save previouse zoom value in order to restore it
      let oldZoom = graphViewer.ZoomF

      // recreate geometry elements
      geomGraph <- new Microsoft.Msagl.GeometryGraph()

      let height = Resources.Main.ep_1.Height
      let width = Resources.Main.ep_1.Width
      geomGraph.LayerSeparation <- (float) (height / 3)
      geomGraph.NodeSeparation <-  (float) (width / 3)

      // first nodes
      for node in graph.NodeMap.Values do
        match node with
        | :? Node as node -> 
              let ep = nodes.[node.Id]
              match ep.Image with
              | Some image -> 
                let textSize = TextRenderer.MeasureText(node.LabelText, graphFont)
                let width = (float) (Math.Max(image.Width, textSize.Width + 25))
                let height = (float) (image.Height + textSize.Height)
                let geomNode = new Microsoft.Msagl.Node(node.Id, Microsoft.Msagl.Splines.CurveFactory.CreateBox(width, height, new Microsoft.Msagl.Point()))
                geomGraph.AddNode(geomNode)
              | None -> failwith "expecting image on every node"
        | _ -> failwith "expecting drawing node"
      
      // then edges (and their labels)
      let mutable geomEdges = []
      for edge in edges do 
        let geomEdge = new Microsoft.Msagl.Edge(geomGraph.FindNode(edge.Source), geomGraph.FindNode(edge.Target))
        geomEdge.ArrowheadLength <- (float) width / 10.0
        geomEdge.Label <- edge.Label.GeometryLabel
        let font = new Font(edge.Label.FontName, (float32) edge.Label.FontSize)
        let w, h = ref 0.0, ref 0.0
        StringMeasure.MeasureWithFont(edge.Label.Text, font, w, h)
        geomEdge.Label.Width <- w.Value
        geomEdge.Label.Height <- h.Value
        geomEdges <- geomEdges @ [geomEdge]
        geomGraph.AddEdge(geomEdge)

      // then calculate layout
      geomGraph.CalculateLayout()

      // bind geometry elements to drawing elements
      for drawingNode in graph.NodeMap.Values do
        match drawingNode with
        | :? Node as drawingNode -> drawingNode.Attr.GeometryNode <- geomGraph.FindNode(drawingNode.Id)
        | _ -> failwith "expecting drawing node"
      for index in [0 .. edges.Count-1] do
        edges.[index].Attr.GeometryEdge <- geomEdges.[index]
      graph.GeometryGraph <- geomGraph

      graphViewer.NeedToCalculateLayout <- false
      graphViewer.Graph <- graph
      graphViewer.ZoomF <- oldZoom
      
    let updateControls() = 
        if currIndex >= 0 then
          rstButton.Enabled <- true
          bckButton.Enabled <- true
        else
          rstButton.Enabled <- false
          bckButton.Enabled <- false
        if currIndex + 1 < edges.Count then
          nowButton.Enabled <- true
          fwdButton.Enabled <- true
        else
          nowButton.Enabled <- false
          fwdButton.Enabled <- false
        stepLabel.Text <- "Step " + (currIndex + 1).ToString() + " of " + edges.Count.ToString()

    let updateStatus() = 
      let rec printStatusFrom (obj: IViewerObject) = 
         match obj with
         | :? DLabel as l -> printStatusFrom l.Parent 
         | :? DEdge as d -> 
              let e = d.Edge
              if edges.Contains e && edges.IndexOf e <= currIndex then
                statusLabel.Text <- e.Source + " --> " + e.Target + ": " + e.LabelText
         | :? DNode as n ->
              statusLabel.Text <- n.Node.LabelText
         | null
         | _ -> 
            statusLabel.Text <- ""
      printStatusFrom underMouse

    let updateMessageLabels (i: int) (m: obj) =
      match m with
      | :? IMessage as m -> 
            descrLabel.Text <- m.Content.ToString()
            msgHeaderLabel.Text <- "Message " + (i+1).ToString() + ": " + m.Sender + " ---> " + m.Receiver
      | _ -> ()
    
    let updateDescription() = 
      let rec printDescrFrom (obj: IViewerObject) = 
         match obj with
         | :? DLabel as l -> printDescrFrom l.Parent 
         | :? DEdge as d -> 
              let e = d.Edge
              if edges.Contains e && edges.IndexOf e <= currIndex then
                updateMessageLabels (edges.IndexOf e) e.UserData
         | null -> ()
         | _ -> ()
      printDescrFrom underMouse
  
    let pickReceiver () = 
      pickFromOptions "Sending message..." "Select the destination:" (Seq.map (fun (n: KeyValuePair<_,_>) -> n.Key) (nodes))

    (*let handleLoadEndPoint () = 
      try 
        let id = textInput "Loading a new endpoint..." "Choose an ID for the new endpoint:"
        match id with 
        | Some id -> 
            let kind = pickFromOptions "Loading a new endpoint..." "Select the endpoint kind:" ["Basic"; "Enforcement"; "PolicyRepository"; "XACML"; "DKAL"; "XACML->DKAL"; "DKAL->XACML" ] 
            let ep: IEndPoint option = match kind with
                                       | Some "Basic" -> Some (BasicEndPoint id :> IEndPoint)
                                       | Some "Enforcement" -> Some (EnforcementEndPoint id :> IEndPoint)
                                       | Some "PolicyRepository" -> Some (PolicyRepositoryEndPoint(id, [], []) :> IEndPoint)
                                       | Some "XACML" -> 
                                            match pickFromOptions "Loading a new endpoint..." "Select the attribute repository:" 
                                                              (["<None>"] @ (List.map (fun (n: KeyValuePair<_,_>) -> n.Key) (nodes |> Seq.toList))) with
                                            | None -> None
                                            | Some "<None>" -> Some (XacmlEndPoint id :> IEndPoint)
                                            | Some attRepId -> Some (XacmlEndPoint(id, attRepId) :> IEndPoint)
                                       | Some "DKAL" -> 
                                          let openFileDialog = new OpenFileDialog()
                                          openFileDialog.AutoUpgradeEnabled <- false
                                          openFileDialog.InitialDirectory <- "C:\\Users\\t-guide\\Desktop\\fse\\main\\DKAL\\GeneralPDP\\Samples" // TODO: change for Application.StartupPath
                                          openFileDialog.Filter <- "DKAL policy (*.dkal)|*.dkal|" +
                                                                   "All files (*.*)|*.*" 
                                          openFileDialog.FilterIndex <- 0
                                          if openFileDialog.ShowDialog() = DialogResult.OK then
                                            let pctx, assertions = xacmlAwareParsingCtx(id)
                                            let assertions = assertions @ pctx.ParseFile(openFileDialog.FileName)
                                            Some (DkalEndPoint(id, assertions) :> IEndPoint)
                                          else
                                            None
                                       | Some "XACML->DKAL" -> 
                                          let dkalId = pickFromOptions "Loading a new endpoint..." "Select the DKAL backend:" (Seq.map (fun (n: KeyValuePair<_,_>) -> n.Key) (nodes))
                                          match dkalId with
                                          | Some dkalId -> 
                                                let pctx, _ = xacmlAwareParsingCtx id
                                                Some (XacmlToDkalEndPoint(id, dkalId, pctx) :> IEndPoint)
                                          | None -> None
                                       | Some "DKAL->XACML" -> 
                                          let xacmlId = pickFromOptions "Loading a new endpoint..." "Select the XACML backend:" (Seq.map (fun (n: KeyValuePair<_,_>) -> n.Key) (nodes))
                                          match xacmlId with
                                          | Some xacmlId -> 
                                              let dkalId = pickFromOptions "Loading a new endpoint..." "Select the DKAL backend:" (Seq.map (fun (n: KeyValuePair<_,_>) -> n.Key) (nodes))
                                              match dkalId with
                                              | Some dkalId -> 
                                                  let openFileDialog = new OpenFileDialog()
                                                  openFileDialog.AutoUpgradeEnabled <- false
                                                  openFileDialog.InitialDirectory <- "C:\\Users\\t-guide\\Desktop\\fse\\main\\DKAL\\GeneralPDP\\Samples" // TODO: change for Application.StartupPath
                                                  openFileDialog.Filter <- "DKAL policy (*.dkal)|*.dkal|" +
                                                                           "All files (*.*)|*.*" 
                                                  openFileDialog.FilterIndex <- 0
                                                  if openFileDialog.ShowDialog() = DialogResult.OK then
                                                    let pctx, assertions = xacmlAwareParsingCtx(id)
                                                    let assertions = assertions @ pctx.ParseFile(openFileDialog.FileName)
                                                    Some (DkalToXacmlEndPoint(id, xacmlId, dkalId, pctx, assertions) :> IEndPoint)
                                                  else
                                                    None
                                              | None -> None
                                          | None -> None
                                       | _ -> None
            match ep with
            | Some ep -> 
                scenario.Value.AddEndPoint(ep)
                ep.Start()
            | None -> ()
        | None -> ()
      with
      | SyntaxError(p,s) -> printfn "%O: %O" p s
      | e -> printfn "Error while reading file: %O" e*)

    let handleSendNewMessage (ep: IEndPoint) =
      let openFileDialog = new OpenFileDialog()
      openFileDialog.AutoUpgradeEnabled <- false
      openFileDialog.InitialDirectory <- "C:\\Users\\t-guide\\Desktop\\fse\\main\\DKAL\\GeneralPDP\\Samples" // TODO: change for Application.StartupPath
      openFileDialog.Filter <- "XACML request (*.req)|*.req|" +
                               "XACML policy (*.pcy)|*.pcy|" +
                               "XACML response (*.rsp)|*.rsp|" +
                               "XACML policy request (*.prq)|*.prq|" +
                               "DKAL infon (*.infon)|*.infon|" +
                               "All files (*.*)|*.*" 
      openFileDialog.FilterIndex <- 0
      if openFileDialog.ShowDialog() = DialogResult.OK then
        try
          let ext = Path.GetExtension(openFileDialog.FileName)
          let content = match ext with
                        | ".req" -> 
                          let text = File.ReadAllText(openFileDialog.FileName)
                          let req = parseRequest(text)
                          Some (XacmlRequestContent req)
                        | ".pcy" ->
                          let text = File.ReadAllText(openFileDialog.FileName)
                          let pcy = parsePolicy(text)
                          Some (XacmlPolicyContent pcy)
                        | ".rsp" ->
                          let text = File.ReadAllText(openFileDialog.FileName)
                          let rsp = parseResponse(text)
                          Some (XacmlResponseContent rsp)
                        | ".prq" ->
                          let text = File.ReadAllText(openFileDialog.FileName)
                          let prq = parsePolicyRequest(text)
                          Some (XacmlPolicyRequestContent prq)
                        | ".infon" ->
                          let text = File.ReadAllText(openFileDialog.FileName)
                          let pctx, _ = xacmlAwareParsingCtx(ep.Id)
                          let infon = pctx.ParseInfon text
                          Some (InfonContent infon)
                        | _ -> MessageBox.Show("File extension not recognized: " + ext) |> ignore
                               None
          match content with
          | None -> ()
          | Some c -> 
              match pickReceiver() with
              | None -> ()
              | Some r -> 
                  // move simulation to most current state
                  if nowButton.Enabled then
                    nowButton.PerformClick()
                  // do message sending
                  ep.Send({sender= ep.Id;
                          receiver= r;
                          content= c})
        with 
        | SyntaxError(p,s) -> printfn "%O: %O" p s
        | e -> printfn "Error while reading file: %O" e

    let makeInvisible (e: Edge) = 
      e.Attr.Color <- graph.Attr.BackgroundColor
      e.Label.FontColor <- graph.Attr.BackgroundColor

    let makeVisible (e: Edge) = 
      e.Attr.Color <- Microsoft.Msagl.Drawing.Color.Black
      e.Label.FontColor <- Microsoft.Msagl.Drawing.Color.Black

    let highlightEdge (edge: Edge option) =
      match highlightedEdge with
      | None -> ()
      | Some e -> 
          if edges.IndexOf e <= currIndex then 
            makeVisible e
          else
            makeInvisible e
      match edge with
      | None -> ()
      | Some e -> 
          e.Label.FontColor <- Microsoft.Msagl.Drawing.Color.Red
          e.Attr.Color <- Microsoft.Msagl.Drawing.Color.Red
      highlightedEdge <- edge
      reDraw()
   
    let doReset _ =
      for i in [0..currIndex] do
        makeInvisible (edges.[i])
      currIndex <- -1
      highlightEdge None
      descrLabel.Text <- ""
      msgHeaderLabel.Text <- ""
      statusLabel.Text <- ""
      updateControls()

    let doBack _ = 
      makeInvisible (edges.[currIndex]) 
      currIndex <- currIndex - 1
      if currIndex >= 0 then
        let edge = edges.[currIndex]
        highlightEdge (Some edge)
        updateMessageLabels currIndex edge.UserData
        statusLabel.Text <- edge.Source + " --> " + edge.Target + ": " + edge.LabelText
      else
        highlightEdge None
        descrLabel.Text <- ""
        msgHeaderLabel.Text <- ""
        statusLabel.Text <- ""
      updateControls()
          
    let doForward _ = 
      currIndex <- currIndex + 1
      let edge = edges.[currIndex]
      makeVisible edge
      highlightEdge (Some edge)
      updateMessageLabels currIndex edge.UserData
      statusLabel.Text <- edge.Source + " --> " + edge.Target + ": " + edge.LabelText
      updateControls()

    let doNow _ =
      for i in [currIndex + 1 .. edges.Count - 1] do
        makeVisible edges.[i]
      currIndex <- edges.Count - 1
      let edge = edges.[currIndex]
      highlightEdge (Some edge)
      updateMessageLabels currIndex edge.UserData
      statusLabel.Text <- edge.Source + " --> " + edge.Target + ": " + edge.LabelText
      updateControls()

    let handleJumpToThisMessage (edge: Edge) =
      let index = edges.IndexOf edge
      for i in [index + 1 .. edges.Count - 1] do
        makeVisible edges.[i]
      currIndex <- index
      highlightEdge (Some edge)
      updateMessageLabels currIndex edge.UserData
      statusLabel.Text <- edge.Source + " --> " + edge.Target + ": " + edge.LabelText
      updateControls()

    let handleGraphClick (args: EventArgs) =
      contextMenu.MenuItems.Clear()
      match args with
      | :? MouseEventArgs as args -> 
            let rec findElement (obj: IViewerObject) = 
              match obj with
              | :? DNode as n -> 
                    let ep = nodes.[n.DrawingNode.Id]
                    let headerMI = new MenuItem(ep.Description)
                    headerMI.Enabled <- false
                    contextMenu.MenuItems.Add(headerMI) |> ignore
                    contextMenu.MenuItems.Add("-") |> ignore
                    let initMessageMI = new MenuItem("Send new message...", new EventHandler(fun _ _ -> handleSendNewMessage ep))
                    contextMenu.MenuItems.Add(initMessageMI) |> ignore
                    // endpoint specific
                    match ep with 
                    | :? DkalEndPoint as dEp -> 
                        let showCommRulesMI = new MenuItem("Show installed communication rules...", new EventHandler(fun _ _ -> 
                          let assertionsText = String.concat "\n" (List.map (fun (a: Assertion) -> a.ToSX().ToString()) (dEp.CommRules()))
                          MessageBox.Show(assertionsText, ep.Id + " communication rules") |> ignore
                          ))
                        contextMenu.MenuItems.Add(showCommRulesMI) |> ignore
                        let showKnowsMI = new MenuItem("Show principal infostrate...", new EventHandler(fun _ _ -> 
                          let assertionsText = String.concat "\n" (List.map (fun (a: Assertion) -> a.ToSX().ToString()) (dEp.Knows()))
                          MessageBox.Show(assertionsText, ep.Id + " infostrate") |> ignore
                          ))
                        contextMenu.MenuItems.Add(showKnowsMI) |> ignore
                    | _ -> ()
              | :? DLabel as l -> findElement l.Parent
              | :? DEdge as e -> 
                    let headerMI = new MenuItem(e.Edge.LabelText)
                    headerMI.Enabled <- false
                    contextMenu.MenuItems.Add(headerMI) |> ignore
                    contextMenu.MenuItems.Add("-") |> ignore
                    let jumpToThisMessageMI = new MenuItem("Jump simulation to this message", new EventHandler(fun _ _ -> handleJumpToThisMessage e.Edge))
                    contextMenu.MenuItems.Add(jumpToThisMessageMI) |> ignore
              | _ -> ()
            findElement underMouse
      | _ -> failwith "Expecting mouse event args in click handler"

    do         
      // set the graph viewer
      graphViewer.LayoutEditingEnabled <- false
      graphViewer.ToolBarIsVisible <- false
      graphViewer.ContextMenu <- contextMenu
      graphViewer.Graph <- graph

      // graph viewer events
      graphViewer.MouseDown.Add(handleGraphClick)
      graphViewer.MouseCaptureChanged.Add(fun _ -> underMouse <- null)
      graphViewer.SelectionChanged.Add(
        fun args -> 
          underMouse <- graphViewer.ObjectUnderMouseCursor
          updateDescription()
          updateStatus())

      // form events
      form.KeyPreview <- true 
      form.KeyDown.Add(fun args -> if args.KeyCode = Keys.Right && fwdButton.Enabled then 
                                     fwdButton.PerformClick()
                                   elif args.KeyCode = Keys.Left && bckButton.Enabled then 
                                     bckButton.PerformClick())

      // set toolbar
      let toolStripContainer = new ToolStripContainer()
      (*let toolStripToolbar = new ToolStrip()
      toolStripToolbar.Items.Add(new ToolStripMenuItem("Load a new endpoint...", 
                                                       Resources.Main.img_open,
                                                       new EventHandler(fun _ _ -> handleLoadEndPoint()))) |> ignore*)
      let toolStripStatus = new StatusStrip()
      toolStripStatus.Items.Add(statusLabel) |> ignore
      //toolStripContainer.TopToolStripPanel.Controls.Add(toolStripToolbar)
      toolStripContainer.BottomToolStripPanel.Controls.Add(toolStripStatus)

      // set the form
      form.WindowState <- FormWindowState.Maximized
      form.Text <- "Scenario viewer"
      form.Icon <- Resources.Main.icon_general_pdp
      
      // left panel with message label
      let pLeft = new Panel()
      descrLabel.Dock <- DockStyle.Fill
      pLeft.Controls.Add(descrLabel)
      msgHeaderLabel.Font <- new Font(msgHeaderLabel.Font, FontStyle.Bold)
      msgHeaderLabel.Dock <- DockStyle.Top
      pLeft.Controls.Add(msgHeaderLabel)

      // right panel with graph viewer and buttons
      let pRight = new Panel()
      graphViewer.Dock <- DockStyle.Fill
      pRight.Controls.Add(graphViewer)
        
      // tooltips
      let tt = new ToolTip()
      tt.SetToolTip(nowButton, "Move simulation to most current state")
      tt.SetToolTip(fwdButton, "Perform one simulation step")
      tt.SetToolTip(bckButton, "Go back one simulation step")
      tt.SetToolTip(rstButton, "Reset simulation to its initial state")

      // set back/forward/etc.. buttons
      nowButton.Image <- Resources.Main.img_last
      nowButton.Dock <- DockStyle.Left
      nowButton.Click.Add(doNow)
      fwdButton.Image <- Resources.Main.img_next
      fwdButton.Dock <- DockStyle.Left
      fwdButton.Click.Add(doForward)
      bckButton.Image <- Resources.Main.img_prev
      bckButton.Dock <- DockStyle.Left
      bckButton.Click.Add(doBack)
      rstButton.Image <- Resources.Main.img_first
      rstButton.Dock <- DockStyle.Left
      rstButton.Click.Add(doReset)
      stepLabel.Dock <- DockStyle.Fill
      stepLabel.TextAlign <- ContentAlignment.MiddleCenter
      updateControls()

      // group back and forward buttons
      let pButtons = new Panel()
      pButtons.Controls.AddRange([| stepLabel; nowButton; fwdButton; bckButton; rstButton|])
      pButtons.Height <- 40
      pButtons.Dock <- DockStyle.Bottom
      pRight.Controls.Add(pButtons)

      // set description label
      descrLabel.ReadOnly <- true
      descrLabel.Multiline <- true
      descrLabel.ScrollBars <- ScrollBars.Vertical

      // add elements to main panel
      let pMain = new Panel()
      let split = new SplitContainer()
      pLeft.Dock <- DockStyle.Fill
      split.Panel1.Controls.Add(pLeft)
      pRight.Dock <- DockStyle.Fill
      split.Panel2.Controls.Add(pRight)
      split.Dock <- DockStyle.Fill
      pMain.Controls.Add(split)

      // add elements to form
      form.SuspendLayout()
      pMain.Dock <- DockStyle.Fill
      toolStripContainer.ContentPanel.Controls.Add(pMain)
      toolStripContainer.Dock <- DockStyle.Fill
      form.Controls.Add(toolStripContainer)
      form.ResumeLayout()
      split.SplitterDistance <- 50

    member this.Display() = 
      let init = fun () -> 
        try
          Application.Run(form)
        with
        | e -> printfn "%O" e
      appThread <- Some (Thread init)
      appThread.Value.Start()

    member this.WaitForWindow() = 
      match appThread with
      | None -> ()
      | Some t -> t.Join()
                  appThread <- None

    member private this.Reset() = 
      graph <- new Graph("scenario")
      geomGraph <- new Microsoft.Msagl.GeometryGraph()
      graph.Attr.BackgroundColor <- new Microsoft.Msagl.Drawing.Color(byte(211), byte(211), byte(211))
      graphViewer.Graph <- graph
      underMouse <- null
      edges.Clear()
      nodes.Clear()
      highlightedEdge <- None
      currIndex <- -1
      updateControls()
      descrLabel.Text <- ""
      statusLabel.Text <- ""
      reDraw()

    

    interface IScenarioViewer with

      member this.AssignScenario (s: IScenario) =
        this.Reset()
        scenario <- Some s
        form.Text <- "Scenario viewer - " + s.Name

      member this.NotifyNewEndPoint (ep: IEndPoint) = 
        nodes.[ep.Id] <- ep
        let n = graph.AddNode(ep.Id)
        applyNodeAttributes(ep, n)
        reDraw()

      member this.NotifyNewMessage (m: IMessage) =
        form.Invoke(new VoidDelegate(fun () ->
          let messageNumber = edges.Count + 1
          let label = "(" + messageNumber.ToString() + ") " + m.Content.Type()
          let edge = graph.AddEdge(m.Sender, label, m.Receiver)
          edge.UserData <- m
          edge.Label.FontSize <- 6
          edges.Add(edge)
          currIndex <- currIndex + 1
          updateControls()
          highlightEdge (Some edge))) |> ignore


