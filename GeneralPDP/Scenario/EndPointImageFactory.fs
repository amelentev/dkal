namespace Microsoft.Research.GeneralPDP.Scenario

open Resources

open System.Drawing

module EndPointImageFactory = 
  
  type EPColor = StdColor | GrayColor | GreenColor | PurpleColor | BlueColor | OrangeColor
    with 
      static member Parse (s: string) = 
        match s.ToLower() with
        | "std" -> StdColor
        | "grey"
        | "gray" -> GrayColor
        | "green" -> GreenColor
        | "purple" -> PurpleColor
        | "blue" -> BlueColor
        | "orange" -> OrangeColor
        | _ -> failwith ("Unrecognized endpoint color: " + s)

  type EPDrawing = EmptyDrawing | FirewallDrawing | DatabaseDrawing | ContactsDrawing | SharePointDrawing

  let image (c: EPColor) (d: EPDrawing) = 
    match d with
    | EmptyDrawing ->
        match c with
        | StdColor -> Resources.Main.ep_1 :> Image
        | GrayColor -> Resources.Main.ep_1gr :> Image
        | GreenColor -> Resources.Main.ep_1g :> Image
        | PurpleColor -> Resources.Main.ep_1p :> Image
        | BlueColor -> Resources.Main.ep_1b :> Image
        | OrangeColor -> Resources.Main.ep_1o :> Image
    | FirewallDrawing ->
        match c with
        | StdColor -> Resources.Main.ep_5 :> Image
        | GrayColor -> Resources.Main.ep_5gr :> Image
        | GreenColor -> Resources.Main.ep_5g :> Image
        | PurpleColor -> Resources.Main.ep_5p :> Image
        | BlueColor -> Resources.Main.ep_5b :> Image
        | OrangeColor -> Resources.Main.ep_5o :> Image
    | DatabaseDrawing ->
        match c with
        | StdColor -> Resources.Main.ep_7 :> Image
        | GrayColor -> Resources.Main.ep_7gr :> Image
        | GreenColor -> Resources.Main.ep_7g :> Image
        | PurpleColor -> Resources.Main.ep_7p :> Image
        | BlueColor -> Resources.Main.ep_7b :> Image
        | OrangeColor -> Resources.Main.ep_7o :> Image
    | ContactsDrawing ->
        match c with
        | StdColor -> Resources.Main.ep_9 :> Image
        | GrayColor -> Resources.Main.ep_9gr :> Image
        | GreenColor -> Resources.Main.ep_9g :> Image
        | PurpleColor -> Resources.Main.ep_9p :> Image
        | BlueColor -> Resources.Main.ep_9b :> Image
        | OrangeColor -> Resources.Main.ep_9o :> Image
    | SharePointDrawing ->
        match c with
        | StdColor -> Resources.Main.ep_s :> Image
        | GrayColor -> Resources.Main.ep_sgr :> Image
        | GreenColor -> Resources.Main.ep_sg :> Image
        | PurpleColor -> Resources.Main.ep_sp :> Image
        | BlueColor -> Resources.Main.ep_sb :> Image
        | OrangeColor -> Resources.Main.ep_so :> Image
