namespace Microsoft.Research.GeneralPDP.Utils

open System.Windows.Forms
open System.Drawing

module GUI =
  
  let pickFromOptions (title: string) (description: string) (options: 'a seq) =
    let okClicked = ref false
    let pickForm = new Form()
    pickForm.Text <- title
    pickForm.FormBorderStyle <- FormBorderStyle.Fixed3D
    pickForm.Size <- Size(360, 120)
    pickForm.Padding <- new Padding(14)
    pickForm.SuspendLayout()
    let label = new System.Windows.Forms.Label()
    label.Text <- description
    label.Dock <- DockStyle.Top
    pickForm.Controls.Add(label)
    let p1 = new Panel()
    let combo = new ComboBox()
    Seq.iter (fun o -> combo.Items.Add(o) |> ignore) options
    combo.DropDownStyle <- ComboBoxStyle.DropDownList
    combo.Width <- 200
    combo.SelectedIndex <- 0
    combo.Dock <- DockStyle.Left
    p1.Controls.Add(combo)
    let okButton = new Button()
    okButton.Text <- "OK"
    okButton.Dock <- DockStyle.Right
    okButton.Click.Add(fun _ -> 
        okClicked.Value <- true
        pickForm.Close())
    combo.KeyDown.Add(fun args -> if args.KeyCode = Keys.Enter then okButton.PerformClick() else ())
    p1.Height <- 25
    p1.Controls.Add(okButton)
    p1.Dock <- DockStyle.Bottom
    pickForm.Controls.Add(p1)
    pickForm.ResumeLayout()
    pickForm.ShowDialog() |> ignore
    if okClicked.Value then
      match combo.Items.[combo.SelectedIndex] with
      | :? 'a as a -> Some a
      | _ -> failwith "Selected option does not have expected type"
    else
      None

  let textInput (title: string) (description: string) =     
    let okClicked = ref false
    let inputForm = new Form()
    inputForm.Text <- title
    inputForm.FormBorderStyle <- FormBorderStyle.Fixed3D
    inputForm.Size <- Size(360, 120)
    inputForm.Padding <- new Padding(14)
    inputForm.SuspendLayout()
    let label = new System.Windows.Forms.Label()
    label.Text <- description
    label.Dock <- DockStyle.Top
    inputForm.Controls.Add(label)
    let p1 = new Panel()
    let text = new TextBox()
    text.Width <- 200
    text.Dock <- DockStyle.Left
    p1.Controls.Add(text)
    let okButton = new Button()
    okButton.Text <- "OK"
    okButton.Dock <- DockStyle.Right
    okButton.Click.Add(fun _ -> 
        okClicked.Value <- true
        inputForm.Close())
    text.KeyDown.Add(fun args -> if args.KeyCode = Keys.Enter then okButton.PerformClick() else ())
    p1.Height <- 25
    p1.Controls.Add(okButton)
    p1.Dock <- DockStyle.Bottom
    inputForm.Controls.Add(p1)
    inputForm.ResumeLayout()
    inputForm.ShowDialog() |> ignore
    if okClicked.Value then
      Some text.Text
    else
      None