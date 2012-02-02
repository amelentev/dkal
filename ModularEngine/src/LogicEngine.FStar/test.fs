module Test

exception Error of string

let sendMsg (p:string) (msg:string) =
  let bytes = System.Text.Encoding.Unicode.GetBytes msg in
  Net.send p bytes

let getMsg() =
  let bytes = Net.receive() in
  let msg = System.Text.Encoding.Unicode.GetString(bytes) in
  System.Console.WriteLine(msg)

let _ = 
  let args = System.Environment.GetCommandLineArgs() |> Seq.toList in
  match args with
    | [_;configFile] ->
      if not (System.IO.File.Exists (configFile)) then
        (System.Console.WriteLine("File not found: {0}", configFile); exit(1))
      else Authenticate.readConfig configFile
    | _ -> (System.Console.WriteLine("usage: test config-file-name"); exit(1)) in
  let p = Authenticate.lookup_me() in
  if (p = "org1") then sendMsg "site1" "sending from org1 to site1"
  else if (p = "site1") then
    (getMsg(); sendMsg "phys1" "sending from site1 to phys1")
  else if (p = "phys1") then
    (getMsg(); sendMsg "keyMgr" "sending from phys1 to keyMgr"; getMsg())
  else if (p = "keyMgr") then
    (getMsg(); sendMsg "keyMgr" "sending keys from keyMgr to phys1")
    
