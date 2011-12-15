#I @".\bin\debug\"
#r "Ast.dll"
#r "Interfaces.dll"
#r "Substrate.Xml.dll"
#r "FSharp.PowerPack.dll"
#r "System.Xml.Linq.dll"

open System.Xml
open System.Xml.Linq
open System.Xml.XPath
open System.Linq
open System.IO
open System.Collections.Generic
open Microsoft.Research.Dkal.Substrate.Xml
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Substrate

let sdoc = @"
<root>
  <trials>
    <trial1 org='org1'>
      <site1 unnotified='true' n1='1100' n2='1250'>
        <phys1 P1='1150' P2='1199'/>
        <phys2 P1='1200' P2='1250'/>
      </site1>
    </trial1>
    <trial2 org='org2' />
    <trial3 org='org1' >
      <site2 unnotified='false' n1='1300' n2='1450' />
      <site3 unnotified='true' n1='1500' n2='1550' />
    </trial3>
  </trials>
</root>"
let xdoc = XDocument.Load(new StringReader(sdoc))

let xmlsubstr = new XmlSubstrate(xdoc, ["xct"]) :> ISubstrate

let output (vars: seq<ITerm>) (res: seq<ISubstitution>) =
  res |> Seq.map (fun subst ->
    vars |> Seq.map (fun var ->
      match var with
      | :? IVar as var -> var.Name+"="+(subst.Apply var).ToString()
      | _ -> "")
    |> String.concat " "
  ) |> String.concat "; " // |> printfn "%A"

let shouldEqual a b = if a <> b then failwith (sprintf "expected: %A, actual: %A" a b) else "Ok"
let mapVals m = m |> Map.toList |> List.unzip |> snd

let var name typ = {Name=name; Type=typ} :> IVar
let constt c = Const(Constant c)

let trialvar = var "TRIAL" Type.String
let orgvar = var "ORG" Type.Principal
let sitevar = var "SITE" Type.Principal
let n1var = var "N1" Type.Int32
let n2var = var "N2" Type.Int32



/// tests
let query1 = new XmlSubstrateQueryTerm("xct", "/root/trials/*[@org='$ORG']", [orgvar], Map.ofList ["", trialvar :> ITerm])

let subst1 = Substitution.Id.Extend(orgvar, Constant "org1")
xmlsubstr.Solve [query1] [subst1] |> output [trialvar]
    |> shouldEqual "TRIAL=\"trial1\"; TRIAL=\"trial3\""


let outputvars2 = Map.ofList ["", sitevar :> ITerm;  "n1", n1var :> ITerm;  "n2", n2var :> ITerm]
let query2 = new XmlSubstrateQueryTerm("xct", "/root/trials/$TRIAL/*[@unnotified='true']", [trialvar], outputvars2)
let subst2 = Substitution.Id.Extend(trialvar, Constant "trial1")
xmlsubstr.Solve [query2] [subst2] |> output (mapVals outputvars2)
    |> shouldEqual "SITE=site1 N1=1100 N2=1250"

xmlsubstr.Solve [query1; query2] [subst1] |> output (Seq.append [trialvar] (mapVals outputvars2))
    |> shouldEqual "TRIAL=\"trial1\" SITE=site1 N1=1100 N2=1250; TRIAL=\"trial3\" SITE=site3 N1=1500 N2=1550"

let physvar = var "PHYS" Type.Principal
let query3 = new XmlSubstrateQueryTerm("xct", "/root/trials/$TRIAL/$SITE/*[1100<=@P1 and @P2<=1249]", [trialvar; sitevar], Map.ofList ["", physvar :> ITerm])

xmlsubstr.Solve [query1; query2; query3] [subst1] |> output (Seq.append [physvar] (outputvars2 |> Map.toList |> List.unzip |> snd))
    |> shouldEqual "PHYS=phys1 SITE=site1 N1=1100 N2=1250"

// get unnotified sites
let getUnnotified = XmlSubstrateQueryTerm("xct", "/root/trials/*/*[@unnotified='true']", [], Map.ofList ["", sitevar :> ITerm])
xmlsubstr.Solve [getUnnotified] [Substitution.Id] |> output [sitevar] 
    |> shouldEqual "SITE=site1; SITE=site3"

// mark all sites as unnotified
let markUnnotified = XmlSubstrateModifyTerm("xct", "/root/trials/*/*", [], Map.ofSeq ["unnotified", Some(constt "true")]) :> ISubstrateUpdateTerm
xmlsubstr.Update [markUnnotified]

// check all sites is unnotifed
xmlsubstr.Solve [getUnnotified] [Substitution.Id] |> output [sitevar] 
    |> shouldEqual "SITE=site1; SITE=site2; SITE=site3"

// delete test
let deleteSite = XmlSubstrateModifyTerm("xct", "/root/trials/trial3/site2", [], Map.ofSeq ["", None])
xmlsubstr.Update [deleteSite]
xmlsubstr.Solve [XmlSubstrateQueryTerm("xct", "/root/trials/*/*", [], Map.ofList ["", sitevar :> ITerm])] [Substitution.Id] |> output [sitevar]
    |> shouldEqual "SITE=site1; SITE=site3"

// insert test
let insertSite = XmlSubstrateInsertTerm("xct", "/root/trials/trial3", [], Map.ofSeq ["",constt "site2"; "unnotified", constt "false"; "n1", constt 1301; "n2", constt 1451])
xmlsubstr.Update [insertSite]
xmlsubstr.Solve [XmlSubstrateQueryTerm("xct", "//site2[@unnotified='false']", [], Map.ofList ["n1",n1var:>ITerm; "n2",n2var:>ITerm])] [Substitution.Id] |> output [n1var; n2var]
    |> shouldEqual "N1=1301 N2=1451"
