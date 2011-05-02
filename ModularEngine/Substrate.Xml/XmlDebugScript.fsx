#I @".\bin\debug\"
#r "Ast.dll"
#r "Ast.Infon.dll"
#r "Ast.Tree.dll"
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
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Ast.Infon
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

let output (vars: seq<IVar>) (res: seq<ISubstitution>) =
  res |> Seq.map (fun subst ->
    vars |> Seq.map (fun var -> 
      var.Name+"="+(subst.Apply var).ToString()) |> 
    String.concat " "
  ) |> printfn "%A"

let var name typ = {Name=name; Type=typ} :> IVar

let outputvar1 = {Name="TRIAL"; Type=Type.String;} :> IVar
let inputvar1 = {Name="ORG"; Type=Type.Principal;}
let query1 = new XmlSubstrateTerm("xct", "/root/trials/*[@org='$ORG']", [inputvar1], dict ["", outputvar1])

let subst1 = Substitution.Id.Extend(inputvar1, SubstrateConstant "org1")
xmlsubstr.Solve [query1] [subst1] |> output [outputvar1]


let inputvar2 = outputvar1
let outputvars2: IDictionary<string, IVar> = dict ["", var "SITE" Type.Principal; "n1", var "N1" Type.Int32; "n2", var "N2" Type.Int32]
let query2 = new XmlSubstrateTerm("xct", "/root/trials/$TRIAL/*[@unnotified='true']", [inputvar2], outputvars2)
let subst2 = Substitution.Id.Extend(inputvar2, SubstrateConstant "trial1")
xmlsubstr.Solve [query2] [subst2] |> output outputvars2.Values

xmlsubstr.Solve [query1; query2] [subst1] |> output (Seq.append [outputvar1] outputvars2.Values)

let outputvar3 = dict ["", var "PHYS" Type.Principal]
let query3 = new XmlSubstrateTerm("xct", "/root/trials/trial1/site1/*[1100<=@P1 and @P2<=1249]", [], outputvar3)

xmlsubstr.Solve [query3] [Substitution.Id] |> output outputvar3.Values