namespace Microsoft.Research.GeneralPDP.Scenario

open Microsoft.Research.GeneralPDP.XACML.Ast
open Microsoft.Research.GeneralPDP.XACML.PDP.Engine
open Basics
open Message
open EndPoint
open EndPointImageFactory

open Microsoft.Msagl.Drawing

open System.Drawing
open System.Collections.Generic

module AttributeRepositoryEndPoint =

  type AttributeRepositoryEndPoint(id: EndPointId) = 
    inherit EndPoint(id)

    // just an example of something that could be done here 
    // other options may be: LDAP, SQL queries, etc...
    let company = new Dictionary<string, string>()
    let nationality = new Dictionary<string * string, string>()
    do
      company.Add("paul", "starbucks")
      nationality.Add(("john", "curtis"), "us")
      nationality.Add(("paula", "packard"), "us")
      nationality.Add(("matt", "packard"), "us")
      nationality.Add(("alain", "spad"), "french")

    override ep.Process(m: IMessage) = 
      match m.Content with
      | XacmlAttributeRequestContent(arc) -> 
          // get essential data from request
          let mutable subjectId = None
          let mutable companyId = None
          for att in arc.Request.Attributes do
            if att.AttributeId = "subject-id" && att.Category = SubjectCategory then 
              match att.Value with
              | StringAtomValue s -> 
                      subjectId <- Some s
              | _ -> ()
            if att.AttributeId = "company-id" && att.Category = SubjectCategory then 
              match att.Value with
              | StringAtomValue s -> 
                      companyId <- Some s
              | _ -> ()

          // try to honor request
          let mutable reply = None
          match arc.Designator with
          | AttributeDesignatorExp(cat, dt, aid, i, mbp) ->
            if cat = SubjectCategory && dt = StringDatatype && aid = "company-id" then
              match subjectId with
              | Some subjectId -> 
                  if company.ContainsKey(subjectId) then
                    reply <- Some (Attribute(cat, aid, StringAtomValue(company.[subjectId])))
              | _ -> ()
            if cat = SubjectCategory && dt = StringDatatype && aid = "nationality" then
              match subjectId, companyId with
              | Some subjectId, Some companyId -> 
                  if nationality.ContainsKey(subjectId, companyId) then
                    reply <- Some (Attribute(cat, aid, StringAtomValue(nationality.[(subjectId, companyId)])))
              | _ -> ()
            
            // reply
            ep.Send({sender= id;
                      receiver= m.Sender;
                      content= XacmlAttributeResponseContent(AttributeResponseContext(reply, arc.Request))})
              
          | _ -> failwith "expecting attribute designator"
      | _ -> "I don't understand content " + m.Content.ToString() |> ep.Fail

    override ep.StartUp() = ()

    override ep.CleanUp() = ()

    override ep.Image = Some (image (ep :> IEndPoint).Color ContactsDrawing)

    override ep.ApplyStyle (n: Node) = 
      n.LabelText <- ep.Description

    override ep.Description = ep.Id + ": AttRepository"
