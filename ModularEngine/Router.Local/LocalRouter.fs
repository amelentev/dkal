namespace Microsoft.Research.Dkal.Router.Local

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Router

/// The LocalRouter provides a IRouter interface for several principals that
/// run in different threads in the same physical computer. Communication is
/// performed in memory by means of AST (with no serialization)
type LocalRouter (routingTable: IRoutingTable, mailer: LocalMailer) =
  
  do
    mailer.SetPrincipalInbox routingTable.Me (fun _ -> ())

  interface IRouter with
    member sr.Me = routingTable.Me
    
    member sr.Roster = routingTable.Principals

    member sr.Receive newElevateMessageFunction =
      mailer.SetPrincipalInbox routingTable.Me newElevateMessageFunction
    
    member sr.Send infon ppal = 
      match ppal with
      | PrincipalConstant(target) -> 
        sr.DoSend infon target
      | Var(v) -> 
        for ppalName in (sr :> IRouter).Roster do
          let s = Substitution.Id.Extend (v :> IVar, PrincipalConstant(ppalName))
          sr.DoSend (infon.Apply(s)) ppalName
      | _ -> failwithf "Expecting principal constant or variable as destination when sending message, found %O" ppal

    member sr.Start () = 
      ()

    member sr.Stop () =
      ()

  member private sr.DoSend infon ppalName = 
    printfn ">>>>>>\r\n>>>>>> SENT TO %O: %O\r\n>>>>>>" ppalName infon
    mailer.SendMessage infon ppalName
        
  member sr.AddMailerCallback (targetAmountOfMessages: int) (f: unit -> unit) =
    mailer.AddCallback targetAmountOfMessages f
