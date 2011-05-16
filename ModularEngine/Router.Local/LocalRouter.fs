namespace Microsoft.Research.Dkal.Router.Local

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast.Infon.Syntax.Factories
open Microsoft.Research.Dkal.Router

open NLog

/// The LocalRouter provides a IRouter interface for several principals that
/// run in different threads in the same physical computer. Communication is
/// performed in memory by means of AST (with no serialization)
type LocalRouter (routingTable: IRoutingTable, mailer: LocalMailer) =
  let log = LogManager.GetLogger("Router.Local")
  
  let printer = PrettyPrinterFactory.InfonPrinter "simple"
  do
    mailer.SetPrincipalInbox routingTable.Me (fun _ _ -> ())

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
    log.Info(">> From {0} to {1}:\r\n{2}\r\n", (sr:>IRouter).Me, ppalName, printer.PrintTerm infon)
    mailer.SendMessage infon (Principal((sr:>IRouter).Me)) ppalName

  member sr.AddMailerCallback (targetAmountOfMessages: int) (f: unit -> unit) =
    mailer.AddCallback targetAmountOfMessages f
