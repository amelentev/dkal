// *********************************************************
//
//    Copyright (c) Microsoft. All rights reserved.
//    This code is licensed under the Apache License, Version 2.0.
//    THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
//    ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
//    IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
//    PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.
//
// *********************************************************

namespace Microsoft.Research.DkalEngine

open System
open System.IO
open System.Text
open Microsoft.FSharp.Text

open Microsoft.Research.DkalEngine.Ast
open Microsoft.Research.DkalEngine.Util

type SqlCommunicator(ctx:ParsingCtx) =
  let sql = SqlConnector(ctx.Options.["common_sql"])
  let serializer = Serializer(ctx)
  let tableName = "dkal_messages_demo"
  let principalName = dict()
  let principalId = dict()
  let fillPrincipals() = 
    if principalName.Count = 0 then
      use reader = sql.ExecReader "SELECT id, name FROM dkal_principals"
      while reader.Read() do
        let id = reader.GetInt32 0
        let name = (reader.GetString 1).Trim(' ')
        System.Console.WriteLine("{0} -> {1}", id,name)
        principalId.Add (name, id)
        principalName.Add (id, name)

  let rec principalById id =
    fillPrincipals()
    ctx.LookupOrAddPrincipal principalName.[id]
  
  let principalId (p:Principal) =
    fillPrincipals()
    match principalId.TryGetValue p.name with
      | true, v -> v
      | _ -> failwith ("principal '" + p.name + "' not in database")
  member this.PrincipalById id = principalById id  
  member this.PrincipalId (p:Principal) = principalId p

  member this.Close () = 
    sql.Close()
  
  member this.SendMessage (msg:Message) =
    let cmd = sql.GetCommand ("INSERT INTO " + tableName + " (sender, reciver, msg) VALUES (@s, @r, @m)")
    let add (n, v) = cmd.Parameters.AddWithValue (n, v) |> ignore
    add ("s", this.PrincipalId msg.source)
    add ("r", this.PrincipalId msg.target)
    add ("m", serializer.SerializeTerms [msg.message; msg.proviso])
    cmd.ExecuteNonQuery() |> ignore
  
  member this.CheckForMessage () =
    let markRead o =
      let cmd = sql.GetCommand ("UPDATE " + tableName + " SET readAt = CURRENT_TIMESTAMP WHERE sentAt = @s")
      let add (n, v) = cmd.Parameters.AddWithValue (n, v) |> ignore
      add ("s", o)
      cmd.ExecuteNonQuery() |> ignore
      
    let getMsg (rd:System.Data.SqlClient.SqlDataReader) =       
      // deserialize
      System.Console.WriteLine("{0}", rd.GetInt32 1)
      let src = principalById (rd.GetInt32 1)
      let raw = rd.GetString 2
      match serializer.DeserializeTerms raw with
        | [msg; prov] ->
          let msg =
            { source = src
              target = ctx.Me
              message = msg
              proviso = prov }
          rd.GetValue 0, msg
        | msg -> 
          failwith ("invalid message " + msg.Length.ToString() + " elts: " + raw)
          
    fillPrincipals()
    let res = 
      sql.ExecQuery ("SELECT TOP 1 sentAt, sender, msg FROM " + tableName + " WHERE reciver = @p__0 AND readAt IS NULL ORDER BY sentAt", 
                     [this.PrincipalId ctx.Me :> obj], false) |> Seq.map getMsg |> Seq.toList
    match res with
      | [] -> None
      | (v, msg) :: _ -> markRead v; Some msg
      
  
    
