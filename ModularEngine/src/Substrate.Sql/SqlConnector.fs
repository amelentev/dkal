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

namespace Microsoft.Research.Dkal.Substrate.Sql

open System
open System.IO
open System.Text
open Microsoft.FSharp.Text
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Interfaces
open Microsoft.FSharp.Collections
open System.Data
open System.Data.SqlClient
open NLog

/// Keeps a connection with a SQL database given by a connection string
/// parameter upon construction. The SqlConnector can be used to execute
/// queries or other commands over the SQL database.
type SqlConnector(connStr) =
  let log = LogManager.GetLogger("Substrate.Sql")
  let conn = new SqlConnection(connStr)
  let check() =
    try
      use comm = new SqlCommand("SELECT 1", conn)
      comm.ExecuteScalar() |> ignore
    with e ->
      log.Info("reopening database: {0}", e)
      conn.Open()
  do
    try
      conn.Open()
    with e -> log.ErrorException("Unable to open db", e)
      
  /// Closes the connection to the SQL database
  member this.Close () =
    conn.Close()

  /// Executes the given query and returs a (lazy) sequence of results
  member this.ExecQuery (s:string, parms:seq<obj>) =
    check()
    log.Debug("execQ: {0} ::: {1}", s, parms |> Seq.mapi (fun i (o:obj) -> "@" + i.ToString() + ": " + o.ToString()) |> String.concat ", ")
    let addParm (comm:SqlCommand) (idx:int) o =
      comm.Parameters.AddWithValue ("p__" + idx.ToString(), o) |> ignore
    seq {
      use comm = new SqlCommand(s, conn)
      do Seq.iteri (addParm comm) parms
      use reader = comm.ExecuteReader()
      while reader.Read() do
        yield reader
      reader.Close() }

  /// Executes the given command and discards the results (useful for non-query
  /// commands such as updates)
  member this.ExecUpdate (s:string, parms:seq<obj>) =
    check()
    log.Debug("execU: {0} ::: {1}", s, parms |> Seq.mapi (fun i (o:obj) -> "@" + i.ToString() + ": " + o.ToString()) |> String.concat ", ")
    let addParm (comm:SqlCommand) (idx:int) o =
      comm.Parameters.AddWithValue ("p__" + idx.ToString(), o) |> ignore
    use comm = new SqlCommand(s, conn)
    do Seq.iteri (addParm comm) parms
    comm.ExecuteNonQuery()

  /// Returns an ITerm by parsing the DbDataReader depending on the type of 
  /// the given variable
  member this.ReadVar (rd:Common.DbDataReader, var: IVar, idx:int) : ITerm =
    try
      if var.Type = Type.Boolean then
        match rd.GetValue idx with
          | :? bool as b -> Constant b :> ITerm
          | :? int as i -> Constant (i <> 0) :> ITerm
          | _ -> Constant (rd.GetBoolean idx) :> ITerm
      elif var.Type = Type.String then
        Constant (rd.GetString idx) :> ITerm
      elif var.Type = Type.Principal then
        PrincipalConstant (rd.GetString idx) :> ITerm
      else
        Constant (rd.GetValue idx) :> ITerm
    with :? InvalidCastException as e ->
      log.Error("cannot convert parm #{0}, to {1}, value: '{2}'", idx, var.Type.Name, rd.GetValue idx)
      raise e
