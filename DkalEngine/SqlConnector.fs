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
open Microsoft.FSharp.Collections
open System.Data
open System.Data.SqlClient

type SqlConnector(connStr) =
  let conn = new SqlConnection(connStr)
  let check() =
    try
      use comm = new SqlCommand("SELECT 1", conn)
      comm.ExecuteScalar() |> ignore
    with e ->
      System.Console.WriteLine ("reopening database: " + e.Message)
      conn.Open()
  do
    conn.Open()
      
  member this.Close () =
    conn.Close()

  member this.ExecNonQuery s =
    check()
    let comm = new SqlCommand(s, conn)
    comm.ExecuteNonQuery()
  
  member this.ExecReader s =
    check()
    let comm = new SqlCommand(s, conn)
    comm.ExecuteReader()
  
  member this.GetCommand s =
    new SqlCommand(s, conn)
  
  member this.ExecQuery (s:string, parms:seq<obj>, log) =
    check()
    if log then
      System.Console.WriteLine ("execQ: {0} ::: {1}", s, parms |> Seq.mapi (fun i (o:obj) -> "@" + i.ToString() + ": " + o.ToString()) |> String.concat ", ")
    let addParm (comm:SqlCommand) (idx:int) o =
      comm.Parameters.AddWithValue ("p__" + idx.ToString(), o) |> ignore
    seq {
      use comm = new SqlCommand(s, conn)
      do Seq.iteri (addParm comm) parms
      use reader = comm.ExecuteReader()
      while reader.Read() do
        yield reader }
      
  member this.ReadVar (getPrincipal : int -> Principal, rd:Common.DbDataReader, var:Var, idx:int) =
    try
      if var.typ.name = "bool" then
        match rd.GetValue idx with
          | :? bool as b -> Const.Bool b
          | :? int as i -> Const.Bool (i <> 0)
          | _ -> Const.Bool (rd.GetBoolean idx)
      elif var.typ.name = "text" then
        Const.Text (rd.GetString idx)
      elif var.typ.name = "principal" then
        Const.Principal (getPrincipal (rd.GetInt32 idx))
      else
        Const.Int (rd.GetInt32 idx)
    with :? InvalidCastException as e ->
      System.Console.WriteLine ("cannot convert parm #{0}, to {1}, value: '{2}'", idx, var.typ.name, rd.GetValue idx)
      raise e
    