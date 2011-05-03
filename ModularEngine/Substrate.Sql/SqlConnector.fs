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
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Interfaces
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
    try
      conn.Open()
    with e -> printfn "%O" e
      
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
      System.Console.WriteLine ("cannot convert parm #{0}, to {1}, value: '{2}'", idx, var.Type.Name, rd.GetValue idx)
      raise e
