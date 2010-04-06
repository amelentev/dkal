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
  do
    conn.Open()
      
  member this.ExecNonQuery s =
    let comm = new SqlCommand(s, conn)
    comm.ExecuteNonQuery()
  
  member this.ExecReader s =
    let comm = new SqlCommand(s, conn)
    comm.ExecuteReader()
  
  member this.GetCommand s =
    new SqlCommand(s, conn)
  
  member this.ExecQuery (s:string, parms:seq<obj>, log) =
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
      
  member this.ReadVar (getPrincipal : int -> Principal, rd:SqlDataReader, var:Var, idx:int) =
    try
      if var.typ.name = "bool" then
        match rd.GetValue idx with
          | :? bool as b -> Const.Bool b
          | :? int as i -> Const.Bool (i <> 0)
          | _ -> Const.Bool (rd.GetBoolean idx)
      elif var.typ.name = "principal" then
        Const.Principal (getPrincipal (rd.GetInt32 idx))
      else
        Const.Int (rd.GetInt32 idx)
    with :? InvalidCastException as e ->
      System.Console.WriteLine ("cannot convert parm #{0}, to {1}, value: '{2}'", idx, var.typ.name, rd.GetValue idx)
      raise e
    