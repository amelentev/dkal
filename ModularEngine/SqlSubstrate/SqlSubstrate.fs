﻿namespace Microsoft.Research.Dkal.SqlSubstrate

open System.Collections.Generic

open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast.Tree
open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Substrate

open System.Data
open System.Data.SqlClient
open System.Text
open System.Linq
open System.Collections.Generic
open System.Xml

type SqlSubstrate(connStr : string, schemaFile: string, namespaces: string list) = 
  let mutable nextId = 0
  let conn = SqlConnector(connStr)

  member private this.NextId () =
    nextId <- nextId - 1
    nextId

  member this.ConnectionString = connStr

  member this.SchemaFile = schemaFile

  member this.GetColumnType (table: string) (column: string) =
    let xd = new XmlDocument()
    xd.Load(schemaFile)
    let xnm = new XmlNamespaceManager(xd.NameTable)
    xnm.AddNamespace("dbml", "http://schemas.microsoft.com/linqtosql/dbml/2007")
    let node = xd.DocumentElement.SelectSingleNode("//dbml:Database/dbml:Table[@Name='" + table + "']/dbml:Type/dbml:Column[@Name='" + column + "']", xnm)
    if node = null then
      failwithf "Could not find column %O in table %O" column table
    else 
      let typ = node.Attributes.["Type"].Value
      System.Type.GetType(typ)

  interface ISubstrate with
    member this.Solve queries substs =
      let queries = queries |> Seq.map (function
        | :? DummySubstrateTerm as t -> t.Query
        | _ as t -> failwithf "not DummySubstrateTerm: %A" t)
      // translate all >2-ary functions functions to 2-ary
      let rec normalize2 = function
        | App(f, a1 :: a2 :: a3 :: tl) ->
          let f' = {Name=f.Name; RetType=f.RetType; ArgsType=List.tail f.ArgsType}
          let n = normalize2( App(f', a2::a3::tl) )
          let f'' = {Name=f.Name; RetType=f.RetType; ArgsType=[f.ArgsType.Head; f.RetType]}
          App(f'', [normalize2 a1; n])
        | App(f, lst) ->
          App(f, lst |> List.map normalize2)
        | t -> t
      // translate boolean (table.column) to (table.column=1)
      let rec boolenize = function
        | App(f, []) when f.RetType = Type.Boolean && f.Name.Contains('.') ->
          let eq = {Name="eq"; RetType=Type.Boolean; ArgsType=[Type.Int32; Type.Int32]} : Function
          App(eq, [App(f, []); Const(SubstrateConstant(1))])
        | t -> t
      let queries = queries |> Seq.map normalize2 |> Seq.map boolenize
      let options = {Trace=9} : SqlCompiler.Options
      let vars = new HashSet<IVar>()
      queries |> Seq.iter (fun q -> vars.UnionWith(q.Vars))
      let checkAssumptions (subst:ISubstitution) (assumptions: ITerm seq)=
        let apply (t:ITerm) = t.Apply subst
        let sqlExpr = SqlCompiler.compile options this.NextId (assumptions |> Seq.map apply)
        SqlCompiler.execQuery (conn, options, sqlExpr, subst, Seq.toList(vars.AsEnumerable()))
      substs |> Seq.collect (fun subst -> checkAssumptions subst queries)

    member this.Namespaces = new HashSet<_>(namespaces)
    member this.RequiredVars (query: ISubstrateTerm) = []