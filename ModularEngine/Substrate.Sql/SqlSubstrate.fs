namespace Microsoft.Research.Dkal.Substrate.Sql

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
    let node = xd.DocumentElement.SelectSingleNode("//dbml:Database/dbml:Table[@Name='dbo." + table + "']/dbml:Type/dbml:Column[@Name='" + column + "']", xnm)
    if node = null then
      failwithf "Could not find column %O in table %O" column table
    else 
      let typ = node.Attributes.["Type"].Value
      System.Type.GetType(typ)

  interface ISubstrate with
    member this.Solve queries substs =
      let queries = queries |> Seq.map (function
        | :? DummySubstrateQueryTerm as t -> t.Query
        | _ as t -> failwithf "not DummySubstrateTerm: %A" t)
      // translate all >2-ary functions functions to 2-ary
      let rec normalize2 = function
        | App(f, a1 :: a2 :: a3 :: tl) ->
          let f' = {Name=f.Name; RetType=f.RetType; ArgsType=List.tail f.ArgsType; Identity=f.Identity}
          let n = normalize2( App(f', a2::a3::tl) )
          let f'' = {Name=f.Name; RetType=f.RetType; ArgsType=[f.ArgsType.Head; f.RetType]; Identity=f.Identity}
          App(f'', [normalize2 a1; n])
        | App(f, lst) ->
          App(f, lst |> List.map normalize2)
        | :? DummySubstrateQueryTerm as st when namespaces.Contains (st:>ISubstrateTerm).Namespace ->
          DummySubstrateQueryTerm(normalize2 st.Query, (st:>ISubstrateTerm).Namespace) :> ITerm
        | t -> t
      // translate boolean (table.column) to (table.column=1)
      let rec boolenize = function
        | App(f, []) when f.RetType = Type.Boolean && f.Name.Contains('.') ->
          let eq = {Name="eq"; RetType=Type.Boolean; ArgsType=[Type.Int32; Type.Int32]; Identity=None} : Function
          App(eq, [App(f, []); Const(Constant(1))])
        | t -> t
      // collect SubstrateTerms and remove them from the query
      let rec separateExternalSubstrates = function
        | App(f, args) ->
          let r = List.unzip (args |> List.map separateExternalSubstrates)
          (App(f, fst r), List.concat (snd r))
        | :? ISubstrateQueryTerm as st when not (namespaces.Contains(st.Namespace)) -> // external SubstrateTerm
          // Works only when used on top level without disjunctions
          Const (Constant true), [st]
          // The problem with arbitrary formula can be resolved by translating the formula to disjunctive normal form and handle each conjunction separately:
          //  So check than all negative SubstrateTerms gives no substitutions, get a substitutions from all positive SubstrateTerms and execute the rest.
        | t -> t, []
      let (queries, substrateTerms) = queries |> Seq.map separateExternalSubstrates |> Seq.toList |> List.unzip
      let substrateTerms = List.concat substrateTerms
      let queries = queries |> Seq.map normalize2 |> Seq.map boolenize
      let options = {Trace=1} : SqlCompiler.Options
      let vars = new HashSet<IVar>()
      queries |> Seq.iter (fun q -> vars.UnionWith(q.Vars))
      let checkAssumptions (subst:ISubstitution) (assumptions: ITerm seq)=
        let apply (t:ITerm) = t.Apply subst
        let sqlExpr = SqlCompiler.compile options this.NextId (assumptions |> Seq.map apply)
        SqlCompiler.execQuery (conn, options, sqlExpr, subst, Seq.toList(vars.AsEnumerable()))
      substs |> SubstrateDispatcher.Solve(substrateTerms) |> Seq.collect (fun subst -> checkAssumptions subst queries)

    member this.Update substrateUpdateTerm = failwith "TODO: implement" // TODO

    member xs.AreConsistentUpdates _ = failwith "TODO: implement" // TODO

    member this.Namespaces = new HashSet<_>(namespaces)
    member this.RequiredVars (query: ISubstrateQueryTerm) = []
