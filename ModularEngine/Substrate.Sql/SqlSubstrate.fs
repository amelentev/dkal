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
  let trace = 1

  let prepareQuery (query: ITerm) =
    // translate all >2-ary functions functions to 2-ary
    let rec normalize2 = function
      | AsBoolean(st) -> AsBoolean (normalize2 (st :> ITerm) :?> ISubstrateQueryTerm)
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
      | Column(_, _) as c when c.Type = Type.Boolean ->
        let eq = {Name="eq"; RetType=Type.Boolean; ArgsType=[Type.Boolean; Type.Boolean]; Identity=None} : Function
        App(eq, [c; True])
      | t -> t
    // collect SubstrateTerms and remove them from the query
    let rec separateExternalSubstrates = function
      | AsBoolean(st) when not (namespaces.Contains(st.Namespace)) -> // external SubstrateTerm
        // Works only when used on top level without disjunctions
        Const (Constant true), [st]
        // The problem with arbitrary formula can be resolved by translating the formula to disjunctive normal form and handle each conjunction separately:
        //  So check than all negative SubstrateTerms gives no substitutions, get a substitutions from all positive SubstrateTerms and execute the rest.
      | App(f, args) ->
        let r = List.unzip (args |> List.map separateExternalSubstrates)
        (App(f, fst r), List.concat (snd r))
      | t -> t, []
    let (query, ests) = query |> separateExternalSubstrates
    let query = query |> normalize2 |> boolenize
    (query, ests)

  let prepareUpdateQuery (query: ITerm) =
    let (query, ests) = prepareQuery query
    if ests.Length>0 then
      failwith "nested substrateTerms in SubstrateUpdateTerm not supported"
    if query.Vars.Length > 0 then
      failwithf "Free variables in SubstrateUpdateTerm: %A"  query
    query

  member private this.NextId () =
    nextId <- nextId - 1
    nextId

  member this.ConnectionString = connStr

  member this.SchemaFile = schemaFile

  member this.HasTable (table: string) =
    let xd = new XmlDocument()
    xd.Load(schemaFile)
    let xnm = new XmlNamespaceManager(xd.NameTable)
    xnm.AddNamespace("dbml", "http://schemas.microsoft.com/linqtosql/dbml/2007")
    let node = xd.DocumentElement.SelectSingleNode("//dbml:Database/dbml:Table[@Name='dbo." + table + "']", xnm)
    node <> null 

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

  member this.modify (sut: SqlSubstrateModifyTerm) =
    let query = prepareUpdateQuery sut.Query
    let updates = sut.ColsMapping |> Seq.map (fun x ->
      (x.Key, fst (SqlCompiler.compile trace this.NextId [x.Value]))) |> List.ofSeq
    let where = SqlCompiler.compile trace this.NextId [query]
    SqlCompiler.execUpdate (conn, trace, where, updates)

  member this.delete (sdt: SqlSubstrateDeleteTerm) =
    let query = prepareUpdateQuery sdt.Query
    let where = SqlCompiler.compile trace this.NextId [query]
    SqlCompiler.execDelete (conn, trace, where, sdt.Table)

  member this.insert (sit: SqlSubstrateInsertTerm) =
    let values = sit.Values |> Seq.map (fun x -> 
      let cc = SqlCompiler.compile trace this.NextId [x.Value]
      (x.Key, fst cc)
    )
    SqlCompiler.execInsert (conn, trace, sit.Table, dict values)

  interface ISubstrate with
    member this.Solve queries substs =
      let (queries: seq<DummySubstrateQueryTerm>) = queries |> Seq.cast
      let (queries, substrateTerms) = queries |> Seq.toList |> List.map (fun t -> t.Query) |> List.map prepareQuery |> List.unzip
      let substrateTerms = List.concat substrateTerms
      let vars = new HashSet<IVar>()
      queries |> Seq.iter (fun q -> vars.UnionWith(q.Vars))

      let checkAssumptions (subst:ISubstitution) (assumptions: ITerm seq)=
        let apply (t:ITerm) = t.Apply subst
        let sqlExpr = SqlCompiler.compile trace this.NextId (assumptions |> Seq.map apply)
        SqlCompiler.execQuery (conn, trace, sqlExpr, subst, Seq.toList(vars.AsEnumerable()))
      substs |> SubstrateDispatcher.Solve(substrateTerms) |> Seq.collect (fun subst -> checkAssumptions subst queries)

    member this.Update terms =
      let update1 (term: ISubstrateUpdateTerm) = 
        match term with
        | :? SqlSubstrateModifyTerm as smt ->
          this.modify smt
        | :? SqlSubstrateDeleteTerm as sdt ->
          this.delete sdt
        | :? SqlSubstrateInsertTerm as sit ->
          this.insert sit
        | _ as t -> failwithf "unknown SubstrateUpdateTerm: %A" t
      terms |> Seq.map update1 |> List.ofSeq |> List.exists(fun x -> x)

    // We return true, and we postpone actual consistency checking until execution
    member xs.AreConsistentUpdates updates = true

    member this.Namespaces = new HashSet<_>(namespaces)
    member this.RequiredVars (query: ISubstrateQueryTerm) = []
