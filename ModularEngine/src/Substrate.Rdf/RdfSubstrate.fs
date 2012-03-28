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
namespace Microsoft.Research.Dkal.Substrate.Rdf

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast
open System.Collections.Generic
open VDS.RDF.Query
open System

/// Resource Description Framework (~Semantic Web) Substrate
///  endpointUrl - SPARQL endpoint for queries. ex: http://dbpedia.org/sparql/
type RdfSubstrate(endpointUrl: string, namespaces: string list) =
  let endpoint = new SparqlRemoteEndpoint(new Uri(endpointUrl));

  let constant t v =
    if t = Type.Int32 then
      Constant (Int32.Parse v)
    else
      Constant v

  let solve11 (query: SparqlQueryTerm) (subst: ISubstitution) =
    let sparql = SparqlParameterizedString(query.query)
    // TODO: non-uri parameters
    query.inputs |> List.iter (fun v -> sparql.SetUri(v.Name, new Uri((subst.Apply(v) :?> IConst).Value.ToString())))
    let res = endpoint.QueryWithResultSet(sparql.ToString())
    res |> Seq.map (fun row ->
          query.outputs |> Seq.fold (fun (acc: ISubstitution) v -> acc.Extend(v, constant v.Type (row.Value(v.Name).ToString())) ) subst
          )

  interface ISubstrate with
    member s.RequiredVars st = 
      st.Vars

    member s.Namespaces = new HashSet<_>(namespaces)

    member s.Solve queries substs =
      let queries: SparqlQueryTerm seq = queries |> Seq.cast
      let solve1Many substs query =
        substs |> Seq.collect (solve11 query)
      queries |> Seq.fold solve1Many substs

    member s.Update terms =
      failwith "Not implemented"

    member s.AreConsistentUpdates _ = failwith "XML substrate does not support updates"
