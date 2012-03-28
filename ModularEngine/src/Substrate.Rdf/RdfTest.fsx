#I @".\bin\debug\"
#r "dotNetRDF.dll"
#r "Substrate.Rdf.dll"
#r "Interfaces.dll"
#r "Ast.dll"

open Microsoft.Research.Dkal.Substrate.Rdf
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Interfaces
open VDS.RDF.Query
open VDS.RDF.Parsing
open System.Text.RegularExpressions
open System

let endpoint = new SparqlRemoteEndpoint(new Uri("http://dbpedia.org/sparql"));
let squery = " Select ?Conf, ?Conf where { ?Conf  dcterms:subject  category:Computer_science_conferences \n filter (?Conf = @Conf) }"
let query = SparqlParameterizedString(squery)
query.SetUri("Conf",  new Uri("http://dbpedia.org/resource/Operating_Systems_Design_and_Implementation"))
printf "%A" (query.ToString())
let res = endpoint.QueryWithResultSet(query.ToString())
res |> Seq.iter (printf "%A")

let var name typ = {Name=name; Type=typ} :> IVar
let confvar = var "Conf" Type.String
let q = SparqlQueryTerm("", " select ?Conf where { ?Conf  dcterms:subject  category:Computer_science_conferences \n filter (?Conf = @Conf) }", [confvar], [confvar])
let subst = RdfSubstrate("http://dbpedia.org/sparql", [""]) :> ISubstrate
let res2 = subst.Solve [q] [Substitution.Id.Extend(confvar, Constant("http://dbpedia.org/resource/Operating_Systems_Design_and_Implementation"))]

printf "%A" res2
