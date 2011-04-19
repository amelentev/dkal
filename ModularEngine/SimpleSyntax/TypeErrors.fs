module Microsoft.Research.Dkal.SimpleSyntax.TypeErrors

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast

let pp = SimplePrettyPrinter() :> IPrettyPrinter

/// Checks that the MetaTerm mt has Type t, returning mt; failing otherwise
let typeCheck (mt: MetaTerm) (t: Type) =
  if mt.Typ() <> t then
    failwith <| "Typing error, found " + pp.PrintType(mt.Typ())
                + " when expecting " + pp.PrintType t
                + " on " + pp.PrintMetaTerm mt
  else
    mt

