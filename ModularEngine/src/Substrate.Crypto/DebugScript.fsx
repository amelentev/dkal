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

#I @"..\Substrate.FSharp\bin\debug\"
#r "Substrate.FSharp.dll"
#r "Ast.dll"
#r "Ast.Infon.dll"
#r "Ast.Tree.dll"
#r "Interfaces.dll"

#I @"..\Substrate.Crypto\bin\debug\"
#r @"Substrate.Crypto.dll"

#r "FSharp.PowerPack.dll"
open Microsoft.FSharp.Math

open Microsoft.Research.Dkal.Substrate.FSharp
open Microsoft.Research.Dkal.Substrate.Crypto

open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Interfaces

// create substrate
let sub = CryptoSubstrate()

type Result = | Sat | UnSat

// helper functions for checking queries
let subsumes (t:#ITerm) (s1:#ISubstitution) (s2:#ISubstitution) =
  (t.Apply s2).Apply s1 = t.Apply s1

type resultT() = 
  let mutable (res:list<string * string>) = []
  with
  member this.succeed(n:string) = 
    System.Console.WriteLine ("success: " + n)
    res <- res @ [(n,"passed")]
    true
  member this.fail(n:string) =
    System.Console.WriteLine ("fail: " + n)
    res <- res @ [(n,"failed")]
    false
  member this.report() = List.fold (fun s (x,y) -> s + "\n" + x + ": " + y) "" res
  override this.ToString() = res.ToString()

let results = new resultT()

let check n res query subst resultSubst =
  match (res, sub.Solve [query] [subst]) with
  | UnSat, r when Seq.isEmpty r -> results.succeed n
  | Sat, r when not (Seq.isEmpty r) && subsumes query (Seq.head r) resultSubst -> results.succeed n 
  | _ -> results.fail n

let ids = Substitution.Id

// Convenient syntax for terms
let x = var<int> "x"
let y = var<int> "y"
let z = var<int> "z"
let s = var<string> "s"

let (-+-) x y = sub.createFunctionTerm "plus" [x;y]
let (---) x y = sub.createFunctionTerm "minus" [x;y]
let (-*-) x y = sub.createFunctionTerm "mult" [x;y]
let (-=-) x y = sub.createFunctionTerm "eq" [x;y]

let (-@-) x y = sub.createQueryTerm x y

// Add string length function
sub.Add "length" (fun (x:string) -> x.Length)
let len x = sub.createFunctionTerm "length" [x]

// Add substring function
sub.Add "substring" (fun (x:string) (i:int) (l:int) -> x.Substring(i,l))
let substring x i l = sub.createFunctionTerm "substring" [x;i;l]

// Substitutions for variables x and y
let subst = ids.Extend(toVar x, con 1).Extend(toVar y, con 2)
let substf = ids.Extend(toVar x, con 1).Extend(toVar y, con 3)

// Query1 (only constants)
let query1 = (con 1 -+- con 2) -@- (con 3)
check "query1" Sat query1 ids ids

// Query2 (constant result)
let query2 = (x -+- y) -@- (con 3)
check "query2" Sat query2 subst ids
check "query2 (wrong subsitution)" UnSat query2 substf ids

let query2f = (x -+- y) -@- (con 4)
check "query2f" UnSat query2f subst ids

// Query3 (compute value for z)
let query3 = (x -+- y) -@- z
check "query3" Sat query3 subst <| ids.Extend(toVar z, con 3)

// Query4 (value of type string)
let query4 = (x -+- y) -@- (len (con "abc"))
check "query4" Sat query4 subst ids
check "query4 (wrong substitution)" UnSat query4 substf ids

// Query5
let query5 = len (con "abc") -+- x -@- z
check "query5" Sat query5 subst <| ids.Extend(toVar z, con 4)

let query6 = substring (con "abcde") (con 1) (con 3) -@- con "bcd"
check "query6" Sat query6 ids ids

// string variable
let query7 = substring (con "abcde") (con 1) (con 3) -@- s
check "query7" Sat query7 ids <| ids.Extend(toVar s, con "bcd")

(* Does not yet work
// generic function
let gplus (x:'a) (y:'a) = 
  GlobalAssociations.GetNumericAssociation<'a>().Add(x,y) 

sub.Add "gplus" gplus
let (-++-) x y  = sub.createFunctionTerm "gplus" [x;y]

let query6 = (con 4.0) -++- (con 3.0) -@- (con 7.0)
*) 

results.report()