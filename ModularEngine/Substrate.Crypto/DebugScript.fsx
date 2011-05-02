
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

// Convenient syntax for terms
let x = var "x"
let y = var "y"
let z = var "z"

let (-+-) x y = sub.createFunctionTerm "plus" [x;y]
let (---) x y = sub.createFunctionTerm "minus" [x;y]
let (-*-) x y = sub.createFunctionTerm "mult" [x;y]
let (-=-) x y = sub.createFunctionTerm "eq" [x;y]

let (-@-) x y = sub.createQueryTerm x y

// Query1 (only constants)
let query1 = (con 1 -+- con 2) -@- (con 3)

let r1 = sub.Solve [query1] [Substitution.Id]
printf "%s" ((not <| Seq.isEmpty r1).ToString())

// Substitution for variables x and y
let subst = Substitution.Id.Extend(toVar x, con 1).Extend(toVar y, con 2)

// Query2 (constant result)
let query2 = (x -+- y) -@- (con 3)

let r2 = sub.Solve [query2] [subst]
printf "%s" ((not <| Seq.isEmpty r2).ToString())

// Query3 (compute value for z)
let query3 = (x -+- y) -@- z

let r3 = sub.Solve [query3] [subst]
printfn "%s" ((not <| Seq.isEmpty r3).ToString())
let zval = (Seq.head r3).Apply(toVar z)
let zconst = toConstElem (zval :?> FunctionTerm)
printfn "%s" (zconst.ToString()) 

// Add string length function
sub.Add "length" (fun (x:string) -> x.Length)
let len x = sub.createFunctionTerm "length" [x]

// Query4 (value of type string)
let query4 = (x -+- y) -@- (len (con "abc"))

let r4 = sub.Solve [query4] [subst]
printfn "%s" ((not <| Seq.isEmpty r4).ToString())

// Query5
let query5 = len (con "abc") -+- x -@- z

let r5 = sub.Solve [query5] [subst]
printfn "%s" ((not <| Seq.isEmpty r5).ToString())
let zval2 = (Seq.head r5).Apply (toVar z)
let zconst2 = toConstElem (zval2 :?> FunctionTerm)
printfn "%s" (zconst2.ToString()) 

(* Does not yet work
// generic function
let gplus (x:'a) (y:'a) = 
  GlobalAssociations.GetNumericAssociation<'a>().Add(x,y) 

sub.Add "gplus" gplus
let (-++-) x y  = sub.createFunctionTerm "gplus" [x;y]

let query6 = (con 4.0) -++- (con 3.0) -@- (con 7.0)

let r6 = sub.Solve [query6] [subst]
printfn "%s" ((not <| Seq.isEmpty r6).ToString())
*) 