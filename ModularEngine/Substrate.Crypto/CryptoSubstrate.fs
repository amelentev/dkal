namespace Microsoft.Research.Dkal.Substrate

open Microsoft.Research.Dkal.Substrate.FSharp

// TODO 
// * actually add cryptographic functionallity
// * add more arithmetic functions
module Crypto =

  let CryptoSubstrate() = 
    let x = new FSharpSubstrate(["Crypto,SimpleIntArithmetic"])
    do x.Add "plus" (fun (x:int) (y:int) -> x + y)
    do x.Add "minus" (fun (x:int) (y:int) -> x - y)
    do x.Add "mult" (fun (x:int) (y:int) -> x * y)
    do x.Add "eq" (fun (x:int) (y:int) -> x = y)
    x


