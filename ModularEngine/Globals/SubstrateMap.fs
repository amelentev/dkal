namespace Microsoft.Research.Dkal.Globals

open Microsoft.Research.Dkal.Interfaces

open System.Collections.Generic

module SubstrateMap =
  let private substrates = new Dictionary<string, ISubstrate>()

  let AddSubstrate (s: ISubstrate) =
    for ns in s.Namespaces do
      substrates.[ns] <- s

  let GetSubstrate (ns: string) = 
    let found, substrate = substrates.TryGetValue ns
    if found then
      substrate
    else
      failwithf "No substrate found for namespace %O" ns

  