namespace Microsoft.Research.Dkal.SignatureProvider.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.SignatureProvider.Simple

/// The SignatureProviderFactory provides a factory to construct different signature 
/// providers. A signature provider kind must be provided.
type SignatureProviderFactory() =
  static member SignatureProvider (kind: string) = 
    match kind with
    | "simple" -> new SimpleSignatureProvider() :> ISignatureProvider
    | k -> failwith <| "Unrecognized signature provider kind: " + k


