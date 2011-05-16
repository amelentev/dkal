namespace Microsoft.Research.Dkal.SignatureProvider.Simple

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Substrate

open NLog

type SimpleSignatureProvider() =

  let log = LogManager.GetLogger("SignatureProvider.Simple")

  interface ISignatureProvider with
    
    /// TODO: use real private keys and signatures
    member see.ConstructSignature (infon: ITerm) (ppalName: string) =
      (infon, ppalName).GetHashCode()

    /// TODO: use real public keys and signature checking
    member see.CheckSignature (infon: ITerm) (ppalName: string) (signature: int) =
      let expectedSignature = (infon, ppalName).GetHashCode()
      expectedSignature = signature



