namespace Microsoft.Research.Dkal.Interfaces

/// ISignatureProvider provides an interface for implementations to construct
/// and verify signed infons
type ISignatureProvider =
  
  /// Construct a signature for the given ITerm
  abstract member ConstructSignature: infon: ITerm -> principalName: string -> int

  /// Checks if the given signature is correct
  abstract member CheckSignature: infon: ITerm -> principalName: string -> signature: int -> bool
