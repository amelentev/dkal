namespace Microsoft.Research.Dkal.Factories

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.SimpleSyntax
open Microsoft.Research.Dkal.Ast.TypedSyntax

/// The PrettyPrinterFactory provides a factory to construct different pretty
/// printers. A pretty printer kind must be provided.
type PrettyPrinterFactory() =
  static member AstPrinter (kind: string) = 
    match kind with
    | "simple" -> SimplePrettyPrinter() :> IAstPrettyPrinter
    | "typed" -> TypedPrettyPrinter() :> IAstPrettyPrinter
    | k -> failwith <| "Unrecognized pretty printer kind: " + k
