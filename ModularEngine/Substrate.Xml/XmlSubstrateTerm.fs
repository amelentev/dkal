namespace Microsoft.Research.Dkal.Substrate.Xml

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast

type XmlSubstrateTerm(ns: string, xpath: string, vars: IVar list, outputVar: IVar) =

  let mutable subst = Substitution.Id

  interface ISubstrateTerm with
    
    member x.Type = Type.Boolean
    member x.Vars = vars
    member x.Apply subst' =
      subst <- subst.ComposeWith subst'
      x :> ITerm
    member x.Normalize() = x :> ITerm
    member x.UnifyFrom _ _ = None
    member x.Unify _ = None

    member x.Namespace = ns

