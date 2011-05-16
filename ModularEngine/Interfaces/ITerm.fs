﻿namespace Microsoft.Research.Dkal.Interfaces

type IType = 
  abstract member Name: string
  abstract member FullName: string

type ITerm =
  abstract member Type: IType
  abstract member Vars: IVar list
  abstract member BoundVars: IVar list
  abstract member Apply: ISubstitution -> ITerm
  abstract member Normalize: unit -> ITerm
  abstract member UnifyFrom: ISubstitution -> ITerm -> ISubstitution option
  abstract member Unify: ITerm -> ISubstitution option

and IVar =
  inherit ITerm
  abstract member Name: string

and IConst =
  inherit ITerm
  abstract member Value: obj

and ISubstitution =
  abstract member Apply: IVar -> ITerm
  abstract member Extend: v: IVar * t: ITerm -> ISubstitution
  abstract member ComposeWith: ISubstitution -> ISubstitution
  abstract member DomainContains: IVar -> bool
  abstract member Domain: IVar list
  abstract member RestrictTo: IVar list -> ISubstitution
  abstract member Forget: IVar list -> ISubstitution
  abstract member IsVariableRenaming: bool
