namespace Microsoft.Research.Dkal.Ast

open Microsoft.Research.Dkal.Interfaces

type Substitution(subst : IVar -> ITerm) = 
    
  /// Returns a new Substitution that behaves like the identity
  static member Id = 
    new Substitution(fun v -> (v :> ITerm)) :> ISubstitution

  interface ISubstitution with
      
    /// Returns a new Substitution that results from extending the current 
    /// Substitution so that it maps x to mt and leaves the rest unchanged
    member s.Extend (x: IVar, t: ITerm) = 
      new Substitution(fun z -> if z = x then t else subst z) :> ISubstitution
       
    /// Applies this Substitution to the given IVar
    member s.Apply (v: IVar) = 
      subst v
      
    /// Returns a new Substitution that results from first applying s' and 
    /// then applying the current Substitution
    member s.ComposeWith (s': ISubstitution) =
      new Substitution(fun z -> z.Apply(s').Apply(s)) :> ISubstitution

    /// Returns true iff v is affected by this Substitution
    member s.Contains (v: IVar) =
      subst v <> (v :> ITerm)

