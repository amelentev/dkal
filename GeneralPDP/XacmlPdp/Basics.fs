namespace Microsoft.Research.GeneralPDP.XACML.PDP

open Microsoft.Research.GeneralPDP.XACML.Ast

module Basics =

  // Exception thrown when an attribute is missing while evaluating a request
  exception XacmlAttributeMissingException of AttributeDesignator

  // Basic type redefinitions referring to semantic elements
  type PredicateF = RequestContext -> Value
  type ObligatedDecisionT = (Decision * Obligation list)
  type PolicyF = RequestContext -> ObligatedDecisionT
  type RuleF = RequestContext -> Decision
  type RuleCombiningAlgF = (Decision * RuleF) list -> RequestContext -> Decision
  type PolicyCombiningAlgF = (PredicateF * PolicyF) list -> Obligation list -> RequestContext -> ObligatedDecisionT
  type FunctionF = Value list -> Value