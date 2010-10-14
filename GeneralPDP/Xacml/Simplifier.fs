namespace Microsoft.Research.GeneralPDP.XACML

open Ast

module Simplifier =

  let analyzeRestrictions e = 
    let negRestr eq = 
      match eq with
      | "=" -> "<>"
      | "<>" -> "="
      | _ -> (failwith "Unrecognized restriction: " + eq)
    let contains restrs (e1, e2, eq) = 
      List.exists (fun (a, b, eq') -> (a=e1 && b=e2 || a=e2 && b=e1) && eq' = eq) restrs
    let rec restrsCompatible rs = 
      match rs with
      | [] -> true
      | (e1, e2, eq) :: rs -> not (contains rs (e1, e2, negRestr eq)) && restrsCompatible rs
    let rec doAnalyzeRestrictions e restrs = 
      match e with
      | ApplyExp(f, [e1; e2]) when f.EndsWith("-equal") -> 
          if contains restrs (e1, e2, "=") then
            printfn "Hit ="
            ValueExp(BoolAtomValue(true)), restrs
          elif contains restrs (e1, e2, "<>") then
            printfn "Hit <>"
            ValueExp(BoolAtomValue(false)), restrs
          else
            e, [(e1, e2, "=")]
      | ApplyExp("and", es) -> 
          let mutable restrs' = []
          let mutable es' = []
          for e in es do
            let e', moreRestrs = doAnalyzeRestrictions e (restrs @ restrs')
            restrs' <- restrs' @ moreRestrs
            if restrsCompatible restrs' then
              es' <- es' @ [e']
            else
              printfn "Hit and"
              es' <- [ValueExp(BoolAtomValue(false))]
          ApplyExp("and", es'), restrs'
      | ApplyExp("not", [e]) -> 
          let e', restrs' = doAnalyzeRestrictions e restrs
          let restrs' = List.map (fun (e1, e2, eq) -> (e1, e2, negRestr eq)) restrs'
          if restrsCompatible (restrs' @ restrs) then
            ApplyExp("not", [e']), restrs'
          else
            printfn "Hit not"
            ValueExp(BoolAtomValue(false)), restrs' 
      | ApplyExp("or", es) ->
          let mutable es' = []
          for e in es do
            let e', moreRestrs = doAnalyzeRestrictions e restrs
            if restrsCompatible (moreRestrs @ restrs) then
              es' <- es' @ [e']
            else
              printfn "Hit or"
              es' <- [ValueExp(BoolAtomValue(false))]
          ApplyExp("or", es'), []
      | ApplyExp(f, es) ->
          let es = List.map (fun e -> let e', _ = doAnalyzeRestrictions e restrs; 
                                      e') es
          ApplyExp(f, es), []
      | e -> e, []
    let e, restrs = doAnalyzeRestrictions e []
    e

  /// Given an AsExpression, returns an equivalent and (hopefully) smaller AsExpression
  let rec simplifyExp (e: Expression) =
   
    // TODO. IDEA: order arguments in function application and eliminate duplicates (in case of and/or). Common subexpression elimination
    let rec simplifyRound e =
      match e with
      | ApplyExp(f, es) -> 
        match f with
        | "not" ->
          match es with
          | [ValueExp(BoolAtomValue false)] -> ValueExp(BoolAtomValue true)
          | [ValueExp(BoolAtomValue true)] -> ValueExp(BoolAtomValue false)
          | [ApplyExp("not", [e])] -> simplifyRound e
          | [ApplyExp("and", es)] -> ApplyExp("or", List.map (fun e -> ApplyExp("not", [simplifyRound e])) es)
          | [ApplyExp("or", es)] -> ApplyExp("and", List.map (fun e -> ApplyExp("not", [simplifyRound e])) es)
          | [v] -> ApplyExp("not", [simplifyRound v])
          | _ -> failwith "not should receive one single parameter"
        | "and" -> 
          let flattenEs = List.collect (fun e -> match e with
                                                 | ApplyExp("and", es) -> es
                                                 | e -> [e]) (List.map simplifyRound es)
          let relevantExps = List.filter (fun e -> e <> ValueExp(BoolAtomValue true)) flattenEs
          if relevantExps.IsEmpty then
            ValueExp(BoolAtomValue true)
          else if List.exists (fun e -> e = (ValueExp(BoolAtomValue false))) relevantExps then
            ValueExp(BoolAtomValue false)
          else if relevantExps.Length = 1 then
            relevantExps.[0]
          else
            ApplyExp(f, relevantExps)
        | "or" -> 
          let flattenEs = List.collect (fun e -> match e with
                                                  | ApplyExp("or", es) -> es
                                                  | e -> [e]) (List.map simplifyRound es)
          let relevantExps = List.filter (fun e -> e <> ValueExp(BoolAtomValue false)) flattenEs
          if relevantExps.IsEmpty then
            ValueExp(BoolAtomValue false)
          else if List.exists (fun e -> e = (ValueExp(BoolAtomValue true))) relevantExps then
            ValueExp(BoolAtomValue true)
          else if relevantExps.Length = 1 then
            relevantExps.[0]
          else
            ApplyExp(f, relevantExps)
        | "integer-less-than-or-equal" ->
          match es with
          | [ValueExp(IntAtomValue a); ValueExp(IntAtomValue b)] -> ValueExp(BoolAtomValue (a <= b))
          | es -> ApplyExp(f, List.map simplifyRound es)
        | "integer-less-than" ->
          match es with
          | [ValueExp(IntAtomValue a); ValueExp(IntAtomValue b)] -> ValueExp(BoolAtomValue (a < b))
          | es -> ApplyExp(f, List.map simplifyRound es)
        | "integer-greater-than-or-equal" ->
          match es with
          | [ValueExp(IntAtomValue a); ValueExp(IntAtomValue b)] -> ValueExp(BoolAtomValue (a >= b))
          | es -> ApplyExp(f, List.map simplifyRound es)
        | "integer-greater-than" ->
          match es with
          | [ValueExp(IntAtomValue a); ValueExp(IntAtomValue b)] -> ValueExp(BoolAtomValue (a > b))
          | es -> ApplyExp(f, List.map simplifyRound es)
        | "integer-equal" ->
          match es with
          | [ValueExp(IntAtomValue a); ValueExp(IntAtomValue b)] -> ValueExp(BoolAtomValue (a = b))
          | es -> ApplyExp(f, List.map simplifyRound es)
        | "boolean-equal" ->
          match es with
          | [ValueExp(BoolAtomValue a); ValueExp(BoolAtomValue b)] -> ValueExp(BoolAtomValue (a = b))
          | es -> ApplyExp(f, List.map simplifyRound es)
        | "string-equal" ->
          match es with
          | [ValueExp(StringAtomValue a); ValueExp(StringAtomValue b)] -> ValueExp(BoolAtomValue (a = b))
          | es -> ApplyExp(f, List.map simplifyRound es)
        | f -> ApplyExp(f, List.map simplifyRound es)
      | e -> e
    let initE = e
    let e = simplifyRound e
    if e = initE then
      e
    else
      simplifyExp e
