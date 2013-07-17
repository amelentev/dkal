// *********************************************************
//
//    Copyright (c) Microsoft. All rights reserved.
//    This code is licensed under the Apache License, Version 2.0.
//    THIS CODE IS PROVIDED *AS IS* WITHOUT WARRANTY OF
//    ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING ANY
//    IMPLIED WARRANTIES OF FITNESS FOR A PARTICULAR
//    PURPOSE, MERCHANTABILITY, OR NON-INFRINGEMENT.
//
// *********************************************************

namespace Microsoft.Research.Dkal.Ast.Infon.Syntax.Simple

open System.IO

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Ast.Infon
open Microsoft.Research.Dkal.Ast.Syntax.Parsing
open Microsoft.Research.Dkal.Utils.Exceptions
open Microsoft.Research.Dkal.Globals

/// The SimpleParser parses from the simple concrete syntax, which uses declared 
/// typed variables. It must be initialized with a Context that holds variable 
/// type information, relation declarations, etc.
type SimpleParser() = 

  let pp = new SimplePrettyPrinter() :> IInfonPrettyPrinter

  let checkMacrosAndType (t: ITerm, solvedMacros: ISubstrateQueryTerm list) (typ: IType option) =
    if not solvedMacros.IsEmpty then
      raise(SemanticCheckException("unresolved macros", pp.PrintTerm(t))) 
    elif typ.IsSome && typ.Value <> t.Type then
      raise(SemanticCheckException(sprintf "Incorrect type, expecting %O, found %O" typ.Value.FullName t.Type.FullName, pp.PrintTerm(t))) 
    else
      t      

  let rec checkConditionSanity (c: ITerm) = 
    match c with
    | SeqCondition(cs) -> 
      let rec wireConds c = 
        match c with
        | WireCondition(_) -> [c]
        | SeqCondition(cs) -> List.collect wireConds cs
        | _ -> []
      if List.length (wireConds c) > 1 then
        raise(SemanticCheckException("Rule condition presents more than one 'upon' construct", pp.PrintTerm(c)))
      List.iter checkConditionSanity cs
    | EmptyCondition -> ()
    | WireCondition(i,p) -> ()
    | KnownCondition(i) -> ()
    | _ -> raise(SemanticCheckException("Expecting condition when checking sanity", pp.PrintTerm(c)))

  and checkActionSanity (a: ITerm) = 
    match a with
    | SeqAction(aa) -> List.iter checkActionSanity aa
    | EmptyAction -> ()
    | Send(_)
    | JustifiedSend(_) 
    | JustifiedSay(_) 
    | Learn(_) 
    | Forget(_) 
    | Apply(_) 
    | Fresh(_) 
    | Complete(_) -> ()
    | Install(r) 
    | Uninstall(r) -> checkRuleSanity(r)
    | _ -> raise(SemanticCheckException("Expecting action when checking sanity", pp.PrintTerm(a)))

  and checkRuleSanity (r: ITerm) = 
    match r with
    | SeqRule(rs) -> List.iter checkRuleSanity rs
    | EmptyRule -> ()
    | Rule(c,a,e) -> checkConditionSanity(c); checkActionSanity(a); checkRuleSanity(e)
    | Forall(_,r) -> checkRuleSanity(r)
    | Var(v) when v.Type=Type.Rule -> ()
    | _ -> raise(SemanticCheckException("Expecting rule when checking sanity", pp.PrintTerm(r)))

  and checkPolicySanity (p: Policy) =
    List.iter checkRuleSanity p.Rules
  
  interface IInfonParser with
    member sp.SetParsingContext (parsingContext: IParsingContext) = 
      Parser.ctxs.Push parsingContext

    member sp.ParseType s = 
      GeneralParser.TryParse (Parser.Type Lexer.tokenize) s 
      
    member sp.ParseTerm s = 
      let t = GeneralParser.TryParse (Parser.Term Lexer.tokenize) s 
      checkMacrosAndType t None
      
    member sp.ParseInfon s = 
      let t = GeneralParser.TryParse (Parser.Term Lexer.tokenize) s 
      checkMacrosAndType t (Some Type.Infon)
      
    member sp.ParseRule s = 
      let t = GeneralParser.TryParse (Parser.Term Lexer.tokenize) s 
      let r = checkMacrosAndType t (Some Type.Rule)
      checkRuleSanity r
      r
    
    member sp.ParsePolicy s = 
      let p = GeneralParser.TryParse (Parser.Policy Lexer.tokenize) s
      checkPolicySanity p
      p

    member sp.ParseSignature s =
      GeneralParser.TryParse (Parser.Signature Lexer.tokenize) s

    member sp.ParseAssembly s =
      let a = GeneralParser.TryParse (Parser.Assembly Lexer.tokenize) s 
      checkPolicySanity a.Policy
      a
