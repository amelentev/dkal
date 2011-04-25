namespace Microsoft.Research.Dkal.Ast.SimpleSyntax

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Utils.PrettyPrinting
open Microsoft.Research.Dkal.Substrate

open System.Collections.Generic

/// The SimplePrettyPrinter prints AST elements into the simple concrete syntax,
/// which uses declared typed variables
type SimplePrettyPrinter() =
  let substrates = new Dictionary<ITerm, string>()
    
  interface IAstPrettyPrinter with
    member spp.PrintType (t: IType) =
      match t with
      | :? Type as t ->
        match t with
        | SubstrateElem(t) when t = typeof<int> -> "int"
        | SubstrateElem(t) when t = typeof<float> -> "float"
        | SubstrateElem(t) when t = typeof<string> -> "string"
        | Sequence(t') -> "seq<" + spp.PrintType t' + ">"
        | Tuple(t1, t2) -> spp.PrintType t1 + " * " + spp.PrintType t2
        | t -> t.ToString().ToLower()
      | _ -> failwith <| "Unknown type implementation when printing"

    member spp.PrintTerm t =
      PrettyPrinter.PrettyPrint <| spp.TokenizeTerm t

    member spp.PrintPolicy p =
      PrettyPrinter.PrettyPrint <| spp.TokenizePolicy p

    member spp.PrintSignature s =
      PrettyPrinter.PrettyPrint <| spp.TokenizeSignature s

    member spp.PrintAssembly a =
      PrettyPrinter.PrettyPrint <| spp.TokenizeAssembly a

  member private spp.PrintType t = (spp :> IPrettyPrinter).PrintType t
  member private spp.PrintTerm mt = (spp :> IPrettyPrinter).PrintTerm mt

  static member FindFunctionSymbol f = 
    match f with
    | "and" -> "&&", true
    | "implies" -> "->", true
    | "said" -> "said", true
    | "or" -> "||", true
    | "not" -> "!", false
    | "eq" -> "==", true
    | "neq" -> "!=", true
    | "lt" -> "<", true
    | "lte" -> "<=", true
    | "gt" -> ">", true
    | "gte" -> ">=", true
    | "plus" -> "+", true
    | "minus" -> "-", true
    | "times" -> "*", true
    | "div" -> "/", true
    | "uminus" -> "-", false
    | "nil" -> "[]", false
    | "cons" -> "::", true
    | f -> f, false

  member private spp.TokenizeTerm mt =
    match mt with
    | App(f, mts) -> 
      let fSymbol, infix = SimplePrettyPrinter.FindFunctionSymbol f.Name
      let args = List.map spp.TokenizeTerm mts
      if infix then
        [ TextToken "(" ]
        @ List.reduce (fun t1 t2 -> t1 @ [TextToken <| " " + fSymbol + " "] @ t2) args
        @ [ TextToken ")" ]
      elif fSymbol = "asInfon" then
        match mts with
        | [exp] -> 
          [ TextToken <| f.Name + "(";
            ManyTokens args.[0] ]
            @ [ TextToken <| ")" ]
        | _ -> failwith "Incorrect arguments in AsInfon(...)"
      elif not infix && mts = [] then
        [ TextToken <| fSymbol ]
      elif fSymbol = "emptyInfon" then
        [ TextToken <| "asInfon(true)" ]
      elif fSymbol = "rule" then
        let vars = mt.Vars |> Seq.toList
        let varsDecl = List.map (fun (v: IVar) -> v.Name + ": " + spp.PrintType v.Type) vars
        let beginVars, endVars =  if varsDecl.Length > 0 then
                                    [ TextToken <| "with " + (String.concat ", " varsDecl);
                                      TabToken;
                                      NewLineToken ], [UntabToken]
                                  else
                                    [], []
        let mainTokens = 
          match mts.[0], mts.[1], mts.[2] with
          | EmptyInfon, EmptyInfon, App(f, [mt']) when 
            f.Name = "learn" ->
            [ TextToken <| "me knows"; 
              TabToken; NewLineToken;
              ManyTokens <| spp.TokenizeTerm mt'
              UntabToken ]
          | _, EmptyInfon, _ ->
            [ TextToken <| "if me knows"; 
              TabToken; NewLineToken;
              ManyTokens <| spp.TokenizeTerm mts.[0]
              UntabToken; NewLineToken;
              TextToken <| "then";
              TabToken; NewLineToken;
              ManyTokens <| spp.TokenizeTerm mts.[2]
              UntabToken]
          | EmptyInfon, _, _ ->
            [ TextToken <| "if wire has"; 
              TabToken; NewLineToken;
              ManyTokens <| spp.TokenizeTerm mts.[1]
              UntabToken; NewLineToken;
              TextToken <| "then";
              TabToken; NewLineToken;
              ManyTokens <| spp.TokenizeTerm mts.[2]
              UntabToken]
          | _ -> 
              [ TextToken <| "if me knows"; 
                TabToken; NewLineToken;
                ManyTokens <| spp.TokenizeTerm mts.[0]
                UntabToken; NewLineToken;
                TextToken <| "wire has"; 
                TabToken; NewLineToken;
                ManyTokens <| spp.TokenizeTerm mts.[1]
                UntabToken; NewLineToken;
                TextToken <| "then";
                TabToken; NewLineToken;
                ManyTokens <| spp.TokenizeTerm mts.[2]
                UntabToken]
        beginVars @ mainTokens @ endVars
      elif fSymbol = "send" then
        [ TextToken <| "send to " + spp.PrintTerm mts.[0];
          TabToken; NewLineToken;
          ManyTokens <| spp.TokenizeTerm mts.[1];
          UntabToken ]
      elif fSymbol = "learn" then
        [ TextToken <| "learn";
          TabToken; NewLineToken;
          ManyTokens <| spp.TokenizeTerm mts.[0];
          UntabToken ]
      elif fSymbol = "seq" then
        [ ManyTokens <| spp.TokenizeTerm mts.[0];
          TextToken ";"; NewLineToken;
          ManyTokens <| spp.TokenizeTerm mts.[1] ]
      else
        [ TextToken <| fSymbol + "(" ]
        @ List.reduce (fun t1 t2 -> t1 @ [TextToken ", "] @ t2) args
        @ [ TextToken ")"]
    | Var(v) -> [TextToken v.Name]
    | Const(c) -> 
      match c with
      | BoolConstant(b) -> [TextToken(b.ToString().ToLower())]
      | PrincipalConstant(p) -> [TextToken(p.ToString())]
      | SubstrateElemConstant(o) when o.GetType() = typeof<string> -> [TextToken("\"" + o.ToString() + "\"")]
      | SubstrateElemConstant(o) -> [TextToken(o.ToString())]
    | :? DummySubstrateTerm as t -> // TODO: use some SubstratePrettyPrinter
      spp.TokenizeTerm t.query
    | _ -> failwith <| "PrettyPrinter does not know how to print ITerm"
   
  member private spp.TokenizePolicy (p: Policy) =
    List.collect (fun a -> spp.TokenizeTerm a @ [ NewLineToken; NewLineToken ]) p.Rules

  member private spp.TokenizeSignature (s: Signature) =
    List.collect (fun sd -> spp.TokenizeSubstrateDeclaration sd @ [ NewLineToken; NewLineToken ]) s.Substrates
      @ List.collect (fun td -> spp.TokenizeTableDeclaration td @ [ NewLineToken; NewLineToken ]) s.Tables
      @ List.collect (fun rd -> spp.TokenizeRelationDeclaration rd @ [ NewLineToken; NewLineToken ]) s.Relations

  member private spp.TokenizeSubstrateDeclaration (sd: SubstrateDeclaration) =
    substrates.[sd.Decl] <- sd.Name
    let kind, args =  match sd.Decl with
                      | Sql(Const(SubstrateElemConstant(arg))) -> "sql", "\"" + arg.ToString() + "\""
                      | Xml(Const(SubstrateElemConstant(arg))) -> "xml", "\"" + arg.ToString() + "\""
                      | _ -> failwith <| "Unrecognized substrate type"
    let namespaces = String.concat ", " [] // TODO
    [TextToken <| "substrate " + sd.Name + " = " + kind + "(" + args + ") namespaces " + namespaces]

  member private spp.TokenizeTableDeclaration (td: TableDeclaration) =
    [TextToken <| "table " + td.Name + "(";
      TextToken <| String.concat ", " (List.map (fun (v: Variable) -> v.Name + ": " + spp.PrintType v.Type) td.Cols);
      TextToken ")" ]

  member private spp.TokenizeRelationDeclaration (rd: RelationDeclaration) =
    [TextToken <| "relation " + rd.Name + "(";
      TextToken <| String.concat ", " (List.map (fun (v: Variable) -> v.Name + ": " + spp.PrintType v.Type) rd.Args);
      TextToken ")" ]

  member private spp.TokenizeAssembly (a: Assembly) =
    let ret = spp.TokenizeSignature a.Signature
                @ spp.TokenizePolicy a.Policy
    substrates.Clear()
    ret