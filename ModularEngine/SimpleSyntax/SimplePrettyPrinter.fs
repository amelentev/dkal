namespace Microsoft.Research.Dkal.SimpleSyntax

open Microsoft.Research.Dkal.Interfaces
open Microsoft.Research.Dkal.Ast
open Microsoft.Research.Dkal.Utils.PrettyPrinting

open System.Collections.Generic

/// The SimplePrettyPrinter prints AST elements into the simple concrete syntax,
/// which uses declared typed variables
type SimplePrettyPrinter() =
  let substrates = new Dictionary<MetaTerm, string>()
    
  interface IPrettyPrinter with
    member spp.PrintType t =
      match t with
      | SubstrateElem(t) when t = typeof<int> -> "int"
      | SubstrateElem(t) when t = typeof<float> -> "float"
      | SubstrateElem(t) when t = typeof<string> -> "string"
      | t -> t.ToString().ToLower()

    member spp.PrintMetaTerm mt =
      PrettyPrinter.PrettyPrint <| spp.TokenizeMetaTerm mt

    member spp.PrintPolicy p =
      PrettyPrinter.PrettyPrint <| spp.TokenizePolicy p

    member spp.PrintSignature s =
      PrettyPrinter.PrettyPrint <| spp.TokenizeSignature s

    member spp.PrintAssembly a =
      PrettyPrinter.PrettyPrint <| spp.TokenizeAssembly a

  member private spp.PrintType t = (spp :> IPrettyPrinter).PrintType t
  member private spp.PrintMetaTerm mt = (spp :> IPrettyPrinter).PrintMetaTerm mt

  static member FindFunctionSymbol f = 
    match f with
    | "andInfon" 
    | "andBool" -> "&&", true
    | "impliesInfon" -> "->", true
    | "saidInfon" -> "said", true
    | "orBool" -> "||", true
    | "notBool" -> "!", false
    | "eqBool" | "eqInt32" | "eqDouble" | "eqString" | "eqPrincipal" -> "==", true
    | "neqBool" | "neqInt32" | "neqDouble" | "neqString" | "neqPrincipal" -> "!=", true
    | "ltInt32" | "ltDouble" -> "<", true
    | "lteInt32" | "lteDouble" -> "<=", true
    | "gltInt32" | "gtDouble" -> ">", true
    | "gteInt32" | "gteDouble" -> ">=", true
    | "plusInt32" | "plusDouble" | "plusString" -> "+", true
    | "minusInt32" | "minusDouble" -> "-", true
    | "timesInt32" | "timesDouble" -> "*", true
    | "divInt32" | "divDouble" -> "/", true
    | "uminusInt32" | "uminusDouble" -> "-", false
    | f -> f, false

  member private spp.TokenizeMetaTerm mt =
    match mt with
    | App(f, mts) -> 
      let fSymbol, infix = SimplePrettyPrinter.FindFunctionSymbol f.Name
      let args = List.map spp.TokenizeMetaTerm mts
      if infix then
        [ TextToken "(" ]
        @ List.reduce (fun t1 t2 -> t1 @ [TextToken <| " " + fSymbol + " "] @ t2) args
        @ [ TextToken ")" ]
      elif fSymbol = "asInfon" then
        match mts with
        | [exp; substrate] -> 
          let found, name = substrates.TryGetValue substrate
          let substrateName = 
            if found && name <> "Default" then 
              [ TextToken <| ", " + name ]
            elif found then
              []
            else
              [ TextToken <| ", " ]
                @ spp.TokenizeMetaTerm substrate
          [ TextToken <| f.Name + "("; 
            ManyTokens args.[0] ]
            @ substrateName
            @ [ TextToken <| ")" ]
        | _ -> failwith "Incorrect arguments in AsInfon(...)"
      elif fSymbol.Contains "." then
        [ TextToken <| fSymbol ]
      elif fSymbol = "emptyInfon" then
        [ TextToken <| "asInfon(true)" ]
      elif fSymbol = "rule" then
        let vars = mt.Vars |> Seq.toList
        let varsDecl = List.map (fun (v: Variable) -> v.Name + ": " + spp.PrintType v.Typ) vars
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
              ManyTokens <| spp.TokenizeMetaTerm mt'
              UntabToken ]
          | _, EmptyInfon, _ ->
            [ TextToken <| "if me knows"; 
              TabToken; NewLineToken;
              ManyTokens <| spp.TokenizeMetaTerm mts.[0]
              UntabToken; NewLineToken;
              TextToken <| "then";
              TabToken; NewLineToken;
              ManyTokens <| spp.TokenizeMetaTerm mts.[2]
              UntabToken]
          | EmptyInfon, _, _ ->
            [ TextToken <| "if wire has"; 
              TabToken; NewLineToken;
              ManyTokens <| spp.TokenizeMetaTerm mts.[1]
              UntabToken; NewLineToken;
              TextToken <| "then";
              TabToken; NewLineToken;
              ManyTokens <| spp.TokenizeMetaTerm mts.[2]
              UntabToken]
          | _ -> 
              [ TextToken <| "if me knows"; 
                TabToken; NewLineToken;
                ManyTokens <| spp.TokenizeMetaTerm mts.[0]
                UntabToken; NewLineToken;
                TextToken <| "wire has"; 
                TabToken; NewLineToken;
                ManyTokens <| spp.TokenizeMetaTerm mts.[1]
                UntabToken; NewLineToken;
                TextToken <| "then";
                TabToken; NewLineToken;
                ManyTokens <| spp.TokenizeMetaTerm mts.[2]
                UntabToken]
        beginVars @ mainTokens @ endVars
      elif fSymbol = "send" then
        [ TextToken <| "send to " + spp.PrintMetaTerm mts.[0];
          TabToken; NewLineToken;
          ManyTokens <| spp.TokenizeMetaTerm mts.[1];
          UntabToken ]
      elif fSymbol = "learn" then
        [ TextToken <| "learn";
          TabToken; NewLineToken;
          ManyTokens <| spp.TokenizeMetaTerm mts.[0];
          UntabToken ]
      elif fSymbol = "seq" then
        [ ManyTokens <| spp.TokenizeMetaTerm mts.[0];
          TextToken ";"; NewLineToken;
          ManyTokens <| spp.TokenizeMetaTerm mts.[1] ]
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
   
  member private spp.TokenizePolicy (p: Policy) =
    List.collect (fun a -> spp.TokenizeMetaTerm a @ [ NewLineToken; NewLineToken ]) p.Rules

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
    [TextToken <| "substrate " + sd.Name + " = " + kind + "(" + args + ")"]

  member private spp.TokenizeTableDeclaration (td: TableDeclaration) =
    [TextToken <| "table " + td.Name + "(";
      TextToken <| String.concat ", " (List.map (fun (v: Variable) -> v.Name + ": " + spp.PrintType v.Typ) td.Cols);
      TextToken ")" ]

  member private spp.TokenizeRelationDeclaration (rd: RelationDeclaration) =
    [TextToken <| "relation " + rd.Name + "(";
      TextToken <| String.concat ", " (List.map (fun (v: Variable) -> v.Name + ": " + spp.PrintType v.Typ) rd.Args);
      TextToken ")" ]

  member private spp.TokenizeAssembly (a: Assembly) =
    let ret = spp.TokenizeSignature a.Signature
                @ spp.TokenizePolicy a.Policy
    substrates.Clear()
    ret