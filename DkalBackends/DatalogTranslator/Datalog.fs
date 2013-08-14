namespace Microsoft.Research.DkalBackends.DatalogBackend.DatalogTranslator

open System.Collections.Generic

module Datalog =
  type Term = 
  | VarTerm of string
  | AtomTerm of string
  | WildcardTerm
  with 
    override t.ToString() =
      match t with
      | VarTerm(s) -> s
      | AtomTerm(s) -> "\"" + s + "\""
      | WildcardTerm -> "_"
    member t.ToProlog() = 
      match t with
      | VarTerm(s) -> s.ToUpper()
      | AtomTerm(s) -> "'" + s + "'"
      | WildcardTerm -> "_"

  let manyArgs i v = List.mapi (fun i e -> VarTerm(e+i.ToString())) (List.replicate i v)

  type Relation(name: string, args: Term seq) = 
    member r.Name = name
    member r.Args = args
    override r.ToString() = 
      r.Name + "(" + (String.concat "," (Seq.map (fun t -> t.ToString()) r.Args)) + ")"

  type Rule =
  | AtomRule of Relation
  | ImpliesRule of Relation * Relation list
  with
    override dr.ToString() = 
      match dr with
      | AtomRule(r) -> r.ToString() + "."
      | ImpliesRule(r, rs) -> r.ToString() + " :- " + 
                                (String.concat ", " (List.map (fun r' -> r'.ToString()) rs)) + "."

  type Sort = string

  type SortDeclaration(name: Sort, numElems: int, ?mappingFile: string) =
    member sd.Name = name
    member sd.NumElems = numElems
    member sd.MappingFile = match mappingFile with 
                            | Some f -> f
                            | None -> ""
    override sd.ToString() = sd.Name + " " + sd.NumElems.ToString() + " " + sd.MappingFile
    
  type RelationAttribute = Input | PrintTuples
    with 
      override ra.ToString() = 
        match ra with
        | Input -> "input"
        | PrintTuples -> "printtuples"

  type RelationDeclaration(name: string, args: (string * Sort) seq, attribute: RelationAttribute) = 
    member rd.Name = name
    member rd.Args = args
    member rd.Attribute = attribute
    override rd.ToString() = rd.Name + "(" + (String.concat ", " (Seq.map (fun (name, sort) -> name + ": " + sort) args)) + ") " + rd.Attribute.ToString()

  type ProgramRulePart = 
  | RulePart of Rule
  | NewLineRulePart
  | CommentRulePart of string
  with 
    override pp.ToString() = 
      match pp with
      | RulePart(r) -> r.ToString() + "\r\n"
      | NewLineRulePart -> "\r\n"
      | CommentRulePart(s) -> "# " + s + "\r\n"

  type ProgramDeclarationPart = 
  | SortDeclarationPart of SortDeclaration
  | RelationDeclarationPart of RelationDeclaration
  | NewLineDeclarationPart
  | CommentDeclarationPart of string
  with 
    override pp.ToString() = 
      match pp with
      | SortDeclarationPart(s) -> s.ToString() + "\r\n"
      | RelationDeclarationPart(r) -> r.ToString() + "\r\n"
      | NewLineDeclarationPart -> "\r\n"
      | CommentDeclarationPart(s) -> "# " + s + "\r\n"


  type Program() = 
    let declParts = new List<ProgramDeclarationPart>()
    let ruleParts = new List<ProgramRulePart>()

    member p.Declarations with get() = declParts
    member p.Rules with get() = ruleParts

    member p.AddDeclarationPart(dp: ProgramDeclarationPart) = declParts.Add(dp)
    member p.AddRulePart(rp: ProgramRulePart) = ruleParts.Add(rp)
    override p.ToString() = String.concat "" (Seq.map (fun (dp: ProgramDeclarationPart) -> dp.ToString()) declParts) + "\r\n" + 
                              String.concat "" (Seq.map (fun (rp: ProgramRulePart) -> rp.ToString()) ruleParts)
