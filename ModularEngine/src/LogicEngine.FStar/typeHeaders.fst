module TypeHeaders

  extern reference Generics {language="F#";
                             dll="mscorlib";
                             namespace="System.Collections";
                             classname="Generic"}
  extern Generics type Dictionary :: * => * => *
  extern Generics type HashSet :: * => *

  extern reference ISignatureProvider {language="F#";
                                       dll="Interfaces";
                                       namespace="";
                                       classname="Microsoft.Research.Dkal.Interfaces"}
  extern ISignatureProvider type ISignatureProvider :: *

  extern reference IInfostrate {language="F#";
                                dll="Interfaces";
                                namespace="";
                                classname="Microsoft.Research.Dkal.Interfaces"}
  extern IInfostrate type IInfostrate :: *

  extern reference ISubstrateQueryTerm {language="F#";
                                        dll="Interfaces";
                                        namespace="";
                                        classname="Microsoft.Research.Dkal.Interfaces"}
  extern ISubstrateQueryTerm type ISubstrateQueryTerm :: *
  type substrate

  (* TODO: Implement this external function *)
  type SubstrateSays :: substrate => ISubstrateQueryTerm => E
  extern ISubstrateQueryTerm val check_substrate: s:substrate
                                               -> q:ISubstrateQueryTerm 
                                               -> b:bool{b=true => SubstrateSays s q}

  extern reference ISubstrateUpdateTerm {language="F#";
                                         dll="Interfaces";
                                         namespace="";
                                         classname="Microsoft.Research.Dkal.Interfaces"}
  extern ISubstrateUpdateTerm type ISubstrateUpdateTerm :: *


  extern reference IType {language="F#";
                          dll="Interfaces";
                          namespace="";
                          classname="Microsoft.Research.Dkal.Interfaces"}
  extern IType type IType :: *

  extern reference Variable {language="F#";
                             dll="Ast";
                             namespace="";
                             classname="Microsoft.Research.Dkal.Ast"}
  extern Variable type Variable :: *

  extern reference Function {language="F#";
                             dll="Ast.Tree";
                             namespace="";
                             classname="Microsoft.Research.Dkal.Ast.Tree"}
  extern Function type Function :: *

  extern reference ITerm {language="F#";
                          dll="Interfaces";
                          namespace="";
                          classname="Microsoft.Research.Dkal.Interfaces"}
  extern ITerm type ITerm :: *

  extern reference IVar {language="F#";
                         dll="Interfaces";
                         namespace="";
                         classname="Microsoft.Research.Dkal.Interfaces"}
  extern IVar type IVar :: *

  extern reference ISubstitution {language="F#";
                                  dll="Interfaces";
                                  namespace="";
                                  classname="Microsoft.Research.Dkal.Interfaces"}
  extern ISubstitution type ISubstitution :: *

  extern reference Substitution {language="F#";
                                 dll="Ast";
                                 namespace="Microsoft.Research.Dkal.Ast";
                                 classname="Substitution"}
  extern Substitution val Id : ISubstitution


  extern reference Application {language="F#";
                                dll="Ast.Tree";
                                namespace="Microsoft.Research.Dkal.Ast.Tree";
                                classname="Application"}
  extern Application type Application


  extern reference Builders  {language="F#";
                              dll="LogicEngine.FStar.Deps";
                              namespace="Microsoft.Research.Dkal.LogicEngine.FStar.Deps";
                              classname="Builders"}

  extern Builders val SeqRuleStr: bool -> string
  extern Builders val EmptyRule: bool -> string 
  extern Builders val RuleF: bool -> string  
  extern Builders val RuleOnce: bool -> string  
  extern Builders val SeqConditionStr: bool -> string  
  extern Builders val EmptyCondition: bool -> string  
  extern Builders val WireCondition: bool -> string  
  extern Builders val KnownCondition: bool -> string  
  extern Builders val SeqActionStr: bool -> string  
  extern Builders val EmptyAction: bool -> string  
  extern Builders val Send: bool -> string  
  extern Builders val JustifiedSend: bool -> string  
  extern Builders val JustifiedSay: bool -> string  
  extern Builders val Learn: bool -> string 
  extern Builders val Forget: bool -> string 
  extern Builders val Install: bool -> string 
  extern Builders val Uninstall: bool -> string 
  extern Builders val Apply: bool -> string 
  extern Builders val Drop: bool -> string 
  extern Builders val EmptyInfon: bool -> string 
  extern Builders val AsInfon: bool -> string 
  extern Builders val And: bool -> string 
  extern Builders val Implies: bool -> string 
  extern Builders val Said: bool -> string 
  extern Builders val Justified: bool -> string 
  extern Builders val EvEmpty: bool -> string 
  extern Builders val EvSignature: bool -> string 
  extern Builders val EvModusPonens: bool -> string 
  extern Builders val EvAnd: bool -> string 
  extern Builders val EvAsInfon: bool -> string
  (* from Ast / Builders.fs *)
  extern Builders val Constant_ITerm : object -> ITerm
  extern Builders val Constant_ITerm_bool : bool -> ITerm
  extern Builders val PrincipalConstant_ITerm : string -> ITerm
  (* from Ast.Infon / Builders.fs *)
  extern Builders val Application_ITerm : Application -> ITerm
  extern Builders val substrateUpdateTerm_ITerm : ISubstrateUpdateTerm -> ITerm
  extern Builders val substrateQueryTerm_ITerm : ISubstrateQueryTerm -> ITerm
  extern Builders val explicitSubstitutionTerm_ITerm : ITerm -> ISubstitution -> ITerm
  (* for making records *)
  extern Builders val mkIVar : string -> IType -> IVar
  extern Builders val mkFunction : string -> IType -> list IType -> option ITerm -> Function
  extern Builders val mkApplication : Function -> list ITerm -> Application
  (* moved from BuildersAst with a different name *)
  extern Builders val ITermOfIVar : IVar -> ITerm
  extern Builders val SubstExtend : ISubstitution -> (IVar * ITerm) -> ISubstitution
  (* from Ast / Type.fs *)
  extern Builders val Infon : bool ->  IType
  extern Builders val Principal : bool ->  IType
  extern Builders val SubstrateUpdate : bool ->  IType
  extern Builders val SubstrateQuery : bool ->  IType
  extern Builders val Action: bool ->  IType
  extern Builders val Condition: bool ->  IType
  extern Builders val Rule: bool ->  IType
  extern Builders val Evidence: bool ->  IType
  extern Builders val Boolean: bool ->  IType
  extern Builders val Int32: bool ->  IType
  extern Builders val Double: bool ->  IType
  extern Builders val String: bool ->  IType

  extern Builders val HashSet_new : list 'a -> HashSet 'a
  extern Builders val HashSet_remove : HashSet 'a -> 'a -> bool
  extern Builders val HashSet_toList : HashSet 'a -> list 'a
  extern Builders val HashSet_contains : HashSet 'a -> 'a -> bool
  extern Builders val HashSet_unionWith : HashSet 'a -> list 'a -> unit
  extern Builders val HashSet_intersectWith : HashSet 'a -> list 'a -> unit
  extern Builders val HashSet_exceptWith : HashSet 'a -> list 'a -> unit

  extern Builders val String_StartsWith : string -> string -> bool
  extern Builders val String_Substring : string -> int -> string
  extern Builders val String_Length : string -> int
  extern Builders val System_Int32_Parse : string -> int
  extern Builders val isEmpty : list 'a -> bool
  extern Builders val max : list 'a -> 'a
  extern Builders val int_to_string : int -> string
  extern Builders val nth : list 'a -> int -> 'a
  extern Builders val lessThan : int -> int -> bool

  extern Builders type listFS :: * => *
  extern Builders val NilFS : bool -> listFS 'a
  extern Builders val ConsFS : 'a -> listFS 'a -> listFS 'a
  extern Builders val PrimsListOfList : listFS 'a -> list 'a
  val ListOfPrimsList : list 'a -> listFS 'a
  let rec ListOfPrimsList (l:list 'a) : listFS 'a = 
    match l with
    | Nil -> NilFS false
    | Cons(h, t) -> ConsFS h (ListOfPrimsList t) 

  extern Builders type optionFS :: * => *
  extern Builders val NoneFS : bool -> optionFS 'a
  extern Builders val SomeFS : 'a -> optionFS 'a
  extern Builders val PrimsOptionOfOption : optionFS 'a -> option 'a
  val OptionOfPrimsOption : option 'a -> optionFS 'a (* Rk: val declaration needed *)
  let OptionOfPrimsOption (o:option 'a) : (optionFS 'a) = 
    match o with
    | None -> (NoneFS false : optionFS 'a)
    | Some a -> SomeFS a

  extern Builders type tupleFS :: * => * => *
  extern Builders val PrimsTupleOfTuple : tupleFS 'a 'b -> ('a * 'b)

  extern Builders val _substrateQueryTerm_Vars : ISubstrateQueryTerm -> listFS IVar
  extern Builders val _substrateUpdateTerm_Vars : ISubstrateUpdateTerm -> listFS IVar
  extern Builders val _substrateQueryTerm_boundVars : ISubstrateQueryTerm -> listFS IVar
  extern Builders val _substrateUpdateTerm_boundVars : ISubstrateUpdateTerm -> listFS IVar

  val substrateQueryTerm_Vars : ISubstrateQueryTerm -> list IVar
  let substrateQueryTerm_Vars i = PrimsListOfList (_substrateQueryTerm_Vars i)
  val substrateUpdateTerm_Vars : ISubstrateUpdateTerm -> list IVar
	let substrateUpdateTerm_Vars is = PrimsListOfList (_substrateUpdateTerm_Vars is)
  val substrateQueryTerm_boundVars : ISubstrateQueryTerm -> list IVar
  let substrateQueryTerm_boundVars is = PrimsListOfList (_substrateQueryTerm_boundVars is)
  val substrateUpdateTerm_boundVars : ISubstrateUpdateTerm -> list IVar
	let substrateUpdateTerm_boundVars is = PrimsListOfList (_substrateUpdateTerm_boundVars is)

  extern Builders val substrateQueryTerm_apply: ISubstrateQueryTerm -> ISubstitution -> ITerm
  extern Builders val substrateUpdateTerm_apply: ISubstrateUpdateTerm -> ISubstitution -> ITerm

  extern Builders val _substrateQueryTerm_unifyFrom: ISubstrateQueryTerm -> ISubstitution -> ITerm -> optionFS ISubstitution
  extern Builders val _substrateUpdateTerm_unifyFrom: ISubstrateUpdateTerm -> ISubstitution -> ITerm -> optionFS ISubstitution

  val substrateQueryTerm_unifyFrom: ISubstrateQueryTerm -> ISubstitution -> ITerm -> option ISubstitution
  let substrateQueryTerm_unifyFrom is s t = PrimsOptionOfOption (_substrateQueryTerm_unifyFrom is s t)
  val substrateUpdateTerm_unifyFrom: ISubstrateUpdateTerm -> ISubstitution -> ITerm -> option ISubstitution
  let substrateUpdateTerm_unifyFrom is s t = PrimsOptionOfOption (_substrateUpdateTerm_unifyFrom is s t)

  extern Builders val domainContains : Dictionary 'a 'b -> 'a -> bool
  extern Builders val subst_apply_def : Dictionary 'a 'b -> 'a -> 'b -> 'b
  extern Builders val emptySubst : bool -> Dictionary 'a 'b
  extern Builders val copy : Dictionary 'a 'b -> Dictionary 'a 'b
  extern Builders val assign : Dictionary 'a 'b -> 'a -> 'b -> bool 
  extern Builders val extend : Dictionary 'a 'b -> 'a -> 'b -> Dictionary 'a 'b

  extern Builders val _domain : Dictionary 'a 'b -> listFS 'a
  val domain : Dictionary 'a 'b -> list 'a 
  let domain s = PrimsListOfList (_domain s)

  extern Builders val _restrictTo : Dictionary 'a 'b -> listFS 'a -> Dictionary 'a 'b
  val restrictTo : Dictionary 'a 'b -> list 'a -> Dictionary 'a 'b
  let restrictTo s l = _restrictTo s (ListOfPrimsList l) 

  extern Builders val _forget : Dictionary 'a 'b -> listFS 'a -> Dictionary 'a 'b
  val forget : Dictionary 'a 'b -> list 'a -> Dictionary 'a 'b
  let forget s l = _forget s (ListOfPrimsList l) 

  extern reference Primitives {language="F#";
                               dll="Ast.Infon"; (* bin\Debug\Ast.Infon.dll *)
                               namespace="Microsoft.Research.Dkal.Ast.Infon";
                               classname="Primitives"}
  extern Primitives val SolveFunction: string -> optionFS Function
  val SolveFunctionW : string -> option Function
  let SolveFunctionW s = PrimsOptionOfOption (SolveFunction s)

  extern reference BuildersAst {language="F#";
                                dll="Ast"; (* bin\Debug\Ast.dll *)
                                namespace="Microsoft.Research.Dkal.Ast";
                                classname="Builders"}
  (* extern BuildersAst val Var : IVar -> ITerm *) (* Rk: moved to Builders.fs because of name conficts *)
  extern BuildersAst val ForallT : (IVar * ITerm) -> ITerm

  extern reference BuildersAstInfon {language="F#";
                             dll="Ast.Infon"; (* bin\Debug\Ast.Infon.dll *)
                             namespace="Microsoft.Research.Dkal.Ast.Infon";
                             classname="Builders"}
  extern BuildersAstInfon val SeqRule : listFS ITerm -> ITerm
  extern BuildersAstInfon val SeqCondition : listFS ITerm -> ITerm
  extern BuildersAstInfon val SeqAction : listFS ITerm -> ITerm
  extern BuildersAstInfon val AndInfon : listFS ITerm -> ITerm
  extern BuildersAstInfon val AndEvidence : listFS ITerm -> ITerm

  val SeqRuleW : list ITerm -> ITerm
	let SeqRuleW l = SeqRule (ListOfPrimsList l)
  val SeqConditionW : list ITerm -> ITerm
	let SeqConditionW l = SeqCondition (ListOfPrimsList l)
  val SeqActionW : list ITerm -> ITerm
	let SeqActionW l = SeqAction (ListOfPrimsList l)
  val AndInfonW : list ITerm -> ITerm
	let AndInfonW l = AndInfon (ListOfPrimsList l)
  val AndEvidenceW : list ITerm -> ITerm
	let AndEvidenceW l = AndEvidence (ListOfPrimsList l)
