module CoreDKAL

type principal = string
type var = string 

type constant = 
  | Bool : bool -> constant 
  | Principal : principal -> constant
  | SubstrateObject : object -> constant 

type typ =  
    | Boolean : typ
    | Infon : typ
    | PrincipalT : typ
    | SubstrateQuery : typ
    | Evidence : typ

type query 
type evidence =
  | EmptyEv : evidence
  | SigEv : evidence
  | MPEv : evidence
  | AndEv : evidence
  | AsInfonEv : evidence

type infon =
  | Variable : var -> infon
(*   | c  *)
(*                | forall (x:t). i  *)
  | Empty : infon
  | AsInfon : query -> infon
  | And : infon -> infon -> infon
  | Said : infon -> infon -> infon
(*                | And i j *)
(*                | Implies i j *)

(*                | Justified i j *)


type infostrate = list infon
type substrate = list query


type In :: 'a::* => 'a => list 'a => P (* Inductive definition of list membership in a constructive style *)

type MkPrefix :: list infon => infon => infon => P =
  | MkPrefix_Nil : i:infon -> MkPrefix [] i i
  | MkPrefix_Cons : p:infon -> ps:list infon -> i:infon -> i':infon 
                   -> MkPrefix ps i i'
                   -> MkPrefix (p::ps) i (Said p i')

type entails :: substrate => infostrate => infon => P =
  | Entails_Emp : S:substrate -> I:infostrate -> entails S I Empty

  | Entails_Hyp_Substrate : S:substrate -> I:infostrate -> q:query 
                 -> In q S  (* Eventually, we get a signature from the DB about this fact *)
                 -> entails S I (AsInfon q)

  | Entails_And_Intro : S:substrate -> I:infostrate 
                     -> i:infon -> i':infon
                     -> j:infon -> j':infon 
                     -> result:infon
                     -> pref: list infon
                     -> MkPrefix pref (And i j) result
                     -> MkPrefix pref i i'
                     -> MkPrefix pref j j'
                     -> entails S I i' 
                     -> entails S I j' 
                     -> entails S I result

  | Entails_And_Elim1 : S:substrate -> I:infostrate
     -> i : infon -> i': infon
     -> j : infon
     -> pref : list infon 
     -> hyp : infon
     -> MkPrefix pref (And i j) hyp
     -> MkPrefix pref i i'
     -> entails S I hyp
     -> entails S I i'


val checkSubstrate : S:substrate -> q:query -> option (In q S)
(* Left unimplemented for now *)

type prefix = list infon

val mkPrefix: pref:prefix -> i:infon -> (i':infon * dummy:(MkPrefix pref i i'){pref=[] => i=i'})
let rec mkPrefix pref i =
  (match pref with
     | [] -> (i, MkPrefix_Nil i)
     | h::t -> let (j, m) = mkPrefix t i in
	             ((Said h j), MkPrefix_Cons h t i j m))

val doDerive: S:substrate -> I:infostrate -> pref:prefix -> i:infon 
           -> option (i':infon * MkPrefix pref i i' * entails S I i')
let rec doDerive _S _I pref i = match i with 
  | Empty -> 
      (match pref with 
         | [] -> let (j, m) = mkPrefix pref i in
		 (* if unable to make F* prove sth, put in a runtime check *)
			     (* if j = Empty then *)
                   Some((j, m, Entails_Emp _S _I))
				  (* else raise "stupid F*" *)
         | _ -> None)

  | AsInfon q -> 
      (match pref with
         | [] -> let (j, m) = mkPrefix pref i in
             (match checkSubstrate _S q with 
                | None -> None
                | Some pf -> Some ((j, m, Entails_Hyp_Substrate _S _I q pf)))
         | _ -> None)

  | And i j -> None (*Some((,,))*)
      
