module Prims

(* logical connectives *)
type l_and :: E => E => P
type l_or  :: E => E => P
type l_not :: E => P
type l_iff :: E => E => P
type l_implies :: E => E => P
type Forall :: 'a::* => ('a => E) => E
type Exists :: 'a::* => ('a => E) => E
type ForallA :: 'a::A => ('a => E) => E
type ExistsA :: 'a::A => ('a => E) => E
type True :: P
type False :: P

type Eq :: 'a::* => 'a => 'a => P
type Eq2 :: 'a::* => 'b::* => 'a => 'b => P
type EqA :: 'a::A => 'a => 'a => E

type NTuple =
  | Tuple_UU : 'a -> 'b -> ('a * 'b)
  | Tuple_UA : 'a::* -> 'b::A -> 'a -> 'b -> ('a * 'b) 
  | Tuple_AU : 'a::A -> 'b::* -> 'a -> 'b -> ('a * 'b)
  | Tuple_AA : 'a::A -> 'b::A -> 'a -> 'b -> ('a * 'b)

type pf  :: E => P  =
  | T                : pf True
                    -> pf (l_implies (pf 'P2) (pf 'Q2))
type object
type bool
type unit
type int
type string
type bytes
val op_Equality : x:'a -> y:'a -> z:bool { z=true <=> x=y}

val id : 'a::* -> 'a -> 'a
let id x = x

val idprop : 'a::P -> 'a -> 'a
let idprop x = x

val apply: ('a -> 'b) -> 'a -> 'b
let apply f x = f x

val idint: int -> int
let idint x = id x

type option :: * => * =
  | None : option 'a
  | Some : 'a -> option 'a

val bind_opt: ('a -> 'b) -> option 'a -> option 'b
let bind_opt f x = match x with
  | None -> None
  | Some x -> Some (f x)
  
type list :: * => * =
  | Nil : list 'a
  | Cons : 'a -> list 'a -> list 'a

type In :: 'a::* => 'a => list 'a => P
assume In_hd: forall (hd:'a) (tl:list 'a). (In hd (Cons hd tl))
assume In_tl: forall (hd:'a) (x:'a) (tl:list 'a). (In x tl) => (In x (Cons hd tl))
assume inConsInv: forall (x:'a) (y:'a) (tl:list 'a). (In x (Cons y tl)) <=> ((x=y) || (In x tl))
assume notinNil: forall (x:'a). not (In x Nil)

type Includes :: 'a::* => list 'a => list 'a => E
assume Includes_nil: forall (l:list 'a). Includes l []
assume Includes_cons: forall (l:list 'a) (l':list 'a) (x:'a). In x l && Includes l l' => Includes l (x::l') 

val contains : a:'a -> l:list 'a -> b:bool{((b=true) <=> (In 'a a l))}
let rec contains a l = match l with 
  | [] -> false
  | hd::tl -> 
      if a=hd then true
      else contains a tl

val remove_dups : list 'a -> list 'a
let rec remove_dups = function 
  | [] -> []
  | hd::tl -> 
      if contains hd tl then remove_dups tl
      else hd::(remove_dups tl)

val includes : l:list 'a -> m:list 'a -> b:bool{b=true => Includes l m}
let rec includes l m = match m with 
  | [] -> true
  | hd::tlm -> (contains hd l) && (includes l tlm)

type Disjoint :: 'a::* => list 'a => list 'a => E
assume (forall (xs:list 'a) (ys:list 'a). Disjoint xs ys <=> (forall (x:'a). In x xs => not(In x ys)))
val check_disjoint: xs:list 'a -> ys:list 'a -> b:bool{b=true <=> Disjoint 'a xs ys}
let rec check_disjoint xs ys = match xs with
  | [] -> 
      assert (forall (x:'a). not (In x xs));
      true
  | x::xtl ->
      if contains x ys 
      then false
      else 
        if check_disjoint xtl ys 
        then 
          (assert (forall (y:'a). In y xs => (x=y) || (In y xtl));
           assert (forall (y:'a). In y xtl => not(In y ys));
           assert (not (In x ys));
           true)
        else 
          (assert (forall (y:'a). In y xs => (x=y) || (In y xtl));
           assert (exists (y:'a). In y xtl && In y ys);
           assert (forall (y:'a). In y xtl => In y xs);
           false)

val map: ('a -> 'b) -> list 'a -> list 'b
let rec map f x = match x with 
  | Nil -> Nil
  | Cons a tl -> Cons (f a) (map f tl)

val for_all: 'a::* -> 'P::('a => E) 
            -> f:(x:'a -> b:bool{b=true => 'P x})
            -> l:list 'a
            -> b:bool{b=true => forall (x:'a). In x l => 'P x}
let rec for_all f = function 
  | [] -> true
  | hd::tl -> f hd && (for_all f tl)

val fold_left: ('a -> 'b -> 'a) -> 'a -> list 'b -> 'a
let rec fold_left f x y = match y with 
  | Nil -> x
  | Cons hd tl -> fold_left f (f x hd) tl

val fold_right: ('a -> 'b -> 'b) -> list 'a -> 'b -> 'b
let rec fold_right f l x = match l with
  | Nil -> x
  | Cons hd tl -> fold_right f tl (f hd x)

val iterate: ('a -> unit) -> list 'a -> unit
let rec iterate f x = match x with
  | Nil -> ()
  | Cons a tl -> let _ = f a in iterate f tl
                                   
val assoc: x:'a -> l:list ('a*'b) -> r:option 'b{((r=None) => forall (y:'b). not (In (x,y) l)) &&
                                                  (forall (y:'b). (r=(Some y)) => (In (x,y) l))}
let rec assoc x l = match l with
  | Nil -> None
  | Cons (x', y) tl -> if x=x' then Some y else assoc x tl

(* logic function Zip: (list 'a) -> (list 'b) -> (list ('a*'b)) *)
(* assume Zip_nil: (Zip (Nil<'a>) (Nil<'b>)) = (Nil<('a*'b)>) *)
(* assume Zip_cons: forall (x:'a) (xs:list 'a) (y:'b) (ys:list 'b).  *)
(*                   (Zip (x::xs) (y::ys)) = ((x,y)::(Zip xs ys)) *)

val zip: xs:list 'a -> ys:list 'b -> list ('a * 'b)
let rec zip xs ys = match xs, ys with 
  | [], [] -> []
  | (x::xtl), (y::ytl) -> (x,y)::(zip xtl ytl)

logic function Append : list 'a -> list 'a -> list 'a
assume forall (y:list 'a). Append Nil y = y
assume forall (a:'a) (tl:list 'a) (y:list 'a). 
   ((Append (Cons a tl) y) = (Cons a (Append tl y)))

logic function Union : list 'a ->  list 'a -> list 'a
assume forall (l1:list 'a) (l2:list 'a). (Union l1 l2)=(Append l1 l2)

val append: x:list 'a -> y:list 'a -> z:list 'a{z=(Append x y)} 
let rec append x y = match x with
  | Nil -> y
  | Cons a tl -> Cons a (append tl y)

logic function ConcatMap: ('a -> list 'b) -> list 'a -> list 'b
assume forall (f:'a -> list 'b). (ConcatMap f [] = [])
(*assume forall (f:'a -> list 'b) (a:'a) (tl:list 'a). (*Rk: cannot do that*)
    ((ConcatMap f (a::tl)) = (Append (f a) (ConcatMap f tl)))*)
val concatMap: ('a -> list 'b) -> list 'a -> list 'b
let rec concatMap f = function
  | [] -> []
  | a::tl -> append (f a) (concatMap f tl)

val mapSome : (x:'a -> option 'b) -> list 'a -> list 'b
(* let rec mapSome f l = match l with  *)
(*   | [] -> [] *)
(*   | hd::tl -> match f hd with  *)
(*       | None -> mapSome f tl *)
(*       | Some x -> x::(mapSome f tl) *)

extern reference String {language="C#";
                         dll="mscorlib";
                         namespace="System";
                         classname="String"}

extern String val Concat: string -> string -> string

val ConcatList: string -> list string -> string
let rec ConcatList sep l = match l with 
  | [] -> ""
  | hd::tl -> 
      let tl = ConcatList sep tl in
        if tl="" then hd
        else Concat (Concat hd sep) tl

logic function UnicodeStringToBytes: string -> bytes
extern reference SysTextUnicodeEncoding {language="C#";
                             dll="mscorlib";
                             namespace="System.Text";
                             classname="UnicodeEncoding"}
extern SysTextUnicodeEncoding val ToUnicodeString: b:bytes -> s:string{(UnicodeStringToBytes s) = b}
extern SysTextUnicodeEncoding val FromUnicodeString: s:string -> b:bytes{(UnicodeStringToBytes s) = b}
                            
extern reference SysConvert {language="C#";
                             dll="mscorlib";
                             namespace="System";
                             classname="Convert"}
logic function B64 : bytes -> string
extern SysConvert val ToBase64String : b:bytes -> s:string{(B64 b)=s}
extern SysConvert val FromBase64String : s:string -> b:bytes{(B64 b)=s}

extern reference Runtime { language = "F#";
                           dll="runtime";
                           namespace="Microsoft.FStar.Runtime";
                           classname="Pickler"}

type Serialized :: 'a::* => 'a => bytes => E

extern Runtime type Ref :: * => *
extern Runtime val newref : 'a -> Ref 'a
extern Runtime val read : Ref 'a -> 'a
extern Runtime val write : Ref 'a -> 'a -> unit

extern Runtime type StreamReader :: *
extern Runtime val StreamReaderCtor : string -> StreamReader
extern Runtime val StreamReaderReadLine : StreamReader -> string

extern Runtime type punit :: P
extern Runtime val freshname : bool -> string
extern Runtime val debug_println : string -> bool
extern Runtime val println : string -> bool
extern Runtime val printfile: string -> string -> bool
extern Runtime val printfileNoLn: string -> string -> bool
extern Runtime val print_stderr : string -> bool
extern Runtime val print_string : string -> bool
extern Runtime val string_of_any : 'a -> string
extern Runtime val string_of_any_for_coq : 'a -> string
extern Runtime val string_of_any_for_coq_afn : 'a -> string
extern Runtime val writeToFile : string -> 'a -> string
extern Runtime val writeCertToFile : string -> 'a -> string
extern Runtime val print_int : int -> bool

logic function Strcat : string -> string -> string
assume forall (str:string). ((Strcat str "") = str)
assume forall (str:string). ((Strcat "" str) = str)
assume forall (s1:string) (s2:string) (s3:string).
  ((Strcat s1 (Strcat s2 s3)) = (Strcat (Strcat s1 s2) s3))
assume forall (s1:string) (s2:string) (s:string).
  ((s1=s2) => (Strcat s1 s) = (Strcat s2 s))
assume forall (s1:string) (s2:string) (s:string).
  ((s1=s2) => (Strcat s s1) = (Strcat s s2))
logic function ReprInt: int -> string
extern Runtime val strcat : s1:string -> s2:string -> r:string{(Strcat s1 s2) = r}
extern Runtime val strStartsWith: string -> string -> bool
extern Runtime val intToString: n:int -> s:string{s=(ReprInt n)}
extern Runtime val stringToInt: s:string -> n:int{s=(ReprInt n)}
extern Runtime val strRmPfx: s:string -> pfx:string -> r:string{s=(Strcat pfx r)}
extern Runtime val strSplitByDelimiter: s:string -> d:string -> (r1:string*r2:string{(Strcat r1 r2)=s})
extern Runtime val intCheckRange: int -> int -> int -> bool

extern Runtime val createComm: int -> ((bool -> bytes) * (int -> bytes -> bool))
extern Runtime val stopAllServers: bool -> bool

extern Runtime val boxToObject: 'a -> object
extern Runtime val addBindings: object -> string -> bool
(* extern Runtime val lookupBindings: object -> option string *)
extern Runtime val clearBindings: bool -> bool

extern Runtime val Assume: 'P::E -> unit -> (y:bool{'P})
extern Runtime val PAssume: 'P::E -> int -> (y:punit{'P})
extern Runtime val pickle: x:'a -> (b:bytes{Serialized x b})
extern Runtime val unpickle: b:bytes -> (x:'a{Serialized x b})
extern Runtime val Assert : 'P::E -> x:unit{'P} -> (y:bool{'P})
extern Runtime val throw: string -> 'a 

val loop : unit -> 'a
let rec loop x = loop x

val raise: string -> 'a
let raise s = 
  println "Exception!";
  throw s

val failwith : string -> 'a
let failwith s = raise (Concat "Failure: " s)
  
val option_get : option 'a -> 'a (* Rk: this declaration is necessary to not have a type error *)
let option_get (opt: option 'a) : 'a = match opt with
  | Some(a) -> a
  | _ -> failwith "option_get error: The option value was None"

val option_map : ('a -> 'b) -> option 'a -> option 'b
let option_map (f: 'a -> 'b) (opt: option 'a) (*: option 'b*) = match opt with (* Rk: type annotation leads to type error *)
  | None -> None
  | Some(a) -> Some(f a)

val ignore : 'a -> unit      
let ignore a = ()

(*let collect f l = fold_right (fun acc -> acc) []*) (* Rk: no type error?? *)
val collect : ('a -> list 'b) -> list 'a -> list 'b (* Rk: need of val declaration otherwise compiler raises exception *)
let collect (f: 'a -> list 'b) (l : list 'a) : list 'b =
  fold_right (fun a acc -> append (f a) acc) l []

val collect_in : l:list 'a -> (x:'a{In x l} -> list 'b) -> list 'b 
let rec collect_in l f = match l with 
  | [] -> []
  | hd::tl -> append (f hd) (collect_in tl f)

val coll : l:list 'a -> (x:'a -> list 'b) -> list 'b 
let coll l f = collect f l

val product : list 'a -> list 'b -> ('a -> 'b -> list 'c) -> list 'c
let product la lb f = 
  collect (fun a -> collect (fun b -> f a b) lb) la

val filter : ('a -> bool) -> list 'a -> list 'a
let filter (f: 'a -> bool) (l : list 'a) =
  fold_right (fun a acc -> if (f a) then a :: acc else acc) l []
    
val List_exists : ('a -> bool) -> list 'a -> bool
let List_exists (f: 'a -> bool) (l : list 'a) =
  fold_left (fun acc a -> acc || (f a)) false l
    
val List_forall : ('a -> bool) -> list 'a -> bool
let List_forall (f: 'a -> bool) (l : list 'a) =
  fold_left (fun acc a -> acc && (f a)) true l
   
val length : (list 'a) -> int
let length (l : list 'a) =
  fold_left (fun acc a -> acc+1) 0 l

val isEmpty : (list 'a -> bool)
let isEmpty l = match l with Nil -> true | _ -> false

(* Primitive functions with trusted specs for concrete refinements *)
val _dummy_op_AmpAmp             : x:bool -> y:bool -> z:bool { z=true =>  x=true &&  y=true}
val _dummy_op_BarBar             : x:bool -> y:bool -> z:bool { (z=true => x=true ||  y=true) && 
                                                                 (z=false => x=false && y=false)}
val _dummy_op_Multiply           : int -> int -> int
val _dummy_op_Division           : int -> x:int{x<>0} -> int
val _dummy_op_Subtraction        : int -> int -> int
val _dummy_op_Addition           : int -> int -> int
val _dummy_op_GreaterThanOrEqual : int -> int -> bool
val _dummy_op_Negation           : x:bool -> y:bool { (y=true => x=false) && (y=false => x=true)}

