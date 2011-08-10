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
assume notinNil: forall (x:'a). not (In x Nil)
assume notinCons: forall (x:'a) (y:'a) (tl:list 'a). ((not (In x tl)) && (not (x=y))) => not (In x (Cons y tl))  
  
val map: ('a -> 'b) -> list 'a -> list 'b
let rec map f x = match x with 
  | Nil -> Nil
  | Cons a tl -> Cons (f a) (map f tl)

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
                                   
val assoc: 'a -> list ('a*'b) -> option 'b
let rec assoc a x = match x with
  | Nil -> None
  | Cons (a', b) tl -> if a=a' then Some b else assoc a tl

val append: x:list 'a -> y:list 'a -> list 'a 
let rec append x y = match x with
  | Nil -> y
  | Cons a tl -> Cons a (append tl y)

val concatMap: ('a -> list 'b) -> list 'a -> list 'b
let rec concatMap f = function
  | [] -> []
  | a::tl -> append (f a) (concatMap f tl)

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


extern reference SysConvert {language="C#";
                             dll="mscorlib";
                             namespace="System";
                             classname="Convert"}
extern SysConvert val ToBase64String : bytes -> string
extern SysConvert val FromBase64String : string -> bytes

extern reference Runtime { language = "F#";
                           dll="runtime";
                           namespace="Microsoft.FStar.Runtime";
                           classname="Pickler"}

type Serialized :: 'a::* => 'a => bytes => E

extern Runtime type ref :: * => *
extern Runtime val ref : 'a -> ref 'a
extern Runtime val read : ref 'a -> 'a
extern Runtime val write : ref 'a -> 'a -> unit

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
extern Runtime val strcat : string -> string -> string

extern Runtime val boxToObject: 'a -> object
extern Runtime val addBindings: object -> string -> bool
(* extern Runtime val lookupBindings: object -> option string *)
extern Runtime val clearBindings: bool -> bool

extern Runtime val Assume: 'P::E -> unit -> (y:unit{'P})
extern Runtime val PAssume: 'P::E -> int -> (y:punit{'P})
extern Runtime val pickle: x:'a -> (b:bytes{Serialized x b})
extern Runtime val unpickle: b:bytes -> (x:'a{Serialized x b})
extern Runtime val assert : 'P::E -> x:unit{'P} -> (y:unit{'P})
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
