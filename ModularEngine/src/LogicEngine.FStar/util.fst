module Util
open Types

(*************)
(* Utilities *)
(*************)

type Mem :: 'a::* => 'a => list 'a => P =
  | Mem_hd : a:'a -> t:list 'a -> Mem 'a a (a::t)
  | Mem_tl : a:'a -> h:'a -> t:list 'a -> Mem 'a a t -> Mem 'a a (h::t)

val mem : a:'a -> l:list 'a -> option (Mem 'a a l)
let rec mem (a : 'a) (l: list 'a) : option (Mem 'a a l) =
  match l with
  | [] -> None
  | h::t when h=a -> Some(Mem_hd a t)
  | h::t -> (match mem a t with
               | None -> None
               | Some(m) -> Some(Mem_tl a h t m))
			   
val fold_left_dep : res:'a -> l:list 'b -> ('a -> x:'b -> Mem x l -> 'a) -> 'a
let rec fold_left_dep res l f = match l with
  | [] -> res
  | h::t -> fold_left_dep (f res h (Mem_hd h t)) t
                          (fun res x m -> f res x (Mem_tl x h t m))

val map_one : l:list 'a -> (x:'a{In x l} -> option 'b) -> option 'b
let rec map_one l f = match l with
  | [] -> None
  | h::t -> (match f h with 
               | None -> map_one t f 
               | res -> res)

val collect_dep : l:list 'a -> (x:'a -> Mem x l -> list 'b) -> list 'b
let rec collect_dep l f =
  fold_left_dep [] l (fun res x m -> append res (f x m))

val option_map : ('a -> 'b) 
              -> option 'a 
              -> option 'b
let option_map f = function
  | None -> None
  | Some a -> Some (f a)

(********** Zip functions taken from coretyping.fst *************)
(* Move this to Util? *)
type MapL :: 'a::* => ('a => P) => list 'a => P =
   | MapL_Nil: 
        'a::*
     -> 'Q::('a => P)
     -> MapL 'a 'Q []

   | MapL_Cons: 
        'a::* 
     -> 'Q::('a => P)
     -> t:list 'a
     -> h:'a
     -> MapL 'a 'Q t
     -> 'Q h
     -> MapL 'a 'Q (h::t)

type ForallL :: _ = (fun ('a::*) (xs:list 'a) ('P::'a => P) => (MapL 'a 'P xs ))
			  
val mapL_p: 'a::*
        -> 'Q::('a => P)
        -> f:(x:'a -> option ('Q x))
        -> l:list 'a
        -> option (MapL 'a 'Q l)
let rec mapL_p f l = match l with
  | [] -> Some (MapL_Nil<'a,'Q>)
  | h::t -> 
     match mapL_p<'a,'Q> f t, f h with
	   | Some pf_tl, Some pf_hd ->
	       Some (MapL_Cons<'a,'Q> t h pf_tl pf_hd)
	   | _ -> None

type ZipE :: 'a::* => 'b::* => ('a => 'b => E) => list 'a => list 'b => P = 
   | ZipE_Nil : 'a::* -> 'b::* -> 'Q::('a => 'b => E) 
             -> ZipE 'a 'b 'Q [] []
   | ZipE_Cons: 'a::* -> 'b::* -> 'Q::('a => 'b => E)
             -> l1:list 'a -> l2:list 'b 
             -> x:'a -> y:'b{'Q x y}
             -> ZipE 'a 'b 'Q l1 l2
             -> ZipE 'a 'b 'Q (x::l1) (Cons<'b> y l2)


type ZipP :: 'a::* => 'b::* => ('a => 'b => P) => list 'a => list 'b => P = 
   | ZipP_Nil : 'a::* -> 'b::* -> 'Q::('a => 'b => P) 
             -> ZipP 'a 'b 'Q [] []
   | ZipP_Cons: 'a::* -> 'b::* -> 'Q::('a => 'b => P)
             -> l1:list 'a -> l2:list 'b 
             -> x:'a -> y:'b 
             -> ZipP 'a 'b 'Q l1 l2
             -> 'Q x y
             -> ZipP 'a 'b 'Q (x::l1) (y::l2)

val zip_p: 'a::* -> 'b::* -> 'Q::('a => 'b => P)
      -> f:(x:'a -> y:'b -> option ('Q x y))
      -> l1:list 'a
      -> l2:list 'b
      -> option (ZipP 'a 'b 'Q l1 l2)
let rec zip_p f l1 l2 = match l1, l2 with
  | [],[] -> Some (ZipP_Nil<'a,'b,'Q>)
  | (x1::tl1), (x2::tl2) ->
      match zip_p<'a,'b,'Q> f tl1 tl2, f x1 x2 with
        | (Some pf_tl), Some pf_hd -> Some (ZipP_Cons<'a,'b,'Q> tl1 tl2 x1 x2 pf_tl pf_hd)
        | _ -> None

val map_p: 'a::* -> 'b::* -> 'Q::('a => 'b => P)
      -> f:(x:'a -> (y:'b * 'Q x y))
      -> l1:list 'a
      -> (l2:list 'b * ZipP 'a 'b 'Q l1 l2)
let rec map_p f l1 = match l1 with
  | [] -> [], ZipP_Nil<'a,'b,'Q>
  | x::tlx ->
      let y, pfHd = f x in
      let tly, pfTl = map_p<'a,'b,'Q> f tlx in
        (y::tly), ZipP_Cons<'a,'b,'Q> tlx tly x y pfTl pfHd
        
val map_p_opt: 'a::* -> 'b::* -> 'Q::('a => 'b => P)
      -> f:(x:'a -> option (y:'b * 'Q x y))
      -> l1:list 'a
      -> option (l2:list 'b * ZipP 'a 'b 'Q l1 l2)
let rec map_p_opt f l1 = match l1 with
  | [] -> Some(([], ZipP_Nil<'a,'b,'Q>))
  | x::tlx ->
      match f x, map_p_opt<'a,'b,'Q> f tlx with
        | Some((y, pfHd)), Some((tly, pfTl)) ->
            Some(((y::tly), ZipP_Cons<'a,'b,'Q> tlx tly x y pfTl pfHd))
        | _ -> None

val map_mapL_p: 'a::* -> 'b::* 
            -> 'Q::('a => 'b => P)
			-> 'R::('b => P)
			-> f:(x:'a -> option(x':'b * 'Q x x' * 'R x'))
			-> l:list 'a
			-> option (l':list 'b * ZipP 'a 'b 'Q l l' * MapL 'b 'R l')
let rec map_mapL_p f l = match l with
  | [] -> Some(([], ZipP_Nil, MapL_Nil))
  | h::t -> 
     (match f h, map_mapL_p f t with
	    | Some((h', qh, rh)), Some((t', qt, rt)) -> 
            Some(((h'::t'), (* need parenthesis around the list; precedence of :: and , *)
			      ZipP_Cons<'a, 'b, 'Q> t t' h h' qt qh,
                  MapL_Cons<'b, 'R> t' h' rt rh))
	    | _ -> None)

type Partial :: P => * = 
  | MkPartial : 'a::P -> 'a -> Partial 'a

logic function lconcat : list 'a -> list 'a -> list 'a
assume (forall (x:list 'a). (lconcat [] x) = x)
assume (forall (hd:'a) (tl:list 'a) (x:list 'a). (lconcat (hd::tl) x) = (hd::(lconcat tl x)))

val concat: l1:list 'a -> l2:list 'a -> l3:list 'a{l3=(lconcat l1 l2)}
let rec concat l1 l2 = match l1 with 
  | [] -> l2
  | hd::tl -> hd::(concat tl l2)
