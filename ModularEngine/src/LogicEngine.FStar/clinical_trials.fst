module ClinicalTrials
open TypeHeaders
open Types
open Interp

(* --------- Borrowed from prog.fst --------- *)
type TrustRule :: _ = 
    (fun (xs:vars) (cs:conditions) (acts:actions) => 
        (forall (a:action) (subst:substitution). In a acts && Holds xs cs subst => (Enabled (ActionSubst a subst))))

let mkRule xs cs acts = 
  assume (TrustRule xs cs acts);
  Rule xs cs acts

(* --------- Type for integer intervals substrate queries --------- *)
type intervalSubstrateQuery = 
    { lowerBound: term; elem: term; upperBound: term }  
    
(* This substrate works only when the three elements are integer constants.
   It returns true iff lowerBound <= elem && elem <= upperBound *)
let evalIntervalSubstrateQuery isq =
    match isq.lowerBound, isq.elem, isq.upperBound with
    | Const l, Const e, Const u -> true  (* XXX: I could not convert l, e, u to Int32 and check (l <= e && e <= u) *)
    | _ -> failwith "expecting constants"

(* --------- Shorthands to construct infons --------- *)
let said ppal infon = App SaidInfon [ppal; infon]
let And i1 i2 = App AndInfon [i1; i2]   (* and is an F* keyword *)
let implies i1 i2 = App ImpliesInfon [i1; i2]
let just i e = App JustifiedInfon [i; e]
let asInfon (isq:intervalSubstrateQuery) = App AsInfon [isq]

(* --------- Shorthands to construct infon relations --------- *)
let rel relInf terms = (App (RelationInfon relInf)) terms
let ri name argsTyp = { name=name; retType=Infon; argsType=argsTyp; identity=None }

(* --------- Infon relations used in the clinical trials scenario --------- *)
let participates site trial = rel (ri "participates" [Principal; Int32]) [site; trial]
let allocatedPatients site n1 n2 trial = (ri "allocatedPatients" [Principal; Int32; Int32; Int32]) [site; n1; n2; trial]
let mayRead ppal record = (ri "mayRead" [Principal; Int32]) [ppal; record]
let physParticipates phys trial site = rel (ri "physParticipates" [Principal; Int32; Principal]) [phys; trial; site]
let physAllocatedPatients phys n1 n2 trial site = (ri "physAllocatedPatients" [Principal; Int32; Int32; Int32; Principal]) [phys; n1; n2; trial; site]
let requestToRead ppal record = (ri "requestToRead" [Principal; Int32]) [ppal; record]
let keyForRecord key record = (ri "keyForRecord" [Int32; Int32]) [key; record]

(* --------- org1 --------- *)
let org1Policy = 

  let me = Const "org1" in
  let trial = Const 42 in
  let site = Const "site1" in
  let site_n1 = Const 1000 in
  let site_n2 = Const 1250 in

  let rule1 = 
    (* quantified vars *)
    let p = {name="p"; typ=Principal} in
    let r = {name="r"; typ=Int32} in
  
    (* actions *)
    let a1 = Send site (said me (participates site trial)) in
    let a2 = Send site (said me (allocatedPatients site site_n1 site_n2 trial)) in
    let a3 = Send site (ForallT [p; r] (implies (And (said site (mayRead (Var p) (Var r))) (asInfon({lowerBound=site_n1; elem=(Var r); upperBound=site_n2}))) (said me (mayRead (Var p) (Var r))))) in
    let actions = [a1; a2; a3] in
  
      mkRule [] [] actions in

  [rule1]

(* --------- site1 --------- *)
let site1Policy = 

  let me = Const "site1" in
  let trial = Const 42 in
  let org = Const "org1" in
  let phys = Const "phys1" in
  let phys_n1 = Const 1010 in
  let phys_n2 = Const 1050 in

  let rule1 = 
    (* unification vars *)
    let e1 = { name="e1"; typ=Evidence } in
    let e2 = { name="e2"; typ=Evidence } in
    let e3 = { name="e3"; typ=Evidence } in
    let n1 = { name="n1"; typ=Int32 } in
    let n2 = { name="n2"; typ=Int32 } in
    let x1 = { name="x1"; typ=Infon } in
    let x2 = { name="x2"; typ=Infon } in
    let vars = [e1; e2; e3; n1; n2; x1; x2] in
    
    (* conditions *)
    let c1 = Upon (just (said org (participates me trial)) (Var e1)) in
    let c2 = Upon (just (said org (allocatedPatients me (Var n1) (Var n2) trial)) (Var e2)) in
    let c3 = Upon (just (implies (Var x1) (Var x2)) (Var e3)) in
    let conds = [c1; c2; c3] in
  
    (* quantified vars *)
    let r = { name="r"; typ=Int32 } in
  
    (* actions *)
    let a1 = Send phys (said me (physParticipates phys trial me)) in
    let a2 = Send phys (said me (physAllocatedPatients phys phys_n1 phys_n2 trial me)) in
    let a3 = Send phys (ForallT [r] (implies (asInfon({lowerBound=phys_n1; elem=r; upperBound=phys_n2})) (said me (mayRead phys r)))) in
    let a4 = Fwd phys (just (implies (Var x1) (Var x2)) (Var e3)) in
    let actions = [a1; a2; a3; a4] in
  
      mkRule vars conds actions in

  [rule1]

(* --------- phys1 --------- *)
let phys1Policy = 

  let me = Const "phys1" in
  let trial = Const 42 in
  let org = Const "org1" in
  let site = Const "site1" in
  let keyMgr = Const "keyMgr" in
  let r = Const 1015 in

  let rule1 = 
    (* unification vars *)
    let e1 = { name="e1"; typ=Evidence } in
    let e2 = { name="e2"; typ=Evidence } in
    let e3 = { name="e3"; typ=Evidence } in
    let e4 = { name="e4"; typ=Evidence } in
    let n1 = { name="n1"; typ=Int32 } in
    let n2 = { name="n2"; typ=Int32 } in
    let x1 = { name="x1"; typ=Infon } in
    let x2 = { name="x2"; typ=Infon } in
    let y1 = { name="y1"; typ=Infon } in
    let y2 = { name="y2"; typ=Infon } in
    let vars = [e1; e2; e3; e4; n1; n2; x1; x2; y1; y2] in
    
    (* conditions *)
    let c1 = Upon (just (said site (physParticipates me trial site)) (Var e1)) in
    let c2 = Upon (just (said site (physAllocatedPatients me (Var n1) (Var n2) trial site)) (Var e2)) in
    let c3 = Upon (just (implies (Var x1) (Var x2)) (Var e3)) in
    let c4 = Upon (just (implies (Var y1) (Var y2)) (Var e4)) in
    let conds = [c1; c2; c3; c4] in
    
    (* actions *)
    let a1 = Send keyMgr (said me (requestToRead me r)) in
    let a2 = Fwd keyMgr (just (implies (Var x1) (Var x2)) (Var e3)) in
    let a3 = Fwd keyMgr (just (implies (Var y1) (Var y2)) (Var e4)) in
    let a4 = Drop (just (said site (physParticipates me trial site)) (Var e1)) in
    let actions = [a1; a2; a3; a4] in
    
      mkRule vars conds actions in

  [rule1]

(* --------- keyMgr --------- *)
let phys1Policy = 

  let me = Const "keyMgr" in
  let org = Const "org1" in
  let r = Const 1015 in
  let k = Const 13131313 in
  
  let rule1 = 
    (* unification vars *)
    let i = { name="i"; typ=Infon } in
    let e1 = { name="e1"; typ=Evidence } in
    let vars = [i; e1] in
  
    (* conditions *)
    let c1 = Upon (just (Var i) (Var e1)) in
    let conds = [c1] in
    
    (* actions *)
    let a1 = Learn (just (Var i) (Var e1)) in
    let actions = [a1] in
    
      mkRule vars conds actions in

  let rule2 = 
    (* unification vars *)
    let e2 = { name="e2"; typ=Evidence } in
    let e3 = { name="e3"; typ=Evidence } in
    let p = { name="p"; typ=Principal } in
    let vars = [e2; e3; p] in
  
    (* conditions *)
    let c1 = Upon (just (said (Var p) (requestToRead (Var p) r)) (Var e2)) in
    let c2 = If (just (said org (mayRead (Var p) r)) (Var e3)) in
    let conds = [c1] in
    
    (* actions *)
    let a1 = Send (Var p) (said me (keyForRecord k r)) in
    let a2 = Drop (just (said (Var p) (requestToRead (Var p) r)) (Var e2)) in
    let actions = [a1] in
    
      mkRule vars conds actions in

  [rule1; rule2]
                     