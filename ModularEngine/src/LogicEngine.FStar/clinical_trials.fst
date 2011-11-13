module ClinicalTrials
open Types
open Interp
open Net

(* --------- Borrowed from prog.fst --------- *)
type TrustRule :: _ = 
    (fun (xs:vars) (cs:conditions) (a:action) => 
        (forall (subst:substitution). Holds xs cs subst => (Enabled (ActionSubst a subst))))

let mkRule xs cs acts = 
  let acts = map (fun a -> assume (TrustRule xs cs a);
                    (a:(a:action{TrustRule xs cs a}))) acts in
    Rule xs cs acts

(* --------- Type for integer intervals substrate queries --------- *)
type intervalSubstrateQuery = ISubstrateQueryTerm

val mkIntervalSubstrateQuery : term -> term -> term -> intervalSubstrateQuery
let mkIntervalSubstrateQuery = mkSubstrateQuery
 
(* --------- Shorthands to construct infons --------- *)
let said ppal infon = App SaidInfon [ppal; infon]
let And i1 i2 = App AndInfon [i1; i2]   (* and is an F* keyword *)
let implies i1 i2 = App ImpliesInfon [i1; i2]
let just i e = App JustifiedInfon [i; e]
let asInfon isq = App AsInfon [(SubstrateQueryTerm isq)]

(* --------- Shorthands to construct infon relations --------- *)
let rel relInf terms = (App (RelationInfon relInf)) terms
let ri name argsTyp = { fname=name; retType=Infon; argsType=argsTyp; identity=None }

(* --------- Infon relations used in the clinical trials scenario --------- *)
let participates site trial = rel (ri "participates" [Principal; Int32]) [site; trial]
let allocatedPatients site n1 n2 trial = rel (ri "allocatedPatients" [Principal; Int32; Int32; Int32]) [site; n1; n2; trial]
let mayRead ppal record = rel (ri "mayRead" [Principal; Int32]) [ppal; record]
let physParticipates phys trial site = rel (ri "physParticipates" [Principal; Int32; Principal]) [phys; trial; site]
let physAllocatedPatients phys n1 n2 trial site = rel (ri "physAllocatedPatients" [Principal; Int32; Int32; Int32; Principal]) [phys; n1; n2; trial; site]
let requestToRead ppal record = rel (ri "requestToRead" [Principal; Int32]) [ppal; record]
let keyForRecord key record = rel (ri "keyForRecord" [Int32; Int32]) [key; record]

(* --------- org1 --------- *)
let org1Policy = 

  let me = Const (PrincipalConstant "org1") in
  let trial = Const (SubstrateConstant (Int 42)) in
  let site = Const (PrincipalConstant "site1") in
  let site_n1 = Const (SubstrateConstant (Int 1000)) in
  let site_n2 = Const (SubstrateConstant (Int 1250)) in

  let rule1 = 
    (* quantified vars *)
    let p = {name="p"; typ=Principal} in
    let r = {name="r"; typ=Int32} in
  
    (* actions *)
    let a1 = Send site (MonoTerm (said me (participates site trial))) in
    let a2 = Send site (MonoTerm (said me (allocatedPatients site site_n1 site_n2 trial))) in
    let a3 = Send site (ForallT [p; r] (implies (And (said site (mayRead (Var p) (Var r))) (asInfon(mkIntervalSubstrateQuery site_n1 (Var r) site_n2))) (said me (mayRead (Var p) (Var r))))) in
    let actions = [a1; a2; a3] in
  
      mkRule [] [] actions in

  [rule1]

(* --------- site1 --------- *)
let site1Policy = 

  let me = Const (PrincipalConstant "site1") in
  let trial = Const (SubstrateConstant (Int 42)) in
  let org = Const (PrincipalConstant "org1") in
  let phys = Const (PrincipalConstant "phys1") in
  let phys_n1 = Const (SubstrateConstant (Int 1010)) in
  let phys_n2 = Const (SubstrateConstant (Int 1050)) in

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
    let c1 = Upon (MonoTerm (just (said org (participates me trial)) (Var e1))) in
    let c2 = Upon (MonoTerm (just (said org (allocatedPatients me (Var n1) (Var n2) trial)) (Var e2))) in
    let c3 = Upon (MonoTerm (just (implies (Var x1) (Var x2)) (Var e3))) in
    let conds = [c1; c2; c3] in
  
    (* quantified vars *)
    let r = { name="r"; typ=Int32 } in
  
    (* actions *)
    let a1 = Send phys (MonoTerm (said me (physParticipates phys trial me))) in
    let a2 = Send phys (MonoTerm (said me (physAllocatedPatients phys phys_n1 phys_n2 trial me))) in
    let a3 = Send phys (ForallT [r] (implies (asInfon(mkIntervalSubstrateQuery phys_n1 (Var r) phys_n2)) (said me (mayRead phys (Var r))))) in
    let a4 = Fwd phys (MonoTerm (just (implies (Var x1) (Var x2)) (Var e3))) in
    let actions = [a1; a2; a3; a4] in
  
      mkRule vars conds actions in

  [rule1]

(* --------- phys1 --------- *)
let phys1Policy = 

  let me = Const (PrincipalConstant "phys1") in
  let trial = Const (SubstrateConstant (Int 42)) in
  let org = Const (PrincipalConstant "org1") in
  let site = Const (PrincipalConstant "site1") in
  let keyMgr = Const (PrincipalConstant "keyMgr") in
  let r = Const (SubstrateConstant (Int 1015)) in

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
    let c1 = Upon (MonoTerm (just (said site (physParticipates me trial site)) (Var e1))) in
    let c2 = Upon (MonoTerm (just (said site (physAllocatedPatients me (Var n1) (Var n2) trial site)) (Var e2))) in
    let c3 = Upon (MonoTerm (just (implies (Var x1) (Var x2)) (Var e3))) in
    let c4 = Upon (MonoTerm (just (implies (Var y1) (Var y2)) (Var e4))) in
    let conds = [c1; c2; c3; c4] in
    
    (* actions *)
    let a1 = Send keyMgr (MonoTerm (said me (requestToRead me r))) in
    let a2 = Fwd keyMgr (MonoTerm (just (implies (Var x1) (Var x2)) (Var e3))) in
    let a3 = Fwd keyMgr (MonoTerm (just (implies (Var y1) (Var y2)) (Var e4))) in
    let a4 = Drop (MonoTerm (just (said site (physParticipates me trial site)) (Var e1))) in
    let actions = [a1; a2; a3; a4] in
    
      mkRule vars conds actions in

  [rule1]

(* --------- keyMgr --------- *)
let keyMgrPolicy = 

  let me = Const (PrincipalConstant "keyMgr") in
  let org = Const (PrincipalConstant "org1") in
  let r = Const (SubstrateConstant (Int 1015)) in
  let k = Const (SubstrateConstant (Int 13131313)) in
  
  let rule1 = 
    (* unification vars *)
    let i = { name="i"; typ=Infon } in
    let e1 = { name="e1"; typ=Evidence } in
    let vars = [i; e1] in
  
    (* conditions *)
    let c1 = Upon (MonoTerm (just (Var i) (Var e1))) in
    let conds = [c1] in
    
    (* actions *)
    let a1 = Learn (MonoTerm (just (Var i) (Var e1))) in
    let actions = [a1] in
    
      mkRule vars conds actions in

  let rule2 = 
    (* unification vars *)
    let e2 = { name="e2"; typ=Evidence } in
    let e3 = { name="e3"; typ=Evidence } in
    let p = { name="p"; typ=Principal } in
    let vars = [e2; e3; p] in
  
    (* conditions *)
    let c1 = Upon (MonoTerm (just (said (Var p) (requestToRead (Var p) r)) (Var e2))) in
    let c2 = If (MonoTerm (just (said org (mayRead (Var p) r)) (Var e3))) in
    let conds = [c1] in
    
    (* actions *)
    let a1 = Send (Var p) (MonoTerm (said me (keyForRecord k r))) in
    let a2 = Drop (MonoTerm (just (said (Var p) (requestToRead (Var p) r)) (Var e2))) in
    let actions = [a1] in
    
      mkRule vars conds actions in

  [rule1; rule2]
     
(* ---------------- for communication ------------------ *)
let _ = 
  let portNum_org = 9200 in
  let portNum_site = 9201 in
  let portNum_phys = 9202 in
  let portNum_keyMgr = 9203 in
  let org = "org1" in
  let site = "site1" in
  let phys =  "phys1" in
  let keyMgr = "keyMgr" in
  let org_recv, send_to_org = createComm portNum_org in
  let site_recv, send_to_site = createComm portNum_site in
  let phys_recv, send_to_phys = createComm portNum_phys in
  let keyMgr_recv, send_to_keyMgr = createComm portNum_keyMgr in
  (suscribe [(org, org_recv, send_to_org);
            (site, site_recv, send_to_site);
            (phys, phys_recv, send_to_phys);
            (keyMgr, keyMgr_recv, send_to_keyMgr)];
  Keys.addKey org;
  Keys.addKey site;
  Keys.addKey phys;
  Keys.addKey keyMgr)