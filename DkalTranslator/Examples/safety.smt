;This version does not work with explicitely build sets at the left of the "entail" relationship whith "said" and "put" on the right, but when using DKAL constructions, this is not really neccesary.
(benchmark dkalEncoding
:logic Empty

:extrasorts (Set)
:extrasorts (Infon)
:extrasorts (Principal)

;Predicates and functions for Set (of Infons)
:extrapreds ((member Infon Set))
:extrafuns ((emptyset Set) (insert Infon Set Set) )

;Predicates and functions for the primal infon system
:extrapreds ((entails Set Infon))
:extrafuns ((trueInfon Infon) (plus Infon Infon Infon) (imp Infon Infon Infon) (said Principal Infon Infon) (put Principal Infon Infon) 
			(asInfon bool Infon))

;shortcuts for trusted on saying and trusted on putting
:extrafuns ((tdonS Principal Infon Infon) (tdonP Principal Infon Infon))
:assumption (forall (p Principal) (x Infon) (= (tdonS p x) (imp (said p x) x))
	:pat{(tdonS p x)}
	:pat{(said p x)}
	)
:assumption (forall (p Principal) (x Infon) (= (tdonP p x) (imp (put p x) x))
	:pat{(tdonP p x)}
	:pat{(put p x)}
	)

;;; Set axioms
:assumption (forall (x Infon) (s Set) (member x (insert x s))
	:pat { (insert x s)} 
)

:assumption (forall (x Infon) (y Infon) (s Set) (implies (not (= x y)) (iff (member x (insert y s)) (member x s) ))
	:pat { (member x (insert y s)) }
)

;Behavior of asInfon
:assumption (forall (p bool) (iff p (= (asInfon p) trueInfon)))

;;; Infon axioms
; True
:assumption (forall (gamma Set)(entails gamma trueInfon))
; x2x
:assumption (forall (x Infon) (gamma Set) (implies (member x gamma)(entails gamma x))
	:pat { (member x gamma) }
;	:pat {(entails gamma x)}
)

;;; Primal Infon inference rules

;Premise Inflation
:assumption (forall (x Infon) (y Infon) (gamma Set) (implies (entails gamma y) (entails (insert x gamma) y))
    :pat { (entails(insert x gamma) y) }
)

;+I
:assumption (forall (x Infon) (y Infon) (gamma Set) (implies (and (entails gamma x) (entails gamma y)) (entails gamma (plus x y)))
	:pat { (entails gamma (plus x y)) }
)

;+E
:assumption (forall (x Infon) (y Infon) (gamma Set) (implies (entails gamma (plus x y)) (entails gamma x))
	:pat { (entails gamma (plus x y)) }
)
:assumption (forall (x Infon) (y Infon) (gamma Set) (implies (entails gamma (plus x y)) (entails gamma y))
	:pat { (entails gamma (plus x y)) }
)

;->E
:assumption (forall (x Infon) (y Infon) (gamma Set) (implies (and (entails gamma x) (entails gamma (imp x y))) (entails gamma y)) 
  ;:pat{(entails gamma x) (imp x y)}
  :pat{(entails gamma y) (imp x y)}
  ;:pat{(entails gamma x) (entails gamma y)}
)

;These two rules are for the primal infon fragment
;->IW
;:assumption (forall (x Infon) (y Infon) (gamma Set) (implies (entails gamma y) (entails gamma (imp x y)))
	;:pat { (entails gamma (imp x y)) }	
;)
;Trans
;:assumption (forall (x Infon) (y Infon) (gamma Set) (implies (and (entails gamma x) (entails (insert x gamma) y)) (entails gamma y))
	;:pat { (entails (insert x gamma) y) }
;)

;->I (the full infon logic rule)
:assumption (forall (x Infon) (y Infon) (gamma Set) (implies (entails (insert x gamma) y) (entails gamma (imp x y)))
	:pat { (entails gamma (imp x y)) }	
	:pat{ (entails (insert x gamma) y)}
)


;Rule S
:extrafuns ((extractSaidSet Set Set))
:assumption (forall (x Infon) (p Principal) (gamma Set) (iff (member (said p x) gamma) (member x (extractSaidSet gamma)) )
	:pat{ (member (said p x) gamma) }
)
:assumption (forall (p Principal) (gamma Set) (x Infon) 
			(implies (entails (extractSaidSet gamma) x) (entails gamma (said p x)))
			:pat{(entails gamma (said p x))}
			)

;Rule P
:extrafuns ((extractPutSet Set Set))
:assumption (forall (x Infon) (p Principal) (gamma Set) (iff (member (said p x) gamma) (member x (extractPutSet gamma)) )
	:pat{ (member (said p x) gamma) }
)
:assumption (forall (x Infon) (p Principal) (gamma Set) (iff (member (put p x) gamma) (member x (extractPutSet gamma)) )
	:pat{ (member (put p x) gamma) }
)
:assumption (forall (p Principal) (gamma Set) (x Infon) 
			(implies (entails (extractPutSet gamma) x) (entails gamma (put p x)))
			:pat{(entails gamma (put p x))}
			)

;--------------------------------------------
;DKAL logic
; The syntax for communication assertion is:
;
; to p1 p2 (provisoFree x)   			means  p1 to p2: [x]
; to p1 p2 (provisoFreeCond x y)   		means  p1 to p2: [x] <= y
; to p1 p2 (provisoPresent x y)   		means  p1 to p2: [x <- y]
; to p1 p2 (provisoPresentCond x y z)   means  p1 to p2: [x <- y] <= z
;
; and equivalently for "from".
:extrapreds ((knows Principal Infon))
:extrafuns ((provisoFree Infon Infon) (provisoFreeCond Infon Infon Infon) (provisoPresent Infon Infon Infon) (provisoPresentCond Infon Infon Infon Infon))
:extrapreds ((to Principal Principal Infon) (from Principal Principal Infon))
:extrapreds ((dummy Infon))

:assumption (dummy trueInfon)

:assumption (forall (x Infon) (= (provisoFree x) (provisoFreeCond x trueInfon)))
:assumption (forall (x Infon) (y Infon) (= (provisoPresent x y) (provisoPresentCond x y trueInfon)))


;Com1 Rule (proviso-free)
:assumption (forall (p1 Principal) (p2 Principal) (x Infon) (y Infon) (z Infon) 
			(implies (and (and (and (to p1 p2 (provisoFreeCond x y)) (from p2 p1 (provisoFreeCond x z))) (knows p1 y)) (knows p2 z)) (knows p2 (said p1 x)))
			:pat{(to p1 p2 (provisoFreeCond x y)) (from p2 p1 (provisoFreeCond x z))}
			:pat{ (knows p2 (said p1 x)) (dummy y) (dummy z)}
			:pat{ (to p1 p2 (provisoFreeCond x y)) (dummy z)}			
			:pat{ (from p2 p1 (provisoFreeCond x z)) (dummy y)}
			)

;Com2 Rule (proviso-present)
:assumption (forall (p1 Principal) (p2 Principal) (x Infon) (y Infon) (z Infon) (k Infon)
			(implies (and (and (and (to p1 p2 (provisoPresentCond x y z)) (from p2 p1 (provisoPresentCond x y k))) (knows p1 z)) (knows p2 k)) (knows p2 (imp y (put p1 x))))
			:pat{(to p1 p2 (provisoPresentCond x y z)) (from p2 p1 (provisoPresentCond x y k))}
			:pat{ (knows p2 (imp y (put p1 x))) (dummy z) (dummy k)}
			:pat{ (to p1 p2 (provisoPresentCond x y z)) (dummy k)}
			:pat{ (from p2 p1 (provisoPresentCond x y k)) (dummy z)}
			)


;Ensue rule
:extrafuns ((knownSet Principal Set))
:assumption (forall (p Principal) (x Infon) (implies (entails (knownSet p) x) (knows p x))
	:pat{(knows p x)}
	:pat{ (entails (knownSet p) x) }
	)

;"knownSet p" is the set of all known infons of the principal p
:assumption (forall (p Principal) (x Infon) (iff (knows p x) (member x (knownSet p)) )
	:pat{(knows p x)}
	)

	
	
;--------------------------
;SAM example (DKAL 1 paper)

:extrafuns ((BAM Principal) (Bruce Principal) (Bob Principal))
:extrafuns ((canAccessCode Principal Infon))

;Bruce core policy
;:assumption (knows Bruce (tdonS BAM (canAccessCode Bruce)))

;Bruce accepts all communications from BAM
;:assumption (forall (y Infon) (from Bruce BAM y))

;All principals communicates their knowledge to Bruce
;:assumption (forall (x Principal) (y Infon) (to BAM Bruce y))

:assumption (forall (x Principal) (implies (and (knows Bruce (said x (canAccessCode Bruce))) (not (= x BAM))) (not (knows Bruce (canAccessCode Bruce)))))

;The safety theorem
:formula (not (implies (knows Bruce (canAccessCode Bruce)) (knows Bruce (said BAM (canAccessCode Bruce))) ))
:formula (not (knows Bruce (said BAM (canAccessCode Bruce))) )
:formula (not (knows Bruce (said Bob (canAccessCode Bruce))) )
:formula (not (knows Bruce (canAccessCode Bruce))) 

;--------------------------
	
)

