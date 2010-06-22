;This version does not work with explicitely build sets at the left of the "entail" relationship whith "said" and "put" on the right, but when using DKAL constructions, this is not really neccesary.
(benchmark starbucks
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
;Starbucks scenario

:extrafuns ((Patrick Principal) (WinLogon Principal) (NTLM Principal) (GINA Principal) (Keyboard Principal) (Windows Principal))
:extrafuns ((isLoggedIn Principal Infon) (couldLogin Principal Infon) (cachedCredentials Principal Infon) (validCredentials Principal Infon)
			(hasEnteredUsername Principal Infon) (hasEnteredPassword Principal Infon))
:extrafuns ((checksOk Principal bool) (allowCachedLogon Principal bool))

;Start-up and local logon
;------------------------

;Patrick wants to log in to Windows, and WinLogon is the one that is responsible for that. Patrick has this knowledge assertion, since he trust in WinLogon with respect to his login state:
:assumption (knows Patrick (tdonP WinLogon (isLoggedIn Patrick)))

;And Patrick accepts any communication from WinLogon
:assumption (forall (x Infon) (from Patrick WinLogon x))

;WinLogon will allow a user x to login when NTLM says so, and WinLogon can verify that x passes all checks and that x is allowed to do a cached logon.
:assumption (forall (x Principal) (to WinLogon x (provisoFreeCond (isLoggedIn x) (plus (plus (said NTLM (couldLogin x)) (asInfon (checksOk x))) (asInfon (allowCachedLogon x)) ))))

;and Winlogon accepts communications from NTLM regarding a successful login
:assumption (forall (x Principal) (from WinLogon NTLM (provisoFree (couldLogin x))))

;Patrick is allowed to make a cached logon, and also Patrick passes all checks succesfully according to WinLogon database
:assumption (= true (checksOk Patrick))
:assumption (= true (allowCachedLogon Patrick))

;NTLM allows someone to login when GINA has cached his credentials, and the credentials are valid
:assumption (forall (x Principal) (to NTLM WinLogon (provisoFreeCond (couldLogin x) (plus (said GINA (cachedCredentials x)) (validCredentials x)))))
:assumption (knows NTLM (validCredentials Patrick))

;And NTLM accepts all communications from GINA
:assumption (forall (x Infon) (from NTLM GINA x))

;GINA caches someone's credentials when that person enters his login and password trough the keyboard
:assumption (forall (y Principal) (to GINA NTLM (provisoFreeCond (cachedCredentials y) (plus (hasEnteredUsername y) (hasEnteredPassword y)) )))

;The Keyboard informs GINA when Patrick has entered his username and password
:assumption (to Keyboard GINA (provisoFree (hasEnteredUsername Patrick)))
:assumption (to Keyboard GINA (provisoFree (hasEnteredPassword Patrick)))

;and GINA accepts communications from any principal
:assumption (forall (x Principal) (y Infon) (from GINA x y))

;In particular, GINA trusts the keyboard
:assumption (forall (x Infon) (knows GINA (tdonS Keyboard x)))

;so GINA learns
:formula (not (knows GINA (hasEnteredUsername Patrick)))
:formula (not (knows GINA (hasEnteredPassword Patrick)))

;Therefore, NTLM learns that GINA has cached Patrick's credentials
:formula (not (knows NTLM (said GINA (cachedCredentials Patrick))))

;Since NTLM knows that Patrick has valid credentials, it comunicates to WinLogon that that Patrick can be logged in, so WinLogon learns that.
:formula (not (knows WinLogon (said NTLM (couldLogin Patrick))))

;Now WinLogon informs that to Patrick, since Patrick passes all checks succesfully and he is allowed to perform a cached login
:formula (not (knows Patrick (said WinLogon (isLoggedIn Patrick))))

;And Patrick learns that he is logged in
:formula (not (knows Patrick (isLoggedIn Patrick)))

;Connecting to the T-mobile wireless network
;-------------------------------------------

:extrafuns ((TMobile Principal) (TrustedRoot Principal) (AuthenticationSite U) (thisCertificate U) (valid U Infon))
:extrafuns ((runs U Infon) (wantsToExecute Principal U Infon) (IE U) (isAllowedToRun Principal U Infon))
:extrafuns ((supportsSSL U Infon) (hasCertificate U U Infon) (expirationTime U Int) (properlySigned U Infon) (currentTime Principal Int) (currentSite U U) (mustShowLock U Infon))

;Patrick, knowing he needs to authenticate with T-mobile to use hotspot, launches IE
;Patrick communicate to Windows all the software he wants to execute
:assumption (forall (x U) (to Patrick Windows (provisoFreeCond (wantsToExecute Patrick x) (wantsToExecute Patrick x))))
;and today he wants to run Internet Explorer
:assumption (knows Patrick (wantsToExecute Patrick IE))

;Windows runs a specific piece of software if the user wants to execute it and he is allowed to do it
:assumption (forall (x Principal) (software U) (implies (knows Windows (plus (said x (wantsToExecute x software)) (isAllowedToRun x software))) (knows Windows (runs software)))) 

;Patrick is allowed to run IE
:assumption (knows Windows (isAllowedToRun Patrick IE))

;and Windows accept all communication events related to running software
:assumption (forall (x Principal) (software U) (from Windows x (provisoFree (wantsToExecute x software))))

;So Windows learns it should run IE
:formula (not (knows Windows (runs IE)))

;The T-mobile router redirects IE ot the T-mobile Authentication Manager web site

;TMobile communicates that the AuthenticationSite supports SSL
:assumption (to TMobile Windows (provisoFree (supportsSSL AuthenticationSite)))
;and the certificate for the Authentication site is thisCertificate
:assumption (to TMobile Windows (provisoFree (hasCertificate AuthenticationSite thisCertificate)))


;This is the current site of IE, since it has been redirected by TMobile
:assumption (= AuthenticationSite (currentSite IE))
;Windows accepts any communication from TMobile
:assumption (forall (x Infon) (from Windows TMobile x))

;so Windows learns that TMobile said that the current site supports SSL and that the certificate is thisCertificate
:formula (not (knows Windows (said TMobile (plus (supportsSSL (currentSite IE)) (hasCertificate (currentSite IE) thisCertificate)))))

;The trusted root communicates that a given certificate is valid when is properly signed, and with the proviso that the certificate has not expired considering the receiver current time
:assumption (forall (x Principal) (y U) (to TrustedRoot x (provisoPresentCond (valid y) (asInfon (< (currentTime x) (expirationTime y))) (properlySigned y) )))
;The trusted root knows that thisCertificate is properlySigned
:assumption (knows TrustedRoot (properlySigned thisCertificate))
;And this is the expiration time of thisCertificate
:assumption (= 20 (expirationTime thisCertificate))

;and accepts any communication from the TrustedRoot
:assumption (forall (x Infon) (from Windows TrustedRoot x))
;This is the current time for Windows
:assumption (= 10 (currentTime Windows))
;So Windows learns
:formula (not (knows Windows (put TrustedRoot (valid thisCertificate))))

;Windows trust on everything the TrustedRoot says
:assumption (forall (x Infon) (knows Windows (tdonP TrustedRoot x)))
;so it learns
:formula (not (knows Windows (valid thisCertificate)))

;Windows knows that it must display the lock on IE when TMobile says that the current site supports SSL, and the site certificate is valid.
:assumption (knows Windows (imp (plus (plus (said TMobile (supportsSSL (currentSite IE))) (said TMobile (hasCertificate (currentSite IE) thisCertificate))) (valid thisCertificate))
			(mustShowLock IE)))

:formula (not (knows Windows (mustShowLock IE)))

;At the T-Mobile Authentication web site, Patrick successfully enters his T-Mobile credentials

:extrafuns ((TMobileRouter Principal))
:extrafuns ((thisIsTheWelcomePageFor Principal Infon))

;T-Mobile get Patrick's credentials and validates them. Then, it communicates to Windows that the credentials are valid
:assumption (to TMobile Windows (provisoFree (validCredentials Patrick)))

;Windows is already receiving any communication from TMobile, so it learns
:formula (not (knows Windows (said TMobile (validCredentials Patrick))))

;The T-Mobile router redirects a user to the welcome page with the proviso that TMobile says that the user has valid credentials
:assumption (forall (x Principal) (to TMobileRouter Windows (provisoPresent (thisIsTheWelcomePageFor x) (said TMobile (validCredentials x) )))

;Windows receives any communication from TMobile
:assumption (forall (x Infon) (from Windows TMobile x))

;So Windows learns that
:formula (not (put TMobileRouter (thisIsTheWelcomePageFor Patrick)))

)

