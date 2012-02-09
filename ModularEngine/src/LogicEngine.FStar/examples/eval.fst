module Eval
open Types
open Interp
open Authenticate

val lookup: substitution -> string -> option int
let rec lookup (s:substitution) name =
  let _ = println "starting lookup ...." in
  match s with
    | [] -> None
	| (n, t) :: rest ->
	  if (n.name = name) then 
	    (match t with
	       | Const c -> (match c with (Int i) -> let _ = println "found i..." in Some i | _ -> None)
		   | _ -> None)
      else lookup rest name


let rule_0 = 
	 let conditions = [ (Upon ((MonoTerm (App (RelationInfon ({fname="Init"; retType=Infon; argsType=[ Int32 ]; identity=None}))
 [ (Var ({name="i"; typ=Int32})) ])))) ] in 
	 let actions = [ (Add (MonoTerm (App (RelationInfon ({fname="NeedSum"; retType=Infon; argsType=[ Int32 ;  Int32 ]; identity=None}))
 [ (Const (Int 1)) ;
 (Const (Int 2)) ]))) ;
 (Add (MonoTerm (App (RelationInfon ({fname="NeedProduct"; retType=Infon; argsType=[ Int32 ;  Int32 ]; identity=None}))
 [ (Const (Int 11)) ;
 (Const (Int 22)) ]))) ;
 (Add (MonoTerm (App (RelationInfon ({fname="KnowsMath"; retType=Infon; argsType=[ Principal ]; identity=None}))
 [ (Const (PrincipalConstant "_BOB_")) ]))) ;
 (Drop (MonoTerm (App (RelationInfon ({fname="Init"; retType=Infon; argsType=[ Int32 ]; identity=None}))
 [ (Var ({name="i"; typ=Int32})) ]))) ] in 
	 mkRule [({name="i"; typ=Int32})] conditions actions
 

 let rule_1 = 
	 let conditions = [ (Upon ((MonoTerm (App (RelationInfon ({fname="NeedSum"; retType=Infon; argsType=[ Int32 ;  Int32 ]; identity=None}))
 [ (Var ({name="x"; typ=Int32})) ;
 (Var ({name="y"; typ=Int32})) ])))) ;
 (Upon ((MonoTerm (App (RelationInfon ({fname="KnowsMath"; retType=Infon; argsType=[ Principal ]; identity=None}))
 [ (Var ({name="p"; typ=Principal})) ])))) ] in 
	 let actions = [ (Fwd (Var ({name="p"; typ=Principal})) (MonoTerm 
 (App (RelationInfon ({fname="Sum"; retType=Infon; argsType=[ Int32 ;  Int32 ; Int32]; identity=None}))
 [ (Const (Int 1)) ;
 (Const (Int 2));
 (EvalTerm (fun (env:substitution) -> 
              match (lookup env "x") with
			    | Some i -> (match (lookup env "y") with Some j -> Const (Int (i+j)) | _ -> Const(Int 0))
				| _ -> Const (Int 0))) ])));
 (Drop (MonoTerm (App (RelationInfon ({fname="NeedSum"; retType=Infon; argsType=[ Int32 ;  Int32 ]; identity=None}))
 [ (Var ({name="x"; typ=Int32})) ;
 (Var ({name="y"; typ=Int32})) ]))) ] in 
	 mkRule [({name="p"; typ=Principal}) ;  ({name="y"; typ=Int32}) ;  ({name="x"; typ=Int32})] conditions actions
  
;;
 let _ = initialize ([ ({prin="_ALICE_"; pubkey="<RSAKeyValue><Modulus>kUWXmVPrxaQBAE+hxwzlzo/+hOTL60RIa2O+Ud0vnUUTB0OO+znVakY2dXnW/hqMa7cx49GCUFUx7iTv4cFYHPtAzFecyrbIzwLBdhbcATj2qpRVsrSoBtVRxEAqiktil5IhtCk7usy85HYDxNbVw/HWHvQRTd6cuYnKb4KNU1E=</Modulus><Exponent>AQAB</Exponent></RSAKeyValue>"; privkey=(Some "<RSAKeyValue><Modulus>kUWXmVPrxaQBAE+hxwzlzo/+hOTL60RIa2O+Ud0vnUUTB0OO+znVakY2dXnW/hqMa7cx49GCUFUx7iTv4cFYHPtAzFecyrbIzwLBdhbcATj2qpRVsrSoBtVRxEAqiktil5IhtCk7usy85HYDxNbVw/HWHvQRTd6cuYnKb4KNU1E=</Modulus><Exponent>AQAB</Exponent><P>xq4lK+sSdwEB1fCcyW5wQKfx74TWx+uxXjHunyXDKZhgkNiZ685VWr1kuiCfil2jOp8Hc/9WnlxcdH1nGvQXiQ==</P><Q>uy7hr+TXGQDNXyrje4MOIR9ZAlN1Oz5henc5IKBn+ddqlUQKH9rIqI/PQgBvT44bnnybBr2DiStafB9Hbs0jiQ==</Q><DP>xMnelZabznWX7OELWtThqJjwoM5Rsul34BXTBZ1wpjWAiFeSdacEkgD/0P/ZJkLDF6BG0JU7pVVUWimPw3m8CQ==</DP><DQ>hGb8AuQ29huoKXn34QTpuKoo1slb8iUE5JBym057XbFvVdgD5VZnezwGGaSfF8HobWmsas8gvKUq4wNpDsoSKQ==</DQ><InverseQ>nRitOmPpVe1vKg3Qq1+2SpCOlKxbfEED22CkRJxLVj8ElX0foB38RNJhHxnPvCAEvGdXVRLdeA4N5/nqU/pNyA==</InverseQ><D>N06ue+KWdeWNuAeZSQYhC/aIaSIOfOC/TZto3xP9x7t/lhlje0Q2e0KGA03Cy3ViFrRlWx3tphX5b3hCl8mbeMJLeOk8RFAWHVe3IpKr3Op+D8P2SD20/UopC4RaMlwsNSW++7a2ldifHCobZTwIAK68eR1cSL3gUt/sqnZOhcE=</D></RSAKeyValue>"); port=6301}) ;
 ({prin="_BOB_"; pubkey="<RSAKeyValue><Modulus>w82NXJIdeCJnm9vNadVHErCaRkoCfU8pap6LDw6N27yHRTTcQcTYZD27D20Nsji21cljjhhpefocP3sa6zeI+VjgtWBB16axdh3eROyyC/1vpS8Q2TIraBrp6Ew8ld51/p35tu5HZhuzN2gltgSTrzsMOvNGfz4HIVUt+DjuDDk=</Modulus><Exponent>AQAB</Exponent></RSAKeyValue>"; privkey=(Some "<RSAKeyValue><Modulus>w82NXJIdeCJnm9vNadVHErCaRkoCfU8pap6LDw6N27yHRTTcQcTYZD27D20Nsji21cljjhhpefocP3sa6zeI+VjgtWBB16axdh3eROyyC/1vpS8Q2TIraBrp6Ew8ld51/p35tu5HZhuzN2gltgSTrzsMOvNGfz4HIVUt+DjuDDk=</Modulus><Exponent>AQAB</Exponent><P>8odIOrLxHyb6lbh5qXd+BBMMOo0BDbH+W/B39y3+2YmSEb3uHhpUA+lrf3bMf0BXLRw695nUtWpC6b+sgo3pZQ==</P><Q>zq3XI8r6y2lwHTddCmczOWYL8oCz+z8t9Jf9/LPg+IN8HnYFxRy5ICLNzx0dgG3WaC4wjpk/royFQ3WTQqVURQ==</Q><DP>0jumxwtarPBzA9oXzGlCmXGRhie4pBCJN1VqCKCcbCIutqZ3hSy5a3KptqJafmxdpUL1crCsjF4ChvGaLsmpaQ==</DP><DQ>qot6jveMsdNEh2dK6C22cDPLwgT//1/oDQBqvl60Un01K3GaW0fTXzg4+iH9WR/Jn9gVi2Xbza34vWzE4mbIvQ==</DQ><InverseQ>0ZcWjOdE2Z3F9fdluvKeerIezF/3gzTmdHd9CYwPK/sIuEFP3tF0jzzJxGKFybxaGq0RYcnkkw74LwXSVOsJaw==</InverseQ><D>LkwECMdjwGwiI3AtecC8NWcck1IclJSLLnKeXskKMdK6CVseWU65+7m86UDX5DQUxyf/KjILfpPs6fWpv8Q51pE+ORHhyXOKPLDj/5vgHa0WFvFK3NoCsvB/ahIKINF+XpsWD7AmBWNJUPuqxZrCyYzv7qPT24gEJZCV9scmfIE=</D></RSAKeyValue>"); port=6302}) ]) in 
 let _ = run [rule_0; rule_1] in ()