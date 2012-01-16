(* A complete, incremental decision procedure for quantified infon logic *)
module HilbertianQIL
open Types
open Util
open Subst
open Typing
open Unify

(*********************************************************************************)
(* Quotation prefixes *)
(*********************************************************************************)
logic function WithPrefix : prefix -> term -> term 
assume forall (i:term). (WithPrefix [] i) = i
assume forall (p:term) (pfx:prefix) (i:term). 
  (WithPrefix (p::pfx) i) = (WithPrefix pfx (App SaidInfon [p; i]))
val withPrefix: pfx:prefix -> i:term -> j:term{j=(WithPrefix pfx i)}
let rec withPrefix pfx i = match pfx with 
  | [] -> i
  | p::pfx' -> (withPrefix pfx' (App SaidInfon [p; i]))


(*********************************************************************************)
(* Alpha conversion (for polyterms in infostrate) *)
(*********************************************************************************)
type alphaEquiv :: polyterm => polyterm => P =
  | AEQ_Mono: i:term -> alphaEquiv (MonoTerm i) (MonoTerm i)

  | AEQ_Poly: 
       xs:vars -> t:term -> ys:vars
    -> polytyping [] (ForallT xs t)
    -> wfG ys 
    -> varsTyEq xs ys
    -> alphaEquiv (ForallT xs t) 
                  (ForallT ys (Subst t (MkSubst xs (AsTerms ys))))

  | AEQ_Justified: 
      p:term -> i:polyterm -> d:term  -> j:polyterm
    -> alphaEquiv i j 
    -> alphaEquiv (JustifiedPoly p i d) (JustifiedPoly p j d)

  | AEQ_Refl: i:polyterm -> alphaEquiv i i

assume forall (i:polyterm) (j:polyterm). (CheckedInfon i && alphaEquiv i j) => CheckedInfon j


  
(*********************************************************************************)
(* Definition of DKAL entailment *)
(* entails S K G i *)
(* entails S K [] i *)
(*********************************************************************************)
type entails :: substrate => infostrate => vars => term => P =
  | Entails_Emp : 
       S:substrate -> K:infostrate -> G:vars
    -> pref:prefix
    -> typing G (WithPrefix pref (App EmptyInfon [])) Infon
    -> entails S K G (WithPrefix pref (App EmptyInfon []))

  | Entails_And_Intro : (* Simpler version with binary And *)
       S:substrate -> K:infostrate -> G:vars
    -> i:term -> j:term -> pref:prefix
    -> entails S K G (WithPrefix pref i)
    -> entails S K G (WithPrefix pref j)
    -> entails S K G (WithPrefix pref (App AndInfon [i;j]))
    
  | Entails_And_Elim1 : 
       S:substrate -> K:infostrate -> G:vars
    -> i1:term -> i2:term -> pref:prefix
    -> entails S K G (WithPrefix pref (App AndInfon [i1; i2]))
    -> entails S K G (WithPrefix pref i1)
    
  | Entails_And_Elim2 : 
       S:substrate -> K:infostrate -> G:vars
    -> i1:term -> i2:term -> pref:prefix
    -> entails S K G (WithPrefix pref (App AndInfon [i1; i2]))
    -> entails S K G (WithPrefix pref i2)
    
  | Entails_W_Imp_Intro : 
       S:substrate -> K:infostrate -> G:vars
    -> i:term -> j:term -> pref:prefix
    -> entails S K G (WithPrefix pref j)
    -> typing G i Infon
    -> entails S K G (WithPrefix pref (App ImpliesInfon [i; j]))

  | Entails_Imp_Elim : 
       S:substrate -> K:infostrate -> G:vars
    -> i:term -> j:term -> pref:list term
    -> entails S K G (WithPrefix pref i)
    -> entails S K G (WithPrefix pref (App ImpliesInfon [i;j]))
    -> entails S K G (WithPrefix pref j)

  | Entails_J_Elim:
       S:substrate -> K:infostrate -> G:vars
    -> p:term -> i:term -> d:term -> pref:prefix
    -> entails S K G (WithPrefix pref (App JustifiedInfon [p;i;d]))
    -> entails S K G (WithPrefix pref i)
    
  (* Should handle this *)
  | Entails_Hyp_Substrate :
       S:substrate -> K:infostrate -> G:vars
    -> q:ISubstrateQueryTerm{SubstrateSays S q}     (* Eventually, we get a signature from the DB about this fact *)
    -> entails S K G (App AsInfon [(SubstrateQueryTerm q)])

 (* Handled by Bernays-Schonfinkel elim *)
  | Entails_Q_Elim:
       S:substrate -> K:infostrate -> G:vars
    -> xs:vars -> t:term -> is:list term 
    -> polyentails S K G (ForallT xs t)
    -> ZipP term var (fun (i:term) (x:var) => typing G i x.typ) is xs
    -> entails S K G (Subst t (MkSubst xs is))

  | Entails_Poly:
       S:substrate -> K:infostrate -> G:vars
    -> i:term
    -> polyentails S K G (MonoTerm i)
    -> entails S K G i

and polyentails :: substrate => infostrate => vars => polyterm => P = 
  | Entails_Hyp_Knowledge :
       S:substrate -> K:infostrate -> G:vars
    -> i:infon{In i K}
    -> i':polyterm
    -> polytyping [] i
    -> alphaEquiv i i'
    -> polytyping G i'
    -> polyentails S K G i'

  | Entails_Q_Intro :
       S:substrate -> K:infostrate -> G:vars
    -> i:term 
    -> wfG G
    -> entails S K G i 
    -> polyentails S K [] (ForallT G i)

  | Entails_Mono: 
      S:substrate -> K:infostrate -> G:vars
    -> i:term
    -> entails S K G i
    -> polyentails S K G (MonoTerm i)

  | Entails_ElimJust: 
      S:substrate -> K:infostrate -> G:vars
    -> p:term -> i:polyterm -> d:term
    -> polyentails S K G (JustifiedPoly p i d)
    -> polyentails S K G i
