module SampleProgram1
open TypeHeaders
open Types
open Interp
open Subst

type TrustRule :: _ = 
    (fun (xs:vars) (cs:conditions) (acts:actions) => 
        (forall (a:action) (subst:substitution). In a acts && Holds xs cs subst => (Enabled (ActionSubst a subst))))

let mkRule xs cs acts = 
  assume (TrustRule xs cs acts);
  Rule xs cs acts

let rule1 = 
  let vx = {name="x"; typ=Principal} in 
  let vars = [vx] in
  let conditions = [(If (MonoTerm (App SaidInfon [(Var vx);
                                                  (App EmptyInfon [])])))] in 
  let actions = [(Learn (MonoTerm (App EmptyInfon [])))] in
    mkRule vars conditions actions


end
