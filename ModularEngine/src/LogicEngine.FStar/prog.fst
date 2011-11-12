module SampleProgram1
open Types
open Interp
open Subst

type TrustRule :: _ = 
    (fun (xs:vars) (cs:conditions) (a:action) => 
        (forall (subst:substitution). Holds xs cs subst => (Enabled (ActionSubst a subst))))

let mkRule xs cs acts = 
  let acts = map (fun a -> assume (TrustRule xs cs a);
                    (a:(a:action{TrustRule xs cs a}))) acts in
    Rule xs cs acts

let rule1 = 
  let vx = {name="x"; typ=Principal} in 
  let vars = [vx] in
  let conditions = [(If (MonoTerm (App SaidInfon [(Var vx);
                                                  (App EmptyInfon [])])))] in 
  let actions = [(Learn (MonoTerm (App EmptyInfon [])))] in
    mkRule vars conditions actions


end
