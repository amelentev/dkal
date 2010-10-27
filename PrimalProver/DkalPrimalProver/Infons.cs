using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Collections;
using TUVienna.CS_CUP.Runtime;

namespace DkalPrimalProver
{

    /* This interface should be implemented by the infons that are atomic: true, variables and functions.
     */
    public interface SimpleInfon
    { 
    }

    /* Base abstract class for Infons.
     * Infons also represents prefixes "p1 told p2 told ... pn told". 
     * Infons can have prefixes (that represent the position of that infon in a bigger
     * infon structure). When used as prefixes, infons have the empty (just EndPrefix) prefix.
     */
    public abstract class Infon : ICloneable
    {

        //Free variables of an Infon
        //For the primal fragment this is not used
        protected System.Collections.Generic.List<Variable> varList = new System.Collections.Generic.List<Variable>();

        //An infon can have a prefix, that represents its position in a larger parse tree. A prefix is an Infon itself.
        Infon prefix;

        protected int cachedHash;
        protected bool isHashFreezed = false;

        /* By default, an Infon has an empty prefix
         */
        public Infon()
        {
            this.prefix = new EndPrefix();
        }

        public Infon(Infon prefix)
        {
            this.prefix = prefix;
        }

        public System.Collections.Generic.List<Variable> getVariableList()
        {
            return varList;
        }

        public Infon getPrefix()
        {
            return prefix;
        }

        public void setPrefix(Infon prefix)
        {
            this.prefix = prefix;
        }

        /* Adds "p said" or "p implied" depending on the Principal p and the Infon SaidImplied 
         */
        public virtual void propagatePrefix(Principal p, Infon saidImplied)
        {
            if (saidImplied is Said)
            {
                prefix = new Said(p, prefix, new EndPrefix());
            }
            else if (saidImplied is Implied)
            {
                prefix = new Implied(p, prefix, new EndPrefix());
            }
            else
            {
                throw new Exception("Said or Implied expected, but Infon is " + saidImplied.GetType());
            }
        }

        public abstract Object Clone();
        public abstract override bool Equals(object obj);
        public abstract override int GetHashCode();
        
        /* Once an Infon is not going to change anymore, the hash code can
         * be freezed for best performance
         */
        public void freezeHashCode()
        {
            isHashFreezed = true;
            cachedHash = this.GetHashCode();
        }

        /*Each subclass has to know how to compute its own subformulas. 
         * We assume the list represent a DFS exploration of the subformula tree
         */
        public abstract List<Infon> getSubformulas();

        /* Calculate all the derived prefixes of the current infon.
         * For each prefix px, it construct a new infon prefixed with px. The prefix of
         * this new infon is empty. 
         * Returns all the infons constructed in this way
         */
        public HashSet<Infon> getGraftedInfons()
        {
            //Calculates all the prefixes
            HashSet<Infon> prefixes = Infon.getPrefixes(this.prefix);
            HashSet<Infon> result = new HashSet<Infon>();
            
            //For each prefix, it calculates the new prefixed infon
            foreach (Infon prefix in prefixes)
            {
                Infon newInfon = (Infon)this.Clone();
                newInfon.removePrefix();
                newInfon.setPrefix(prefix);
                result.Add(newInfon);
            }

            return result;
        }

        /* Calculates all the derived prefixes of a given prefix.
         * These are all the possible replacements of said by implied of the input parameter
         * Returns the set of derived prefixes
         */
        public static HashSet<Infon> getPrefixes(Infon prefix)
        {
            HashSet<Infon> result = new HashSet<Infon>();
            if (prefix is SaidImplied)
            {
                SaidImplied saidImplied = (SaidImplied)prefix;
                //We calculate all the prefixes of the knowledge
                HashSet<Infon> subResult = Infon.getPrefixes(saidImplied.getKnowledge());

                //If the prefix is "p said ..." we add "p said" and "p implied" to all the prefixes
                //of the knowledge
                if (saidImplied is Said)
                {
                    foreach (Infon subPrefix in subResult)
                    {
                        result.Add(new Said(saidImplied.getPrincipal(), subPrefix, new EndPrefix()));
                        result.Add(new Implied(saidImplied.getPrincipal(), subPrefix, new EndPrefix()));
                    }
                }
                //If it is "p implied ..." we add just "p implied" to all the prefixes of the knowledge
                else
                {
                    foreach (Infon subPrefix in subResult)
                    {
                        result.Add(new Implied(saidImplied.getPrincipal(), subPrefix, new EndPrefix()));
                    }
                }

            }
            //This case should be EndPrefix (given that a prefix is composed by said, implied or EndPrefix)
            else
            {
                result.Add(prefix);
            }
            return result;
        }

        /* Returns the current infon prefixed with its own prefix. The prefix of this new infon is empty.
         * The return value is an entirely new copy.
         */
        public Infon getInfonWithPrefix()
        {
            return getInfonWithThisPrefix(this.getPrefix());
        }

        /* Returns the current infon prefixed with the prefix "prefix".
         */
        public Infon getInfonWithThisPrefix(Infon prefix)
        {
            //If the prefix is empty, we just return a copy of the current infon with empty prefix
            if (prefix is EndPrefix)
            {
                Infon infon = (Infon)this.Clone();
                infon.removePrefix();
                return infon;
            }
            else
            {
                //If we are in the case of "p told", we add p told to the prefixed infon of the knowledge
                SaidImplied saidImpliedPrefix = (SaidImplied)prefix;

                Infon result;
                if (saidImpliedPrefix is Said)
                {
                    result = new Said(saidImpliedPrefix.getPrincipal(), getInfonWithThisPrefix(saidImpliedPrefix.getKnowledge()), new EndPrefix());
                }
                else
                {
                    result = new Implied(saidImpliedPrefix.getPrincipal(), getInfonWithThisPrefix(saidImpliedPrefix.getKnowledge()), new EndPrefix());
                }
                result.removePrefix();

                return result;
            }
        }

        /* Removes the prefix of the current infon and all the prefixes of the sub-infons that compose the current infon
         */
        public abstract void removePrefix();
    }


    /* Dummy infon used to represent the end of a prefix. The "told" chain ends here.
     */
    public class EndPrefix : Infon
    {
        public EndPrefix()
            : base(null)
        {
        }

        public override object Clone()
        {
            return new EndPrefix();
        }

        public override List<Infon> getSubformulas()
        {
            List<Infon> list = new List<Infon>();
            list.Add(this);

            return list;
        }

        public override bool Equals(object obj)
        {
            return obj is EndPrefix;
        }

        public override int GetHashCode()
        {
            return 1;
        }

        public override string ToString()
        {
            return "End";
        }

        public override void removePrefix()
        {
        }
    }

    /* The infon that represents True.
     */
    public class TrueInfon : Infon, SimpleInfon
    {
        public override String ToString()
        {
            return "True";
        }

        public override List<Infon> getSubformulas()
        {
            List<Infon> set = new List<Infon>();
            set.Add(this);
            return set;
        }

        public override bool Equals(object obj)
        {
            return (obj is TrueInfon) && getPrefix().Equals(((TrueInfon)obj).getPrefix());
        }

        public override int GetHashCode()
        {
            if (isHashFreezed)
            {
                return cachedHash;
            }
            else
            {
                return getPrefix().GetHashCode();
            }
        }

        public override Object Clone()
        {
            TrueInfon trueInfon = new TrueInfon();
            trueInfon.setPrefix((Infon)this.getPrefix().Clone());
            return trueInfon;
        }

        public override void removePrefix()
        {
            setPrefix(new EndPrefix());
        }

    }

    /* A variable infon. This is not currently used for the primal fragment
     * The type of a variable is filled during parsing.
     */
    public class Variable : Infon, SimpleInfon
    {

        string name;
        string type;

        public Variable(string name)
        {
            this.name = name;
            this.getVariableList().Add(this);
        }

        public string getName()
        {
            return this.name;
        }

        public void setType(string type)
        {
            this.type = type;
        }

        public string getType()
        {
            return this.type;
        }

        public override String ToString()
        {
            return getName();
        }

        public override List<Infon> getSubformulas()
        {
            List<Infon> set = new List<Infon>();
            set.Add(this);
            return set;
        }

        public override bool Equals(object obj)
        {
            if (!(obj is Variable))
            {
                return false;
            }
            else
            {
                Variable var = (Variable)obj;

                return var.getName().Equals(name) && var.getType().Equals(type) && var.getPrefix().Equals(this.getPrefix());
            }
        }

        public override int GetHashCode()
        {
            if (isHashFreezed)
            {
                return cachedHash;
            }
            else
            {
                return name.GetHashCode() +
                    type.GetHashCode() + 
                    getPrefix().GetHashCode();
            }
            
        }

        public override Object Clone()
        {
            Variable var = new Variable(this.getName());
            var.setType(this.getType());
            var.setPrefix((Infon)this.getPrefix().Clone());
            return var;
        }

        public override void removePrefix()
        {
            setPrefix(new EndPrefix());
        }

    }

    /* Interface to be shared by Plus and Implies, since both have a left and right sides.
    */
    public interface PlusOrImplies
    {
        Infon getRight();
        Infon getLeft();
    }

    /* The infon that represents a "+"
     */
    public class Plus : Infon, PlusOrImplies
    {

        Infon left;
        Infon right;

        public Plus(Infon left, Infon right)
        {
            this.left = left;
            this.right = right;
            this.getVariableList().AddRange(left.getVariableList());
            this.getVariableList().AddRange(right.getVariableList());
        }

        public Infon getLeft()
        {
            return this.left;
        }

        public Infon getRight()
        {
            return this.right;
        }

        public override String ToString()
        {
            return getLeft().ToString() + " + " + getRight().ToString();
        }

        public override List<Infon> getSubformulas()
        {
            List<Infon> set = new List<Infon>();
            set.Add(this);
            set.AddRange(this.left.getSubformulas());
            set.AddRange(this.right.getSubformulas());
            return set;
        }

        public override bool Equals(object obj)
        {
            if (!(obj is Plus))
            {
                return false;
            }
            else
            {
                Plus plus = (Plus)obj;

                return plus.getLeft().Equals(left) && plus.getRight().Equals(right) && plus.getPrefix().Equals(this.getPrefix());
            }
        }

        public override int GetHashCode()
        {
            if (isHashFreezed)
            {
                return cachedHash;
            }
            else 
            {
                return left.GetHashCode() + right.GetHashCode() + getPrefix().GetHashCode();
            }
            
        }

        /* The prefix is propagated to the children of the Plus
         */
        public override void propagatePrefix(Principal p, Infon saidImplied)
        {
            base.propagatePrefix(p, saidImplied);
            left.propagatePrefix(p, saidImplied);
            right.propagatePrefix(p, saidImplied);
        }

        public override object Clone()
        {
            Plus plus = new Plus((Infon)left.Clone(), (Infon)right.Clone());
            plus.setPrefix((Infon)this.getPrefix().Clone());
            return plus;
        }

        public override void removePrefix()
        {
            setPrefix(new EndPrefix());
            right.removePrefix();
            left.removePrefix();
        }

    }

    /* This infon represents the "->"
     */
    public class Implies : Infon, PlusOrImplies
    {
        Infon left;
        Infon right;

        public Implies(Infon left, Infon right)
        {
            this.left = left;
            this.right = right;
            this.getVariableList().AddRange(left.getVariableList());
            this.getVariableList().AddRange(right.getVariableList());
        }

        public Infon getLeft()
        {
            return this.left;
        }

        public Infon getRight()
        {
            return this.right;
        }

        public override String ToString()
        {
            return getLeft().ToString() + " -> " + getRight().ToString();
        }


        public override List<Infon> getSubformulas()
        {
            List<Infon> set = new List<Infon>();
            set.Add(this);
            set.AddRange(this.left.getSubformulas());
            set.AddRange(this.right.getSubformulas());
            return set;
        }

        public override bool Equals(object obj)
        {
            if (!(obj is Implies))
            {
                return false;
            }
            else
            {
                Implies implies = (Implies)obj;

                return implies.getLeft().Equals(left) && implies.getRight().Equals(right) && implies.getPrefix().Equals(this.getPrefix());
            }
        }

        public override int GetHashCode()
        {
            if (isHashFreezed)
            {
                return cachedHash;
            }
            else
            {
                return left.GetHashCode() + right.GetHashCode() + this.getPrefix().GetHashCode();
            }
            
        }

        public override void propagatePrefix(Principal p, Infon saidImplied)
        {
            base.propagatePrefix(p, saidImplied);
            left.propagatePrefix(p, saidImplied);
            right.propagatePrefix(p, saidImplied);
        }

        public override object Clone()
        {
            Implies imp = new Implies((Infon)left.Clone(), (Infon)right.Clone());
            imp.setPrefix((Infon)this.getPrefix().Clone());
            return imp;
        }

        public override void removePrefix()
        {
            setPrefix(new EndPrefix());
            right.removePrefix();
            left.removePrefix();
        }

    }

    /* Base class for Said and Implied.
     */
    public abstract class SaidImplied : Infon
    {

        Principal principal;
        Infon knowledge;

        public SaidImplied() { }

        /* This constructor does not propagate the prefix. It is basically used to construct prefixes
         */
        public SaidImplied(Principal principal, Infon knowledge, Infon prefix)
        {
            this.principal = principal;
            this.knowledge = knowledge;
            this.getVariableList().AddRange(knowledge.getVariableList());
            this.setPrefix(prefix);
        }

        /* This constructor propagates the prefix to the sub-infons
         */
        public SaidImplied(Principal principal, Infon knowledge)
        {
            this.principal = principal;
            this.knowledge = knowledge;
            this.getVariableList().AddRange(knowledge.getVariableList());

            if (principal != null && knowledge != null)
            {
                knowledge.propagatePrefix(principal, this);
            }
        }

        public SaidImplied(Infon knowledge)
        {
            this.knowledge = knowledge;

            if (principal != null && knowledge != null)
            {
                knowledge.propagatePrefix(principal, this);
            }
        }


        public void setPrincipal(Principal principal)
        {
            this.principal = principal;

            if (principal != null && knowledge != null)
            {
                knowledge.propagatePrefix(principal, this);
            }
        }

        public Principal getPrincipal()
        {
            return this.principal;
        }

        public Infon getKnowledge()
        {
            return this.knowledge;
        }


        public override List<Infon> getSubformulas()
        {
            List<Infon> set = new List<Infon>();
            set.Add(this);
            set.AddRange(this.knowledge.getSubformulas());
            return set;
        }


        public override bool Equals(object obj)
        {
            if (!(obj is SaidImplied))
            {
                return false;
            }
            else
            {
                SaidImplied saidImplied = (SaidImplied)obj;

                bool equals = saidImplied.getPrincipal().Equals(principal) && saidImplied.getKnowledge().Equals(knowledge);
                return equals && getPrefix().Equals(saidImplied.getPrefix());
            }
        }

        public override int GetHashCode()
        {
            if (isHashFreezed)
            {
                return cachedHash;
            }
            else 
            {
                return principal.GetHashCode() + knowledge.GetHashCode() + getPrefix().GetHashCode();
            }
        }

        public override void propagatePrefix(Principal p, Infon saidImplied)
        {
            base.propagatePrefix(p, saidImplied);
            getKnowledge().propagatePrefix(p, saidImplied);
        }

        public abstract override object Clone();

        public override void removePrefix()
        {
            setPrefix(new EndPrefix());
            knowledge.removePrefix();
        }

    }

    /* Infon that represents "p said x"
     */
    public class Said : SaidImplied
    {
        public Said() { }

        public Said(Principal principal, Infon knowledge)
            : base(principal, knowledge)
        { }

        public Said(Principal principal, Infon knowledge, Infon prefix)
            : base(principal, knowledge, prefix)
        { }

        public Said(Infon knowledge)
            : base(knowledge)
        {
        }

        public override String ToString()
        {
            if (getKnowledge() is SimpleInfon || getKnowledge() is SaidImplied)
            {
                return getPrincipal().ToString() + " said " + getKnowledge().ToString();
            }
            else 
            {
                return getPrincipal().ToString() + " said (" + getKnowledge().ToString() + ")";
            }
        }

        public override bool Equals(object obj)
        {
            if (!(obj is Said))
            {
                return false;
            }
            else
            {
                return base.Equals(obj);
            }
        }

        public override List<Infon> getSubformulas()
        {
            List<Infon> list = new List<Infon>();
            list.Add(this);
            list.AddRange(this.getKnowledge().getSubformulas());

            return list;
        }

        public override object Clone()
        {
            return new Said(new Principal(getPrincipal().getName()), (Infon)getKnowledge().Clone(), (Infon)getPrefix().Clone());
        }

        public override int GetHashCode()
        {
            return base.GetHashCode();
        }
    }

    /* Infon that represents "p implied x"
     */
    public class Implied : SaidImplied
    {
        public Implied() { }

        public Implied(Principal principal, Infon knowledge, Infon prefix)
            : base(principal, knowledge, prefix)
        { }

        public Implied(Principal principal, Infon knowledge)
            : base(principal, knowledge)
        {
        }

        public Implied(Infon knowledge)
            : base(knowledge)
        {
        }

        public override String ToString()
        {
            if (getKnowledge() is SimpleInfon || getKnowledge() is SaidImplied)
            {
                return getPrincipal().ToString() + " implied " + getKnowledge().ToString();
            }
            else
            {
                return getPrincipal().ToString() + " implied (" + getKnowledge().ToString() + ")";
            }
        }

        public override bool Equals(object obj)
        {
            if (!(obj is Implied))
            {
                return false;
            }
            else
            {
                return base.Equals(obj);
            }
        }

        public override object Clone()
        {
            return new Implied(new Principal(getPrincipal().getName()), (Infon)getKnowledge().Clone(), (Infon)getPrefix().Clone());
        }

        public override int GetHashCode()
        {
            return base.GetHashCode();
        }
    }

    /* Infon that represents a function.
     * A function has a name and a list of arguments
     */
    public class Function : Infon, SimpleInfon
    {
        string name;
        System.Collections.ArrayList arguments;

        //To be filled during the parsing
        string returnType = "";

        public Function(string constant)
        {
            this.name = constant;
            this.arguments = new System.Collections.ArrayList();
        }

        public Function(string name, System.Collections.ArrayList arguments)
        {
            this.name = name;
            this.arguments = arguments;
            foreach (Infon arg in arguments)
            {
                this.getVariableList().AddRange(arg.getVariableList());
            }
        }

        public void setArguments(System.Collections.ArrayList arguments)
        {
            this.arguments = arguments;
            foreach (Infon arg in arguments)
            {
                this.getVariableList().AddRange(arg.getVariableList());
            }
        }


        public string getName()
        {
            return this.name;
        }

        public ArrayList getArguments()
        {
            return this.arguments;
        }

        public override String ToString()
        {
            String res = getName().ToString();

            if (getArguments().Count > 0)
            {
                res += "(";
                foreach (Object arg in getArguments())
                {
                    res += arg.ToString() + ", ";
                }
                res = res.Remove(res.Length - 2);

                res += ")";
            }

            return res;
        }

        public override List<Infon> getSubformulas()
        {
            List<Infon> set = new List<Infon>();
            set.Add(this);
            return set;
        }

        public override bool Equals(object obj)
        {
            if (!(obj is Function))
            {
                return false;
            }
            else
            {
                Function fun = (Function)obj;
                return fun.getName().Equals(name) && sameArguments(fun) && fun.getPrefix().Equals(this.getPrefix());
            }
        }

        public override int GetHashCode()
        {
            if (isHashFreezed)
            {
                return cachedHash;
            }
            else
            {
                return name.GetHashCode() + this.getPrefix().GetHashCode();
            }
        }

        private bool sameArguments(Function fun)
        {
            if (fun.getArguments().Count != arguments.Count)
            {
                return false;
            }

            for (int i = 0; i < arguments.Count; i++)
            {
                if (!arguments[i].Equals(fun.getArguments()[i]))
                {
                    return false;
                }
            }
            return true;
        }

        public string getReturnType()
        {
            return returnType;
        }

        public void setReturnType(string returnType)
        {
            this.returnType = returnType;
        }

        /*
         * Only makes a shallow copy of the arguments. Some cloneable interface should be implemented
         * by the arguments.
         * This is a pending issue... we should decide which type of arguments beside infons we would like
         * to have.
         */
        public override object Clone()
        {
            ArrayList arguments = this.getArguments();
            
            //Beware, we are making a shallow copy here. This is still pending
            //since we are not really using the function arguments for anything right
            //now. Functions are treated very much as constants
            ArrayList argumentsCopy = (ArrayList) arguments.Clone();

            Function function = new Function(name, argumentsCopy);
            function.setReturnType(this.getReturnType());
            function.setPrefix((Infon)getPrefix().Clone());
            return function;
        }

        public override void removePrefix()
        {
            setPrefix(new EndPrefix());
        }
    }
}
