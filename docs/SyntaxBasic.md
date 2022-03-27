# Basic Syntax Guide
In order to define the DKAL syntax we first introduce the [DKAL variables](#syntaxBasicVariables), then the [DKAL data types](#syntaxBasicTypes), and finally expand on how to construct terms of each type.

## {anchor:syntaxBasicVariables} DKAL Variables
Variables in DKAL start with a capitalized letter. The rest of the name can use upper or lowercase letters, underscores and numbers. 

Variables must be explicitly declared with a DKAL data type. The variable declaration syntax is as follows:
{{
with <v1>: <t1>, ..., <vn>: <tn>
}}
Where <v1>, ..., <vn> are variable names and <t1>, ..., <tn> are types.

Whenever a fresh variable needs to be used only once, a single underscore symbol ("_") may be used in its place as a wildcard.

## {anchor:syntaxBasicTypes} Data Types
DKAL has the following infon data types:
* {{Dkal.Infon}} is used to represent [unjustified infons](#syntaxBasicInfon), infons that carry no attached evidence.
* {{Dkal.JustifiedInfon}} is used to represent [infons that carry attached evidence](#syntaxBasicJustifiedInfon).
* {{Dkal.AbstractInfon}} is used to represent infons that are either justified or unjustified. In can be seen as an abstract supertype of {{Dkal.Infon}} and {{Dkal.JustifiedInfon}}.

DKAL also uses the following data types:
* {{Dkal.Evidence}} is used to represent [proofs](#syntaxBasicProof). An infon may be communicated together with a proof. For more information on infon proofs or evidence see the paper on [Evidential Authorization using DKAL](http://research.microsoft.com/en-us/um/people/gurevich/Opera/203.pdf).
* {{Dkal.Principal}} is used to represent [principals](#syntaxBasicPrincipal) (or agents) in a distributed environment. Each principal computes its own knowledge and has its own policy that indicates how to construct and use that knowledge.
* {{Dkal.Rule}} is used to represent policy [rules](#syntaxBasicRule). A rule has two parts: a condition and an action to be performed whenever the condition is satisfied.
* {{Dkal.Condition}} is used to represent the [condition](#syntaxBasicCondition) of a rule.
* {{Dkal.Action}} is used to represent the [action](#syntaxBasicAction) of a rule.
* {{Dkal.SubstrateQuery}} is used to represent a [query on a data source](#syntaxBasicSubstrateQuery). Data sources allow a principal to obtain information and use it as part of its knowledge. This information is obtained by means of substrate query terms that have {{Dkal.SubstrateQuery}} type.
* {{Dkal.SubstrateUpdate}} is used to represent an [update on a data source](#syntaxBasicSubstrateUpdate). Some data sources may also have mutable state and the policy can indicate how and when to modify this state. Substrate update terms have {{Dkal.SubstrateUpdate}} type.

In addition, DKAL also supports all the .NET basic types, such as {{System.String}}, {{System.Int32}}, {{System.Boolean}}, etc. DKAL has support for some of the [.NET literals](#syntaxBasicOtherLiterals).

## {anchor:syntaxBasicInfon} Unjustified Infon Terms
Infon terms can be constructed using any of the following:
* **Conjunction:** {{<i1> && <i2>}} where both {{<i1>}} and {{<i2>}} are infon terms.
* **Disjunction:** {{<i1> || <i2>}} where both {{<i1>}} and {{<i2>}} are infon terms. Disjunction supported only on propositional primal infon logic based [Logic Engines](Logic-Engines).
* **Implication:** {{<i1> -> <i2>}} where both {{<i1>}} and {{<i2>}} are infon terms.
* **Quotation:** {{<p> said <i>}} where {{<p>}} is a principal term and {{<i>}} is an infon term.
* **Relation:** {{<relName>(<t1>,...,<tn>)}} where {{<relName>}} is a declared infon relation (see [relation declarations](#syntaxPolicyHeaderRelation)) of arity {{n}} and {{<t1>}},...,{{<tn>}} have the correct types according to the types indicated in the declaration of {{<relName>}}.
* **AsInfon:** {{asInfon(<sq>)}} where {{<sq>}} is a substrate query term. If the query is successful to produce results, it is interpreted as a known infon, otherwise it is treated as an unknown infon (i.e., an infon which is not part of the principal knowledge).

If {{X}} is an integer variable {{r}} is an infon relation with a single integer argument, then the following are legal unjustified infon terms:
* {{r(X)}}
* {{r(1) && r(2) -> r(3)}}

## {anchor:syntaxBasicJustifiedInfon} Justified Infon Terms
Justified infon terms are constructed using:
* {{<i> [<e>](_e_)}} where {{<i>}} is an infon term and {{<e>}} is an evidence term. 

If {{david}} is a principal and {{E}} is an evidence variable, then the following is a legal justified infon term:
* {{david said r(27) [E](E)}}

## {anchor:syntaxBasicEvidence} Evidence Terms
Evidence terms are automatically constructed by the engine when justified communication is used (see [action terms](#syntaxBasicAction)). In order to pattern match against evidence, the following operations on evidence terms are provided:
* **Conjunction:** {{<e1> and <e2>}} where both {{<e1>}} and {{<e2>}} are evidence terms.

## {anchor:syntaxBasicPrincipal} Principal Terms
Principal terms are of two forms:
* **Principal literals:** any identifier starting with lower case such as {{org1}} or {{keyMgr}} is interpreted as the name of a principal.
* **me:** {{me}} is a variable of type {{Dkal.Principal}} which is resolved in run-time to point to the name of the principal that installed the policy where it was found. 

## {anchor:syntaxBasicQuant} Quantified Infon Terms
Quantified terms are constructed using the following syntax:
{{ 
forall <v1>: <t1>, ..., <vn>: <tn> <infon> 
}}
Where <v1>, ..., <vn> are variable names, <t1>, ..., <tn> are types and <infon> is infon term with <v1>..<vn> variables.
The difference between "with" variables and "forall" variables is that "with" variables will be instantiated (engine will try to find substitution for them), but "forall" variables will not (engine will try to derive <infon> for all values of variables <v1>...<vn>).

Quantified Infon Terms correctly works only on Propositional Primal Infon Logic based [Logic Engines](Logic-Engines) (BPIL, SPIL, TPIL).

## {anchor:syntaxBasicKA} Knowledge Assertions
Knowledge assertion are constructed using the following syntax:
{{
knows <infon>
}} Where {{<infon>}} is an infon term. 
Knowledge assertions are initial knowledge of a principal.

## {anchor:syntaxBasicRule} Rule Terms
Rules are constructed using the following syntax:
{{
<condition> do <action>
}} Where {{<condition>}} is an optional condition term and {{<action>}} is an action term.
If {{r}} is an infon relation with a single integer argument, then the following are legal rule terms:
{{
with P: Dkal.Principal, X: System.Int32
	upon 
		P said r(X)
	do
		send to P:
			r(X)
			
do
	send to me:
		r(0)
		
with P: Dkal.Principal, E: Dkal.Evidence, X: System.Int32
	if 
		P said r(X) [E](E)
	do
		send to P:
			r(1)
}}
**Important:** rules are executed as follows:
# The condition is evaluated and a set of substitutions that satisfy the condition is computed. These substitution may assign values to free variables appearing in the conditions.
# For each substitution {{s}} from the previous step, {{s}} is applied to the action of the rule and the resulting (concrete) action is executed.

## {anchor:syntaxBasicCondition} Condition Terms
There are three types of conditions: [upon conditions](#syntaxBasicConditionUpon), [if conditions](#syntaxBasicConditionIf) and [combined conditions](#syntaxBasicConditionCombined).

### {anchor:syntaxBasicConditionUpon} Upon Conditions
**Upon conditions** are patterns that are matched against the incoming messages. There is no logical reasoning involved. For instance, if a principal receives {{r(1)}} and also {{r(1) -> r(2)}} then trying to check an upon condition over {{r(2)}} will fail since the principal did not receive such a message. The syntax for upon conditions is:
{{
upon <i>
}} Where {{<i>}} is an abstract infon term.
For instance, if {{david}} is a principal, {{X}} is an integer variable, {{E}} is an evidence variable and {{r}} is an infon relation with a single integer argument, the following are legal upon conditions:
* {{upon david said r(X)}}
* {{upon r(X) [E](E)}}

Incoming messages are treated differently depending on whether they carry evidence. A message {{i}} with evidence {{e}} is accepted and matched as {{i}}, regardless of who sent it. A messages {{i}} with no evidence is accepted as {{p said i}} where {{p}} is the sender.

For instance, a message {{r(2)}} from {{david}} will match the first rule, but not the second. A message {{r(2)}} from {{david}} accompanied with evidence will match any of the two rules.

Optionally, in the case of upon conditions that match evidence, we can specify a pattern for the sender of the message, using the following syntax:
{{
upon <i> from <p>
}} Where <i> is a {{JustifiedInfon}} term and <p> is a principal term.
For instance:
* {{upon r(X) [E](E) from david}}

**IMPORTANT:** If you wish to persist an incoming message (e.g., to use it later in another rule) you need to explicitly move it to the infostrate via the [learn action](#syntaxBasicActionLearn).

### {anchor:syntaxBasicConditionIf} If Conditions
**If conditions** are targets to be derived by the logic engine from the current knowledge of the principal. If a principal has learned both {{r(1)}} and also {{r(1) -> r(2)}} then trying to check an if condition over {{r(2)}} will succeed.

Unlike upon conditions which work over the incoming infons, if conditions work over infons that have been explicitly learned in the past and are part of the current knowledge base. The syntax for if conditions is:
{{
if <i>
}} Where {{<i>}} is an abstract infon term.
For instance:
* {{if r(22) && r(23)}}
* {{if r(X) [E](E)}}

### {anchor:syntaxBasicConditionCombined} Combined Conditions
Finally, conditions can be combined to form more complex conditions. For instance:
{{ 
upon 
	david said r(X)
if 
	r(X)
upon
	john said r(0)
}} This condition is satisfied when the three atomic parts succeed.

## {anchor:syntaxBasicAction} Action Terms
There are many different actions that can be used in rules:
* The **send** action is used to send a message to a principal. The message is sent as is.
The syntax for the send action is as follows:
{{
send to <p>: <i>
}} Where <p> is a principal term and <i> is an abstract infon term. The message that is sent to {{<p>}}   is   {{<q> said <i>}}, where {{<q>}} is the principal executing the action.

* The **justified say** action is used to send a quoted message to a principal. The syntax for this action is as follows:
{{
say with justification to <p>: <i>
}} Where {{<p>}} is a principal term and {{<i>}} is an unjustified infon term. 
The actual message that is sent to {{<p>}} is constructed as {{<q> said <i> [<e>](_e_)}}, where {{<q>}} is the principal executing the action, and {{<e>}} is a digital signature that {{<p>}} can use to verify that it was {{<q>}} the person who effectively said {{<i>}}.

* The **justified send** action is used to send a justified message to a principal. The syntax for this action is as follows:
{{
send with justification to <p>: <i>
}} Where {{<p>}} is a principal term and {{<i>}} is an unjustified infon term. If {{<q>}} is the principal executing the action then {{<i>}} must be an infon of the form {{<i1> -> <q> said <i2>}}. This action is used to let {{<p>}} act as if {{<q>}} said {{<i2>}} but only when {{<i1>}} holds.

The actual message that is sent to {{<p>}} is constructed as {{<i> [<e>](_e_)}}, where {{<e>}} is a digital signature that {{<p>}} can use to verify that it was {{<q>}} the person who effectively sent {{<i>}}.

* {anchor:syntaxBasicActionLearn} The **learn** action is used to add an infon to the principal's infostrate (or knowledge base). The syntax for this action is as follows:
{{
learn <i>
}} Where {{<i>}} is an abstract infon term.

* The **forget** action is used to remove an infon from the principal's infostrate (or knowledge base). The syntax for this action is as follows:
{{
forget <i>
}} Where {{<i>}} is an abstract infon term.
Note that the **forget** action only removes infons from the principal's infostrate, but does not entail removing the infon's knowledge itself. For example, a principal's infostrate may contain infons
{{
p, q, p -> q
}}
In the case that the **forget** action is used to forget {{q}}, the infon will be removed from the infostrate, but {{q}} will still be derivable as true.

* The **install** action is used to add a rule to the principal's policy. The syntax for this action is as follows:
{{
install <r>
}} Where {{<r>}} is a rule term.

* The **uninstall** action is used to remove a rule from the principal's policy. The syntax for this action is as follows:
{{
uninstall <r>
}} Where {{<r>}} is a rule term.

* The **apply** action is used to execute an update on a data source. The syntax for this action is as follows:
{{
apply <sut>
}} Where {{<sut>}} is a substrate update term.

* Actions can be combined in order to form more complex actions. For instance:
{{
send to david: r(23)
send with justification to peter: r(1) -> me said r(23)
forget r(23)
}} This is an action which results from executing the 3 atomic actions.

## {anchor:syntaxBasicSubstrateQuery} Substrate Query Terms
Substrate query terms are used to query data stored in different kinds of data sources. This type of term is constructed using the following syntax:
{{
{| <namespace> | <query> |}
}} Where {{<namespace>}} is a string literal that is used to identify the data source on which the query is performed and {{<query>}} contains the actual query, whose syntax depends on the kind of data source pointed by {{<namespace>}}.
* If {{<namespace>}} points to a {{sql}} data source, then {{<query>}} must be a boolean expression which may contain references to columns in the database. For instance:
	* {{(records.id = 27 || records.id = 28) && X = records.patientAge}} instantiates the integer variable {{X}} with the age of patients whose record ID is either 27 or 28.
	* {{Y = X + 1}} instantiates the integer variable {{Y}} with the result of incrementing {{X}} by 1, provided that {{X}} has already been assigned a value by a condition.

* If {{<namespace>}} points to an {{xml}} data source, then {{<query>}} must be of the following form:
{{
<xpath> | <assignments>
}} Where <xpath> is a string literal containing an [XPath expression](http://www.w3.org/TR/xpath/) (which can potentially refer to DKAL variables by prepending a {{$}} sign to their name) and <assignments> defines how to map the results from the XPath expression to DKAL variables. For instance:
	* {{"//record[@id = $ID or @id = 28](@id-=-$ID-or-@id-=-28)/@patientAge" | X}} instantiates the variable {{X}} with the age of patients whose record ID is equal to the value of the integer variable {{ID}} or equal to 28.
	* {{"//record[@id = 27 or @id = 28](@id-=-27-or-@id-=-28)" | X <-> "patientAge"}} performs the same query as the previous one.
	* {{"//studies/*" | KIND, X <-> "cost"}} instantiates the {{KIND}} string variable with the name of each child node of the "studies" node. It also instantiates the {{X}} integer variable with the "cost" attribute of such node.

* If {{<namespace>}} is "basic" then the basic data source is used. This data source understands arithmetic and logical operations over ground values. For instance:
	* {{Y := X + 1}} instantiates the integer variable {{Y}} with the result of incrementing {{X}} by 1, provided that {{X}} has already been assigned a value by a condition.
	* {{COST < 1000}} succeeds if the {{COST}} integer variable (which must be already instantiated) has value less than 1000.

## {anchor:syntaxBasicSubstrateUpdate} Substrate Update Terms
Substrate update terms are used to express operations that make modifications over the data stored in a data source. Their syntax is as follows:
{{
{| <namespace> | <update> |}
}} Where {{<namespace>}} is a string literal that is used to identify the data source on which the update is executed and {{<update>}} contains the actual update details, whose syntax depends on the kind of data source pointed by {{<namespace>}}.

Currently, only {{sql}} data sources support update operations ({{xml}} updates support is experimental). Row insertion, deletion and modification are provided by means of the following constructs:
* Row insertions are described using the following syntax:
{{
<c1> := <v1>, ..., <cn> := <vn> | insert <table>
}} Where {{<table>}} is the table on which the insertion is performed; {{<c1>}}, ..., {{<cn>}} are the (unqualified) column names; and {{<v1>}}, ..., {{<vn>}} are the terms with the adequate types (according to the column values) used to populate the new row.

For instance:
{{
name := "Peter", age := 29 | insert patients
}}

* Row deletions are described using the following syntax:
{{
<query> | delete <table>
}} Where {{<table>}} is the table on which the deletion is performed and {{<query>}} specifies the condition that rows need to satisfy in order to be deleted. The {{<query>}} uses the same language as {{sql}} [substrate query terms](#syntaxBasicSubstrateQuery).

For instance:
{{
patients.age < -1 | delete patients
}}

* Row modifications are described using the following syntax:
{{
<query> | update <tc1> := <v1>, ..., <tcn> := <vn>
}} Where {{<query>}} specifies the condition that rows need to satisfy in order to be modified; {{<tc1>}}, ..., {{<tcn>}} are the qualified column names; and {{<v1>}}, ..., {{<vn>}} are the terms with the adequate types (according to the column values) that give new values to each column. The {{<query>}} uses the same language as {{sql}} [substrate query terms](#syntaxBasicSubstrateQuery).

For instance:
{{
patients.age < 6 | update patients.age := patients.age + 1
}}

## {anchor:syntaxBasicOtherLiterals} Other Literal Terms
The following literals are supported:
* **String** literals are expressed using double quotes. For instance {{"Hello world!"}}.
* **Integer** and **floating point** literals are expressed as usual.