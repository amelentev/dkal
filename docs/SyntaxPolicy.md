# Policy Language Syntax Guide
A DKAL policy specifies how a principal behaves and interacts with the other principals. It indicates what to communicate and when, who to trust, what data sources to use, etc.

DKAL policies have a ".dkal" file extension and contain a [header with declarations](#syntaxPolicyHeader) and a set of initial [rules](#syntaxPolicyRules).

## {anchor:syntaxPolicyHeader} DKAL Policy Header
A DKAL policy header contains the following type of statements:
* [Data sources declarations](#syntaxPolicyHeaderSubstrate)
* [Relation declarations](#syntaxPolicyHeaderRelation)
* [Type renames](#syntaxPolicyHeaderTypeRename)
* [Macro declarations](#syntaxPolicyHeaderMacro)

### {anchor:syntaxPolicyHeaderSubstrate} Data Sources Declarations
Data sources declarations allow the engine to query and modify data from different providers such as XML files, SQL Server databases, etc.

A data source declaration is defined as follows:
{{
datasource <kind>(<argList>) namespaces <namespacesList>
}} Where:
* {{<kind>}} is the data source kind. Currently supported data source kinds are {{sql}} and {{xml}}.
* {{<argList>}} is a list of string literals which provide parameters used to connect to the given data source kind, as the following table indicates.
|| Data source kind || Parameters || Description ||
| {{sql}} | {{connectionString}}, {{schemaFile}} | The {{connectionString}} parameter is the [SQL Server connection string](http://msdn.microsoft.com/en-us/library/ms254978.aspx) used to connect to the SQL Server database. The {{schemaFile}} parameter must point to a valid SQL Server schema file (e.g., generated with [SqlMetal.exe](http://msdn.microsoft.com/en-us/library/bb386987.aspx)) for the SQL Server database.  |
| {{xml}} | {{xmlFileName}} or {{xmlContent}} | In this case only a single parameter is expected. It can either be the name of a file containing an XML document, or it can be a string containing an XML document itself. |
* {{<namespaceList>}} is a list of literal strings that act as namespaces that will be served by this data source. Queries and data source updates will have a namespace, which the engine uses to identify a data source that understands such namespace.

For example:
{{
dataprovider xml("<clients><client name='peter' balance='20'/></clients>") namespaces "clients"
dataprovider xml("keys.xml") namespaces "secretKeys"
dataprovider sql("Server=localhost,1136;Database=Transactions;User ID=foo;Password=bar", "schema.dbml") namespaces "creditCardTransactions"
}} 

### {anchor:syntaxPolicyHeaderRelation} Relation Declarations
Relation declarations are used to construct new sorts of infons. For instance, in a banking scenario it may be desirable to have an infon relation {{balanceOf}} to refer to the balance of a given principal. This can be added by using the following declaration:
{{
relation balanceOf(P: Principal, AMOUNT: System.Int32)
}} 

After declaring such an infon relation, we may now construct the following infons:
* {{balanceOf(Me, 33)}}
* {{peter said balanceOf(peter,10000)}}

In general, relations are declared as follows:
{{
relation <relationName>(<argName1>: <argType1>, <argName2>: <argType2>, ...)
}} Where each {{<argNameX>>}} is the parameter formal name and {{<argTypeX>}} is its type.

### {anchor:syntaxPolicyHeaderTypeRename} Type Renames
In order to improve readability, DKAL allows policies to declare type renames. Type rename syntax is as follows:
{{
type <newTypeName> = <oldTypeName>
}} Where <newTypeName> is an uppercase identifier that will become a new name to refer to the already declared <oldTypeName>.

### {anchor:syntaxPolicyHeaderMacro} Macro Declarations
Macros are used to declare substrate queries that are used in many places in order to improve readability and maintainability of the policy. Macros are declared as follows:
{{
macro <macroName> (<p1>: <t1>, ..., <pn>: <tn>) : <t>
	<body>
}} Where:
* {{<macroName>}} is going to be the name of the macro; 
* {{<p1>}}, ..., {{<pn>}} are its formal parameters (with types {{<t1>}}, ..., {{<tn>}} respectively);  
* {{<t>}} is the return type of the macro
* {{<body>}} is a [substrate query term](SyntaxBasic#syntaxBasicSubstrateQuery) that defines the intent of the macro. The {{Ret}} variable with type {{<t>}} can be used in the body to refer to the return value of the macro.

A macro can be invoked in any of the following places:
# Macros with boolean return type can be used inside an {{asInfon}} expression. 
# Macros with any return type can be used whenever a term of this type is expected. 

In any case the formal parameters are replaced by the concrete parameters in the macro body. The location where the macro is found is replaced by a fresh variable of the macro return type. This variable is instantiated by adding the concretized macro body as a condition to the surrounding rule.

For instance, if we have the following macro:
{{
macro increment(X: System.Int32) : System.Int32
	{| "basic" | Ret := X + 1 |}
}} Then it could potentially be used as follows:
{{
with P: Dkal.Principal, Z: System.Int32
	upon
		P said r(Z)
	do
		send to P: r(increment(Z))
}} And this is resolved to:
{{
with P: Dkal.Principal, Z: System.Int32, FreshVar0: System.Int32
	upon
		P said r(Z)
	if
		asInfon({| "basic" | FreshVar0 := Z + 1 |})
	do
		send to P: r(FreshVar0)
}}

## {anchor:syntaxPolicyRules} DKAL Rules
Once relation, types and macros are declared, the DKAL policy for the current principal must be defined. Each rule found in the file is going to be installed as part of the initial policy. This policy may evolve over time if new rules are installed or uninstalled.