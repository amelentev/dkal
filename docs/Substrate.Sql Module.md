# {anchor:top} {{Substrate.Sql}} Module
----
This module defines the following types:
* [SqlConnector](Substrate.Sql-Module#SqlConnector)
* [ASqlSubstrateUpdateTerm](Substrate.Sql-Module#ASqlSubstrateUpdateTerm)
* [SqlSubstrateModifyTerm](Substrate.Sql-Module#SqlSubstrateModifyTerm)
* [SqlSubstrateDeleteTerm](Substrate.Sql-Module#SqlSubstrateDeleteTerm)
* [SqlSubstrateInsertTerm](Substrate.Sql-Module#SqlSubstrateInsertTerm)
* [SqlSubstrate](Substrate.Sql-Module#SqlSubstrate)
* [SqlCompiler](Substrate.Sql-Module#SqlCompiler)
* [SqlOp](Substrate.Sql-Module#SqlOp)
* [TableId](Substrate.Sql-Module#TableId)
* [Expr](Substrate.Sql-Module#Expr)
* [Column](Substrate.Sql-Module#Column)
* [Var](Substrate.Sql-Module#Var)
* [Const](Substrate.Sql-Module#Const)
* [Op](Substrate.Sql-Module#Op)
* [SqlWriter](Substrate.Sql-Module#SqlWriter)
* [ActivePatterns](Substrate.Sql-Module#ActivePatterns)
* [Builders](Substrate.Sql-Module#Builders)
* [SqlPrimitives](Substrate.Sql-Module#SqlPrimitives)
----
## {anchor:SqlConnector} {{SqlConnector}} Type
Keeps a connection with a SQL database given by a connection string parameter upon construction. The SqlConnector can be used to execute queries or other commands over the SQL database.

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | unit | {{Close}} |  | Closes the connection to the SQL database |
|  | SqlDataReader seq | {{ExecQuery}} | string, Object seq | Executes the given query and returs a (lazy) sequence of results |
|  | int | {{ExecUpdate}} | string, Object seq | Executes the given command and discards the results (useful for non-query commands such as updates) |
|  | [ITerm](Interfaces-Module#ITerm) | {{ReadVar}} | DbDataReader, [IVar](Interfaces-Module#IVar), int | Returns an ITerm by parsing the DbDataReader depending on the type of  the given variable |
>{[Back to top](#top)}>
----
## {anchor:ASqlSubstrateUpdateTerm} {{ASqlSubstrateUpdateTerm}} Type
An abstract SqlSubstrateUpdateTerm that is extended by modify terms, insert terms and delete terms

### Implemented Interfaces
* ITerm
* ISubstrateUpdateTerm
* ISubstrateTerm

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | [ITerm](Interfaces-Module#ITerm) | {{Apply}} | [ISubstitution](Interfaces-Module#ISubstitution) |  |
|  | [ITerm](Interfaces-Module#ITerm) | {{Normalize}} |  |  |
|  | [ISubstitution](Interfaces-Module#ISubstitution) option | {{UnifyFrom}} | [ISubstitution](Interfaces-Module#ISubstitution), [ITerm](Interfaces-Module#ITerm) |  |
| {{static}} | IDictionary<string, [ITerm](Interfaces-Module#ITerm)> | {{dictApply}} | [ISubstitution](Interfaces-Module#ISubstitution), IDictionary<string, [ITerm](Interfaces-Module#ITerm)> |  |

### Properties
|| Return type || Property name || Description ||
| [IVar](Interfaces-Module#IVar) list | {{Vars}} |  |
>{[Back to top](#top)}>
----
## {anchor:SqlSubstrateModifyTerm} {{SqlSubstrateModifyTerm}} Type
**Base type**: ASqlSubstrateUpdateTerm
A term that represents a modification over several rows in different tables  of the database.

### Implemented Interfaces
* ITerm
* ISubstrateUpdateTerm
* ISubstrateTerm

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | [ITerm](Interfaces-Module#ITerm) | {{Apply}} | [ISubstitution](Interfaces-Module#ISubstitution) |  |
|  | [ITerm](Interfaces-Module#ITerm) | {{Normalize}} |  |  |
|  | [ISubstitution](Interfaces-Module#ISubstitution) option | {{UnifyFrom}} | [ISubstitution](Interfaces-Module#ISubstitution), [ITerm](Interfaces-Module#ITerm) |  |
|  | string | {{ToString}} |  |  |
|  | bool | {{Equals}} | Object |  |
|  | int | {{GetHashCode}} |  |  |

### Properties
|| Return type || Property name || Description ||
| [ITerm](Interfaces-Module#ITerm) | {{Query}} | Indicates over which rows the update needs to be applied |
| IDictionary<string, [ITerm](Interfaces-Module#ITerm)> | {{ColsMapping}} | Indicates what columns need to be modified and what new values they need to get |
| [IVar](Interfaces-Module#IVar) list | {{Vars}} |  |
>{[Back to top](#top)}>
----
## {anchor:SqlSubstrateDeleteTerm} {{SqlSubstrateDeleteTerm}} Type
**Base type**: ASqlSubstrateUpdateTerm
A term that represents a deletion of several rows in a table on the database

### Implemented Interfaces
* ITerm
* ISubstrateUpdateTerm
* ISubstrateTerm

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | [ITerm](Interfaces-Module#ITerm) | {{Apply}} | [ISubstitution](Interfaces-Module#ISubstitution) |  |
|  | [ITerm](Interfaces-Module#ITerm) | {{Normalize}} |  |  |
|  | [ISubstitution](Interfaces-Module#ISubstitution) option | {{UnifyFrom}} | [ISubstitution](Interfaces-Module#ISubstitution), [ITerm](Interfaces-Module#ITerm) |  |
|  | string | {{ToString}} |  |  |
|  | bool | {{Equals}} | Object |  |
|  | int | {{GetHashCode}} |  |  |

### Properties
|| Return type || Property name || Description ||
| [ITerm](Interfaces-Module#ITerm) | {{Query}} | Indicates which rows are to be deleted |
| string | {{Table}} | Indicates on which table the deletion is to be performed |
| [IVar](Interfaces-Module#IVar) list | {{Vars}} |  |
>{[Back to top](#top)}>
----
## {anchor:SqlSubstrateInsertTerm} {{SqlSubstrateInsertTerm}} Type
**Base type**: ASqlSubstrateUpdateTerm
A term that represents an insertion of a single row in a table on the database

### Implemented Interfaces
* ITerm
* ISubstrateUpdateTerm
* ISubstrateTerm

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | [ITerm](Interfaces-Module#ITerm) | {{Apply}} | [ISubstitution](Interfaces-Module#ISubstitution) |  |
|  | [ITerm](Interfaces-Module#ITerm) | {{Normalize}} |  |  |
|  | [ISubstitution](Interfaces-Module#ISubstitution) option | {{UnifyFrom}} | [ISubstitution](Interfaces-Module#ISubstitution), [ITerm](Interfaces-Module#ITerm) |  |
|  | string | {{ToString}} |  |  |
|  | bool | {{Equals}} | Object |  |
|  | int | {{GetHashCode}} |  |  |

### Properties
|| Return type || Property name || Description ||
| string | {{Table}} | Indicates on which table the rows is going to be added |
| IDictionary<string, [ITerm](Interfaces-Module#ITerm)> | {{Values}} | Gives values to each of the columns in the new row |
| [IVar](Interfaces-Module#IVar) list | {{Vars}} |  |
>{[Back to top](#top)}>
----
## {anchor:SqlSubstrate} {{SqlSubstrate}} Type
A SqlSubstrate is an abstraction of a SQL database that can be used to  issue queries, and perform updates (such as rows insertions/deletions).

### Implemented Interfaces
* ISubstrate

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | bool | {{HasTable}} | string |  |
|  | Type | {{GetColumnType}} | string, string |  |
|  | bool | {{modify}} | [SqlSubstrateModifyTerm](Substrate.Sql-Module#SqlSubstrateModifyTerm) |  |
|  | bool | {{delete}} | [SqlSubstrateDeleteTerm](Substrate.Sql-Module#SqlSubstrateDeleteTerm) |  |
|  | bool | {{insert}} | [SqlSubstrateInsertTerm](Substrate.Sql-Module#SqlSubstrateInsertTerm) |  |

### Properties
|| Return type || Property name || Description ||
| string | {{ConnectionString}} |  |
| string | {{SchemaFile}} |  |
>{[Back to top](#top)}>
----
## {anchor:SqlCompiler} {{SqlCompiler}} Type
The SqlCompiler has functionality to transform SQL query terms into real SQL queries that can be executed on a SqlConnector.

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
| {{static}} | Dictionary<a, b> | {{dict}} |  |  |
| {{static}} | List<a> | {{vec}} |  |  |
| {{static}} | unit | {{addSqlOp}} | string, string |  |
| {{static}} | unit | {{addPrefixSqlOp}} | string, string |  |
| {{static}} | unit | {{init}} |  |  |
| {{static}} | [Expr](Substrate.Sql-Module#Expr) | {{sqlEq}} | [Expr](Substrate.Sql-Module#Expr), [Expr](Substrate.Sql-Module#Expr) |  |
| {{static}} | [Expr](Substrate.Sql-Module#Expr) | {{sqlNeq}} | [Expr](Substrate.Sql-Module#Expr), [Expr](Substrate.Sql-Module#Expr) |  |
| {{static}} | [Expr](Substrate.Sql-Module#Expr) | {{sqlAnd}} | [Expr](Substrate.Sql-Module#Expr), [Expr](Substrate.Sql-Module#Expr) |  |
| {{static}} | unit | {{err}} | [ITerm](Interfaces-Module#ITerm), string |  |
| {{static}} | [Expr](Substrate.Sql-Module#Expr) * [Variable](Ast-Module#Variable) * [Expr](Substrate.Sql-Module#Expr) list | {{simplify}} | [Expr](Substrate.Sql-Module#Expr) |  |
| {{static}} | [Expr](Substrate.Sql-Module#Expr) * [Variable](Ast-Module#Variable) * [Expr](Substrate.Sql-Module#Expr) list | {{compile}} | a, b, [ITerm](Interfaces-Module#ITerm) seq |  |
| {{static}} | [ISubstitution](Interfaces-Module#ISubstitution) seq | {{execQuery}} | [SqlConnector](Substrate.Sql-Module#SqlConnector), a, [Expr](Substrate.Sql-Module#Expr) * [Variable](Ast-Module#Variable) * [Expr](Substrate.Sql-Module#Expr) list, [ISubstitution](Interfaces-Module#ISubstitution), [IVar](Interfaces-Module#IVar) list |  |
| {{static}} | bool | {{execUpdate}} | [SqlConnector](Substrate.Sql-Module#SqlConnector), a, [Expr](Substrate.Sql-Module#Expr) * [Variable](Ast-Module#Variable) * [Expr](Substrate.Sql-Module#Expr) list, string * [Expr](Substrate.Sql-Module#Expr) list |  |
| {{static}} | bool | {{execDelete}} | [SqlConnector](Substrate.Sql-Module#SqlConnector), a, [Expr](Substrate.Sql-Module#Expr) * [Variable](Ast-Module#Variable) * [Expr](Substrate.Sql-Module#Expr) list, string |  |
| {{static}} | bool | {{execInsert}} | [SqlConnector](Substrate.Sql-Module#SqlConnector), a, string, IDictionary<string, [Expr](Substrate.Sql-Module#Expr)> |  |

### Properties
|| Return type || Property name || Description ||
| Logger | {{log}} |  |
| Dictionary<string, [SqlOp](Substrate.Sql-Module#SqlOp)> | {{sqlOps}} |  |
| [Expr](Substrate.Sql-Module#Expr) | {{sqlTrue}} |  |
| [Expr](Substrate.Sql-Module#Expr) | {{sqlFalse}} |  |
| [Expr](Substrate.Sql-Module#Expr) seq -> [Expr](Substrate.Sql-Module#Expr) | {{sqlMultiAnd}} |  |
>{[Back to top](#top)}>
----
## {anchor:SqlOp} {{SqlOp}} Type
Type to represent a SQL operator

### Implemented Interfaces
* IEquatable<SqlOp>
* IStructuralEquatable
* IComparable<SqlOp>
* IComparable
* IStructuralComparable

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | int | {{CompareTo}} | [SqlOp](Substrate.Sql-Module#SqlOp) |  |
|  | int | {{CompareTo}} | Object |  |
|  | int | {{CompareTo}} | Object, IComparer |  |
|  | int | {{GetHashCode}} | IEqualityComparer |  |
|  | int | {{GetHashCode}} |  |  |
|  | bool | {{Equals}} | Object, IEqualityComparer |  |
|  | bool | {{Equals}} | [SqlOp](Substrate.Sql-Module#SqlOp) |  |
|  | bool | {{Equals}} | Object |  |

### Properties
|| Return type || Property name || Description ||
| string | {{name}} |  |
| bool | {{infix}} |  |
>{[Back to top](#top)}>
----
## {anchor:TableId} {{TableId}} Type
Type to represent a SQL table. The scope is used to distinguish between tables that are in a nested query and its results therefore need to be  treated independently

### Implemented Interfaces
* IEquatable<TableId>
* IStructuralEquatable
* IComparable<TableId>
* IComparable
* IStructuralComparable

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | int | {{CompareTo}} | [TableId](Substrate.Sql-Module#TableId) |  |
|  | int | {{CompareTo}} | Object |  |
|  | int | {{CompareTo}} | Object, IComparer |  |
|  | int | {{GetHashCode}} | IEqualityComparer |  |
|  | int | {{GetHashCode}} |  |  |
|  | bool | {{Equals}} | Object, IEqualityComparer |  |
|  | bool | {{Equals}} | [TableId](Substrate.Sql-Module#TableId) |  |
|  | bool | {{Equals}} | Object |  |

### Properties
|| Return type || Property name || Description ||
| int | {{scope}} |  |
| string | {{name}} |  |
>{[Back to top](#top)}>
----
## {anchor:Expr} {{Expr}} Type
Type to represent SQL expressions.

### Implemented Interfaces
* IEquatable<Expr>
* IStructuralEquatable

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
| {{static}} | [Expr](Substrate.Sql-Module#Expr) | {{NewOp}} | [SqlOp](Substrate.Sql-Module#SqlOp), [Expr](Substrate.Sql-Module#Expr) list |  |
| {{static}} | [Expr](Substrate.Sql-Module#Expr) | {{NewConst}} | [IConst](Interfaces-Module#IConst) |  |
| {{static}} | [Expr](Substrate.Sql-Module#Expr) | {{NewVar}} | [Variable](Ast-Module#Variable) |  |
| {{static}} | [Expr](Substrate.Sql-Module#Expr) | {{NewColumn}} | [TableId](Substrate.Sql-Module#TableId), string |  |
|  | int | {{GetHashCode}} | IEqualityComparer |  |
|  | int | {{GetHashCode}} |  |  |
|  | bool | {{Equals}} | Object, IEqualityComparer |  |
|  | [Expr](Substrate.Sql-Module#Expr) | {{Map}} | [Expr](Substrate.Sql-Module#Expr) -> [Expr](Substrate.Sql-Module#Expr) option |  |
|  | bool | {{IsGround}} |  |  |
|  | string | {{ToString}} |  |  |
|  | [Expr](Substrate.Sql-Module#Expr) | {{Subst}} | Dictionary<string, [Expr](Substrate.Sql-Module#Expr)> |  |
|  | bool | {{Equals}} | [Expr](Substrate.Sql-Module#Expr) |  |
|  | bool | {{Equals}} | Object |  |

### Properties
|| Return type || Property name || Description ||
| int | {{Tag}} |  |
| bool | {{IsOp}} |  |
| bool | {{IsConst}} |  |
| bool | {{IsVar}} |  |
| bool | {{IsColumn}} |  |
>{[Back to top](#top)}>
----
## {anchor:Column} {{Column}} Type
**Base type**: Expr
Table access

### Implemented Interfaces
* IEquatable<Expr>
* IStructuralEquatable

### Properties
|| Return type || Property name || Description ||
| [TableId](Substrate.Sql-Module#TableId) | {{Item1}} |  |
| string | {{Item2}} |  |
>{[Back to top](#top)}>
----
## {anchor:Var} {{Var}} Type
**Base type**: Expr
Variable

### Implemented Interfaces
* IEquatable<Expr>
* IStructuralEquatable

### Properties
|| Return type || Property name || Description ||
| [Variable](Ast-Module#Variable) | {{Item}} |  |
>{[Back to top](#top)}>
----
## {anchor:Const} {{Const}} Type
**Base type**: Expr
Constant

### Implemented Interfaces
* IEquatable<Expr>
* IStructuralEquatable

### Properties
|| Return type || Property name || Description ||
| [IConst](Interfaces-Module#IConst) | {{Item}} |  |
>{[Back to top](#top)}>
----
## {anchor:Op} {{Op}} Type
**Base type**: Expr
Operation

### Implemented Interfaces
* IEquatable<Expr>
* IStructuralEquatable

### Properties
|| Return type || Property name || Description ||
| [SqlOp](Substrate.Sql-Module#SqlOp) | {{Item1}} |  |
| [Expr](Substrate.Sql-Module#Expr) list | {{Item2}} |  |
>{[Back to top](#top)}>
----
## {anchor:SqlWriter} {{SqlWriter}} Type
A SqlWriter prints SQL Expr elements into SQL syntax using a StringBuilder

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
|  | unit | {{pr}} | Object |  |
|  | string | {{get}} | string |  |
|  | unit | {{parm}} | Object |  |

### Properties
|| Return type || Property name || Description ||
| IDictionary<[TableId](Substrate.Sql-Module#TableId), string> | {{tables}} |  |
| List<Object> | {{parms}} |  |
| string | {{fromClause}} |  |
| [Expr](Substrate.Sql-Module#Expr) -> unit | {{print}} |  |
>{[Back to top](#top)}>
----
## {anchor:ActivePatterns} {{ActivePatterns}} Type
Defines the public interface on how to pattern match AST elements defined for the SqlSubstrate

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
| {{static}} | [ITerm](Interfaces-Module#ITerm) list option | {{|AndBool|_|}} | [ITerm](Interfaces-Module#ITerm) | Matches a conjunction |
| {{static}} | [ITerm](Interfaces-Module#ITerm) list option | {{|OrBool|_|}} | [ITerm](Interfaces-Module#ITerm) | Matches a disjunction |
| {{static}} | [ISubstrateQueryTerm](Interfaces-Module#ISubstrateQueryTerm) option | {{|AsBoolean|_|}} | [ITerm](Interfaces-Module#ITerm) | Matches an asBoolean construct (used to nest queries) |
| {{static}} | string * string option | {{|Column|_|}} | [ITerm](Interfaces-Module#ITerm) | Matches a table.column term |
>{[Back to top](#top)}>
----
## {anchor:Builders} {{Builders}} Type
Defines the public interface on how to construct AST elements defined in the SqlSubstrate

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{AndBool}} | [ITerm](Interfaces-Module#ITerm) list | Constructs a conjunction |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{OrBool}} | [ITerm](Interfaces-Module#ITerm) list | Constructs a disjunction |
| {{static}} | [ITerm](Interfaces-Module#ITerm) | {{AsBoolean}} | [ISubstrateQueryTerm](Interfaces-Module#ISubstrateQueryTerm) | Constructs an AsBoolean expression (used for nested substrate queries) |
>{[Back to top](#top)}>
----
## {anchor:SqlPrimitives} {{SqlPrimitives}} Type
Defines the primitive functions used to construct Application elements in the SqlSubstrate builders

### Methods
|| Modifiers || Return type || Method name || Method parameters || Description ||
| {{static}} | [Function](Ast.Tree-Module#Function) option | {{SolveOverloadOperator}} | string, [IType](Interfaces-Module#IType) | Given an overloaded function name and the type of one of its parameters it looks the type information to see if there is a match. |
>{[Back to top](#top)}>
----
>{Automatically generated on 6/10/2011 2:28:40 PM}>
