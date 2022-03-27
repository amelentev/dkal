This project was imported from dkal.codeplex.com

# Project Description
DKAL (Distributed Knowledge Authorization Language) is a distributed authorization policy language. This project contains an engine for running DKAL policies. It is implemented primarly in F#.

Try DKAL online in your browser: [http://www.rise4fun.com/dkal](http://www.rise4fun.com/dkal)

## News
* +2014/09/01+:
	* New paper about SPIL - ["Primal Infon Logic with Conjunctions as Sets"](http://research.microsoft.com/en-us/um/people/gurevich/Opera/221.pdf) published in [TCS2014](http://www.easyconferences.eu/tcs2014/).
* +2014/05/04+:
	* SPIL [Logic Engine](<docs/Logic Engines.md>) now is based on new [randomized algorithm](http://dkal.codeplex.com/SourceControl/latest#ModularEngine/src/LogicEngine.PPIL/SPIL.fs) with O(N) average complexity for all inputs.
	* Previous SPIL Logic Engines implementations are available on names "SPILsufarr" and "SPILhash"
	* [http://www.rise4fun.com/dkal](http://www.rise4fun.com/dkal) updated.
* +2013/9/1+:
	* New [Logic Engines](<docs/Logic Engines.md>)!
		* UFOL engine allows for expressing rules and actions in the universal fragment of First Order Logic; most noticeable is the ability to use negation and disjunction operators.
		* Datalog engine takes advantage of translating PIL to Datalog using Z3 as a backend (and in the future, XSB as well)
* +2012/10/21+:
	* [release:DKAL v1.0.0.613 released](96609)
* +2012/8/29+:
	* Try updated DKAL with syntax highlighting on [rise4fun](http://www.rise4fun.com/dkal)
	* New Propositional Primal Infon Logic based [Logic Engines](<docs/Logic Engines>): BPIL, SPIL, TPIL
	* You can select logic engine in first line of [multi-policy file](SyntaxMultiPolicy).
	* [Quantified Infon Terms](SyntaxBasic#syntaxBasicQuant) ("forall")
	* 'do once' construct was removed. [Knowledge Assertions](SyntaxBasic#syntaxBasicKA) ('knows') introduced instead.
	* 'Me' principal constant renamed to 'me'
	* 'substrate' keyword renamed to 'datasource'
* +2011/6/9+: We added lots of [developer documentation](DeveloperDocumentation).
* +2011/6/6+: [release:DKAL v1.0.0.364 released](67844)
* +2011/6/6+: Try out DKAL live on [rise4fun](http://www.rise4fun.com/Dkal)

## Documentation
For a quick start on how to use DKAL, please refer to our documentation on:
* [Obtaining DKAL](docs/ObtainingDKAL.md)
* [Compiling DKAL](docs/CompilingDKAL.md)
* [Using DKAL](docs/UsingDKAL.md)
* [DKAL syntax](docs/Syntax.md)
* [DKAL examples](docs/Examples.md)

Otherwise, if you plan to extend or modify the DKAL engine, please visit our [developer documentation](docs/DeveloperDocumentation.md) section.

## DKAL Challenges
We have collected a [list of interesting theoretical and practical challenges](docs/Challenges.md) related to DKAL and its underlying infon logic.

## Underlying Theory
More information about DKAL, and its underlying theory, please visit our section on [suggested reading](docs/SuggestedReading.md), or go directly to [Yuri Gurevich's web page on the topic](http://research.microsoft.com/en-us/um/people/gurevich/dkal.htm).
