# Logic Engines

DKAL have the following logic engines:

* "simple" - prolog-like engine with some simple 2nd order infon logic support. linear but incomplete.
* based on Propositional Primal Infon Logic (complexity in propositional case):
	* BPIL = [Basic Primal Infon Logic](http://research.microsoft.com/en-us/um/people/gurevich/Opera/215.pdf). Linear.
	* SPIL = BPIL + Conjunctions as sets. Implementations:
		* SPIL - randomized algorithm. Linear on average for all inputs. ([SPILrnd](http://dkal.codeplex.com/SourceControl/latest#ModularEngine/src/LogicEngine.PPIL/SPIL.fs))
		* SPILsufarr - deterministic algorithm based on suffix arrays. O(depth*length) worst case time complexity.
		* SPILhash - deterministic algorithm based on hashing. Linear on average.
	* TPIL - [BPIL + transitive rule](http://research.microsoft.com/en-us/um/people/gurevich/Opera/211.pdf). Quadratic time complexity.
	* TSPIL - SPIL + transitive tule. Cubic time complexity.
* UFOL - allows for expressions in the universal fragment of first order logic. Negation and disjunction are allowed, but be aware that failure to infer a given formula is not equivalent to ability to infer its negation. In the same tone, ability to infer a disjunction is not equivalent to ability to infer any of its disjuncts.
* [datalog](http://research.microsoft.com/en-us/um/people/gurevich/Opera/208.pdf) - the datalog engine takes advantage of Propositional Primal Infon Logic translation to Datalog, and it employs Z3's datalog solver as backend.