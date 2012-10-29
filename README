Tests: 

So far we pass 7 tests of the provided 90: the 4 lambda?.py tests in scope, and in types test_booleans.py, test_comparisons.py, and test_floats.py. Hopefully, this number will increase in the near future, perhaps even before the design check. 


Design: 

Our core language consists of a set of types similar to that provided in parseltongue, with a few additions (a type corresponding to that "pass" instruction, for instance). 

Our syntax language has something close to one type for each of the types in the AST python specification. Basically, everything that is parsed as a different type goes to a different clause in the desugarer.

After analyzing how python scope works, we chose to implement ours with an environment and a store. Our store is just as the one taught in class, but our environment goes from an identifier to a (ScopeType * Location), with ScopeType being either 'Local', 'NonLocal' or 'Global'. This information is useful because there's a lot of perks related to this in python.

One interesting decision is that we have to treat the 'global scope' differently from the others, with a case in the desugarer that handles just this (and is, therefore, executed just once). We'd like to talk about this, because there may be a more beautiful way of achieving what we want.

We've also implemented a "VUnbound" type, which represents an unbound variable (but is never actually returned --- it is used for error checking) --- we want to discuss this as well, to make sure that it is a viable choice. 
