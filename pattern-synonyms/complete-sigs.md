
The exhaustiveness checker currently chokes on pattern synonyms. 
They are marked as always fallible patterns which means that we must also always include a catch-all case in order to avoid a warning.

```
dataA=Apattern::ApatternP=Afoo::A->AfooP=A
```


leads to the warning that pattern matches for `foo` are non-exhaustive.

```wiki
simpletest.hs:7:1: warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In an equation for ‘foo’: Patterns not matched: _
```


Inspecting the definition of `P` we can see that the matches for `foo` are indeed exhaustive as `A` is a unary data type but the pattern match checker does not make use of this information.


And neither should it! Pattern synonyms are a means of \*abstraction\*, if the exhaustiveness checker could look through a definition then the implementation of `P` would leak into error messages. 
We want users to be able to replace bona-fide data constructors with pattern synonyms without consumers noticing. 
To that end, we allow users to specify a complete set of pattern synonyms in order to sate the pattern match checker. If a complete pragma is not provided then we keep the same behaviour as in previous releases.

# Definitions


We introduce a new top-level pragma which we use to declare a complete set of conlikes.


A **conlike** is either a data constructor or a pattern synonym.


We say that a set of conlikes is a **complete** match when a function which is defined by matching on all of the conlikes is total.

# Syntax


A user declares a complete match using a `COMPLETE` pragma. The definition consists of a list of conlikes. 

```
{-# COMPLETE con_1, ..., con_n #-}
```

`COMPLETE` pragmas can appear in any module in which `con_1,..., con_n` are in scope. Specifically, they need not be in the defining module.

# Semantics

`COMPLETE` pragmas are only used in the pattern match checker. 


For a given `COMPLETE` pragma, for a complete matching `c`, if a user writes a function which matches on all constructors in `c` then we consider the function to be total and the pattern match checker should not emit a warning.

`COMPLETE` pragmas are always imported and exported from modules. 

## Typing


We verify that the result types of each constructor in a complete match agrees with each other. This is a sanity check as users would never be able to match on all constructors if the set of patterns is inconsistent in this manner.

# Examples


The following examples should emit no warnings.

```
patternP::ApatternP=A{-# COMPLETE P #-}foo::A->AfooP=A
```

```
patternN::Maybe a
patternN=Nothing{-# COMPLETE N, Just #-}qux::Maybe a ->BoolquxN=Falsequx(Just x)=True
```

# Discussion


Obviously this feature allows nefarious users to define any set of patterns as complete. This is a consequence of the flexibility of the design.

## Error Messages


The pattern match checker checks each set of patterns individually and combines the results together at the end. The current naive algorithm tries to choose the result where the set of redundant and uncovered matches are both empty. If there is no such set then it prefers results with few uncovered cases, ties being broken by the number of redundant clauses. 


We write `C: R<n> U<m>` for the result of matching `C` which has `n` redundant matches and `m` uncovered cases. 


For example, for two complete matchings `C1` and `C2`, then 

<table><tr><th> Match Result 1       </th>
<th> Match Result 2       </th>
<th> Chosen Result </th>
<th> Reason 
</th></tr>
<tr><th>`C1: R<0> U<0>`</th>
<th>`C2: R<n> U<m>`</th>
<th>`C1`</th>
<th>`C1` is a perfect matching so the function is total. 
</th></tr>
<tr><th>`C1: R<1> U<1>`</th>
<th>`C2: R<0> U<2>`</th>
<th>`C1`</th>
<th>`C1` has fewer uncovered clauses. 
</th></tr>
<tr><th>`C1: R<2> U<0>`</th>
<th>`C2: R<1> U<0>`</th>
<th>`C2`</th>
<th>`C1` and `C2` have the same number of uncovered clauses but `C2` has fewer redundant clauses 
</th></tr></table>


It may be desirable to produce a summary of the results in a more intelligent way. Exploration of this is left open for further discussion.
