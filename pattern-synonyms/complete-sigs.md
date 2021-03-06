
The exhaustiveness checker currently chokes on pattern synonyms. 
They are marked as always fallible patterns which means that we must also always include a catch-all case in order to avoid a warning.


```
data A = A

pattern :: A
pattern P = A

foo :: A -> A
foo P = A
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


Optionally a user may also specify the type constructor of the patterns.

```
{-# COMPLETE con_1, ..., con_n :: T #-}
```

# Semantics

`COMPLETE` pragmas are only used in the pattern match checker. 


For each `COMPLETE` pragma we identify the type of the set of conlikes. 


For a `COMPLETE` pragma of type `T` with conlikes `c_1, ..., c_n`. If in a pattern match of type `T` we match on `c_1, ..., c_n` then
we assume that the match is covering. This means that the pattern match checker will no longer emit a warning about incomplete pattern matches. 

`COMPLETE` pragmas are always imported and exported from modules. 

`COMPLETE` pragmas only specify that a certain match is covering, because of this, we do not warn about redundant or inaccessible matches when the pattern match checking result arises
from checking against a `COMPLETE` pragma.


Different `COMPLETE` pragmas can mention overlapping sets of conlikes. If there is more than one relevant `COMPLETE` pragma then each is tried, if none of them result in a covering match then an error is reported
as described in the "Error Reporting" section.



There are no checks to ensure that the set of patterns is actually complete or covering in any way. It is up to the user to get it right.



`COMPLETE` pragmas are a new source of orphan modules. For example,


```
module M where

import N( pattern P, pattern Q )
{-# COMPLETE P, Q #-}
```


where neither `P` nor `Q` is defined in M.  Then every module that is
transitively "above" M would need to read `M.hi` just in case its
`COMPLETE` pragmas was relevant.

## Typing


We refer to the leftmost type in the result type of a conlike as the rtc.


For example, the rtc of `P :: a -> b -> T a b` is `T`, the rtc of `P :: T` is `T` and the rtc of `P :: ... -> f Int` is a type variable `f`. 


We distinguish between polymorphic and monomorphic conlikes. 

- A monomorphic conlike is when the rtc is a type constructor.
- A polymorphic conlike is when the rtc is not a type constructor.


By the end of type checking, we must identify a type constructor to identify with each set of patterns. 


In the case where there is at least one monomorphic pattern `P`,  the rtc of `P` is the type constructor for the whole set of patterns. 
Each monomorphic conlike must have the same rtc as `P`.
There is no requirement for polymorphic conlikes.
In addition, if the user specifies the type signature then the rtc of `P` must be the same as the type constructor specified by the type signature. 


In the case where all the patterns are polymorphic, a user must provide a type signature but we accept the definition regardless of the type signature they provide. 
The type constructor for the whole set of patterns is the type constructor as specified by the user. If the user does not provide a type signature then the definition is rejected as ambiguous. 


This design is a consequence of the design of the pattern match checker. Complete sets of patterns must be identified relative to a type. 
This is a sanity check as users would never be able to match on all constructors if the set of patterns is inconsistent in this manner.


### Examples of Typing


```
pattern P :: ()
pattern P = ()

{-# COMPLETE P #-}
```


Accepted, the type of the match is `()` as the rtc of `P` is `()`.


```
pattern P :: ()
pattern P = ()

{-# COMPLETE P :: () #-}
```


Accepted, the type of the match is `()` as the rtc of `P` is `()` and this is the same as the user specified type which is `()`.


```
pattern P :: ()
pattern P = ()

{-# COMPLETE P :: Int #-}
```


Rejected as the user specified type signature is not the same as the rtc of `P`. (`Int /= ()`)


```
pattern P :: ()
pattern P = ()

pattern Q :: Maybe a
pattern Q = Nothing

{-# COMPLETE P, Q #-}
```


Rejected: the rtc of `P` and `Q` is different.


```
pattern P :: ()
pattern P = ()

pattern Q :: ()
pattern Q = ()

{-# COMPLETE P, Q #-}
```


Accepted, the type of the whole set is `()` as this is the rtc of both `P` and `Q`.


```
class C f where
    match :: f a -> ()

pattern P :: C f => f a
pattern P <- (match -> ())

{-# COMPLETE P #-}
```


Rejected. All the conlikes are polymorphic but no type signature is provided


```
class C f where
    match :: f a -> ()

pattern P :: C f => f a
pattern P <- (match -> ())

{-# COMPLETE P :: () #-}
```


Accepted. All the conlikes are polymorphic but a type signature is provided.


```
class C f where
    match :: f a -> ()

pattern P :: C f => f a
pattern P <- (match -> ())

pattern Q :: ()
pattern Q = ()

{-# COMPLETE P, Q #-}
```


Accepted. There is one polymorphic conlike `P` but there is also a monomorphic conlike `Q` which fixes the type of the whole set. Therefore the type of the whole match is `()`. 


```
pattern P :: [Int] 
pattern P = [5]

pattern Q :: [Char]
pattern Q = ['a']

{-# COMPLETE P, Q #-}
```


Accepted. The rtc of `P` and `Q` is `[]`. This is despite the fact that `P` and `Q` could never be used in the same pattern match. The check is there as a convenience to the user.

# Examples



The following examples should emit no warnings.


```
pattern P :: A
pattern P = A

{-# COMPLETE P #-}

foo :: A -> A
foo P = A
```

```

pattern N :: Maybe a
pattern N = Nothing

{-# COMPLETE N, Just #-}

qux :: Maybe a -> Bool
qux N = False
qux (Just x) = True

```

## Error Messages


When the pattern match checker requests a set of constructors for a type constructor `T`, we now return a list of sets which include the normal data constructor set and also any `COMPLETE` pragmas of type `T`.
We then try each of these sets, not warning if any of them are a perfect match. In the case the match isn't perfect, we select one of the branches of the search depending on how good the result is.


The results are prioritised in this order.

1. Fewest uncovered clauses
1. Fewest redundant clauses
1. Fewest inaccessible clauses
1. Whether the match comes from a `COMPLETE` pragma or the built-in set of data constructors for a type constructor.


It may be desirable to produce a summary of the results in a more intelligent way; #13363 showed that the current state of affairs is broken. Exploration of this is left open for further discussion.
