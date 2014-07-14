# Injective type families


This page summarizes the design behind injective type families ([\#6018](https://gitlab.haskell.org//ghc/ghc/issues/6018)). It is a work in progress. This page will evolve to reflect the progress made.


Person responsible for this page and the implementation: Jan Stolarek (just so you now who is meant by "I").

## Outline


The idea behind [\#6018](https://gitlab.haskell.org//ghc/ghc/issues/6018) is to allow users to declare that a type family is injective. The plan is to allow several kinds of injectivity:

1. Injectivity in all the arguments, where knowing the result (right-hand side) of a type family determines all the arguments on the left-hand side.

>
> Example 1:
>
> ```wiki
> type family F a b c
> type instance F Int  Char Bool = Bool
> type instance F Char Bool Int  = Int
> type instance F Bool Int  Char = Char
> ```

1. Injectivity in some of the arguments, where knowing the RHS of a type family determines only some of the arguments on the LHS.

>
> Example 2:
>
> ```wiki
> type family G a b c
> type instance G Int  Char Bool = Bool
> type instance G Int  Char Int  = Bool
> type instance G Bool Int  Int  = Int
> ```

>
> Here knowing the RHS allows us to determine first two arguments, but not the third one.

1. Injectivity in some of the arguments, where knowing the RHS of a type family and some of the LHS arguments determines other (possibly not all) LHS arguments.

>
> Example 3:
>
> ```wiki
> type family H a b c
> type instance H Int  Char   Double = Int
> type instance H Bool Double Char   = Int
> ```
>
>
> Here knowing the RHS and any single parameter uniquely determines the remaining two parameters.

>
> Example 4:
>
> ```wiki
> type family J a b c
> type instance J Int  Char   Double = Int
> type instance J Bool Double Double = Int
> ```
>
>
> Knowing the RHS and either `a` or `b` allows to uniquely determine the remaining two parameters, but knowing the RHS and `c` gives us no information about `a` or `b`.


Note that examples above are shown for open type families but the implementation will work for both open and closed type families.

## Proposed syntax


The proposed syntax for injectivity declaration is based on functional dependencies syntax. The injectivity declaration begins with `|` following type family declaration head. `|` is followed by a list of comma-separated injectivity conditions. Each injectivity condition has the form:

```wiki
result A -> B
```


where `A` is a possibly-empty list of type variables declared in type family head and `B` is non-empty list of said type variables. Things on the left and right of `->` are called LHS ans RHS of an injectivity condition, respectively. `result` becomes a restricted word that cannot be used as a type variables identifier in a type family declaration (neither in declaration head nor in the equations). This is identical to how the `role` word is treated.


The examples given in the "Outline" section could have the following injectivity conditions:

```wiki
type family F a b c | result -> a b c
type family G a b c | result -> a b
type family H a b c | result a -> b c, result b -> a c, result c -> a b
type family J a b c | result a -> b c, result b -> a c
```


For closed type families each of the above lines would be appended with the `where` keyword.

## Plan of attack


My plan is to divide implementation of this feature into smaller steps, each of which will provide a working set of features usable to the end users of GHC. These are the steps:

1. Implement injective type families that are:

>
> a) injective in all the arguments (Example 1 above)

>
> b) only admit RHS that is a concrete type or a call to a type constructor or a type variable introduced by the LHS or a recursive call to self. This means that calls to another type family will result in compilation error for type families declared as injective.

1. Lift restriction a) ie. allow type families injective only in some arguments (Examples 2-4 above)
1. Lift restriction b) ie. allow injective type families to call other type families?


Step 3 of the above outline is in fact more in the lines of [\#4259](https://gitlab.haskell.org//ghc/ghc/issues/4259) so it will most likely not be implemented as part of [\#6018](https://gitlab.haskell.org//ghc/ghc/issues/6018).

## Examples?

*More examples will be added from comments in [\#6018](https://gitlab.haskell.org//ghc/ghc/issues/6018). If anyone can supply even more examples (type family declarations + their usage that currently doesn't work but should work with injectivity) please add them here.*


Example A:

```wiki
type family F a | result -> a where
    F Char = Bool
    F Bool = Int
    F Int  = Char

idChar :: (F a ~ Bool) => a -> Char
idChar a = a
```


GHC should infer that `a` is in fact `Char`. Right now this program is rejected.

## Implementation outline

*This section outlines **what** will be done in the implementation, without giving specific details **how** it will be done. Details on the implementation within GHC will be added as work progresses.*


The implementation needs to check the correctness of injectivity conditions declarations. This includes checking that:

- only in-scope type variables are used. For example `type family F a | result -> b` should result with "not in scope: b" error.
- there are no identical conditions (this wouldn't hurt, but the user deserves a warning about this)
- type variables are not repeated on either LHS or RHS of the injectivity condition. For example `result a a -> ...` or `... -> a b a` should generate a warning. Note that it probably is OK to have the same variable both on the LHS and RHS of an injectivity condition: in the above examples it is true that `type family G a b c | result c -> a b c`. The question is whether this has any practical relevance.
- injectivity conditions don't overlap (eg. `result -> a b` overlaps `result -> a`). This probably deserves a warning.


I am not certain at the moment how to treat these injectivity conditions declarations:

- `result -> a, result -> b` is technically correct but we could just say `result -> a b`. Do the two separate declarations have the same power as the combined one?


Of course the implementation needs to verify that injectivity conditions specified for a type family really hold.

## Questions without an answer (yet)

1. Is there a point in allowing injectivity declarations for associated types?
