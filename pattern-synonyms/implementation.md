## Implementation notes for pattern synonyms

## Frontend representation


After parsing, and during renaming, pattern synonyms are stored as
`HsBind`s.

## Renaming


During renaming, pattern synonyms are put in recursive binding groups
together with function bindings. Other pattern synonyms and
[ViewPatterns](view-patterns) can cause recursive pattern synonym declarations, which
are rejected.

## Typechecking


The typechecking pass turns `PatSynBind`s into a `PatSyn` and several
`HsBind`s. To fill in the `PatSyn`, we typecheck the right-hand side
of the pattern synonym declaration, then do some extra processing on
it to reject as-patterns and optionally compute the reverse of the
pattern synonym (for implicitly bidirectional ones). Afterwards, we
collect universal & existential type variables and typeclass dictionary
variables to be used when creating `ConPatOut` patterns from pattern
synonym occurrences, and generate some `HsBind`s:

- The `PatSyn` stores typing information for the pattern synonym, to
  be consulted when typechecking pattern synonym usage sites.

- The first `HsBind` is the binder for the *matcher* function
  generated from the pattern synonym. The matcher is used when
  desugaring pattern synonym usage sites (see below).

- For bidirectional pattern synonyms, another `HsBind` called a
  *wrapper* is created to be used for pattern synonym usages in
  expression contexts. It is a wrapper in the same sense as a
  constructor wrapper.


Pattern synonym occurrences in patterns are turned into `ConPatOut`s
just like regular constructor matches. `ConPatOut` has been changed to
store a `ConLike` instead of a `DataCon`; the `ConLike` type is simply
the sum of `DataCon` and `PatSyn`.

## Desugaring

### Grouping


During match grouping, subsequent `PatSyn` patterns are combined into
one group per pattern synonym. For example, given the following code:

```wiki
pattern Single x <- [x]
pattern Pair x y <- [x, y]

f []             = 0
f (Single True)  = 1
f (Single False) = 2
f (Pair _ _)     = 3
f (_:_:_)        = 4
```


the two `Single` patterns are put in one `PgSyn` `PatGroup`.


### Matching


For each pattern synonym, a matcher function is generated which gets a
scrutinee and a success and a failure continuations. Given a type

```wiki
data T a where
     MkT :: (Cls b) => b -> a -> T a
```


and a pattern synonym

```wiki
pattern Pat x y = MkT x y
```


we generate the matcher function

```wiki
$mPat :: forall r a. T a -> (forall b. Cls b => b -> a -> r) -> r -> r
$mPat scrutinee pass fail = case scrutinee of
  MkT x y -> pass x y
  _ -> fail
```


Occurrences of pattern synonyms are then desugared into calls to this
matcher function. This allows pattern synonym definitions to be just
as opaque as function definitions: their type defines their interface
completely. This gives us a story for exporting pattern synonym
definitions that is entirely consistent with existing function
definition exports.
