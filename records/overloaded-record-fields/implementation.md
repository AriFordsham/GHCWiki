# [OverloadedRecordFields](records/overloaded-record-fields) implementation notes

**This page gives implementation details of the [OverloadedRecordFields](records/overloaded-record-fields) extension.  It is targeted at GHC hackers.**

- See the [redesign page](records/overloaded-record-fields/redesign) for a more gentle introduction.
- See [ Adam's post on the Well-Typed blog](http://www.well-typed.com/blog/2015/03/overloadedrecordfields-revived/)


Following the 2015 redesign, we have three separate components to implement, which will be described (and implemented) in order:

- the `DuplicateRecordFields` extension, which permits the same field name to be used multiple times in the same module, but only where unambiguous;
- the `OverloadedLabels` extension, to enable the `#x` syntax;
- the `HasField` and `FieldUpdate` typeclasses, with special-purpose constraint solving behaviour, which do not require a language extension.


As of March 2015, work is progressing on the implementation. Part 1 (the `DuplicateRecordFields` extension) is available at [ Phab:D761](https://phabricator.haskell.org/D761) and being reviewed. Part 2 (the `OverloadedLabels` extension) is nearly ready, and part 3 will be implemented after the first two parts have been completed. Note that all the parts are useful in isolation.

## 1. The `DuplicateRecordFields` extension


See [separate notes of the implementation of DuplicateRecordFields](records/overloaded-record-fields/duplicate-record-fields#implementation).

## 2. The `OverloadedLabels` extension


This part is new in the 2015 redesign  The implementation is fairly straightforward and close to (but simpler than) the existing `ImplicitParameters` extension. The key points:

- We extend the lexer to treat `#x` as a single lexeme (only when `OverloadedLabels` is enabled) and parse it into a new constructor `HsOverLabel "x"` of `HsSyn`.

- A new module `GHC.OverloadedLabels` defines the (renamed) `IV` class

  ```wiki
  class OverloadedLabel (x :: Symbol) a where
    overloadedLabel :: a
  ```

- When the typechecker sees `HsOverLabel "x"`, it emits a new wanted constraint `OverloadedLabel "x" alpha`, just like `HsIPVar`.

## 3. The magic type classes


The `HasField` and `FieldUpdate` classes, and `FieldType` and `UpdatedRecordType` type families, will be defined in the module `GHC.Records` in the `base` package.  Contrary to the previous design, we will not generate any dfuns/axioms for these classes \*at all\*.  Instead, the typechecker will implicitly create evidence as required.  This gets rid of a whole lot of complexity.


The only additional things that need to be generated at datatype declarations are updater functions (one per field), which correspond to the selector functions that are already generated.  So for example

```wiki
data T = MkT { x, y :: Int }
```


will generate

```wiki
$sel:x:T :: T -> Int
$sel:x:T (MkT x _) = x

$upd:x:T :: T -> Int -> T
$upd:x:T (MkT _ y) x = MkT x y
```


The updater function will always have a name prefixed with `$upd:`, regardless of whether `OverloadedRecordFields` is enabled.

### GADT record updates


Consider the example

```wiki
data W a where
    MkW :: a ~ b => { x :: a, y :: b } -> W (a, b)
```


It would be nice to generate

```wiki
-- $upd:x:W :: W (a, b) -> a -> W (a, b)
$upd:x:W s e = s { x = e }
```


but this record update is rejected by the typechecker, even though it is perfectly sensible, because of [\#2595](https://gitlab.haskell.org//ghc/ghc/issues/2595). The currently implemented workaround is instead to generate the explicit update

```wiki
$upd:x:W (MkW _ y) x = MkW x y
```


which is fine, but rather long-winded if there are many constructors or fields. Essentially this is doing the job of the desugarer for record updates.


Note that `W` does not admit type-changing single update for either field, because of the `a ~ b` constraint. Without it, though, type-changing update should be allowed.

### Unused bindings


Unused local bindings are tricky in the presence of the magic type classes, as the following example illustrates:

```wiki
module M (f)
  data S = MkS { foo :: Int }
  data T = MkT { foo :: Int }

  f = #foo (MkS 3)
  g x = #foo x
```


The renamer calculates the free variables of each definition, to produce a list of `DefUses`. The typechecker will discover that `f` uses only `S(foo)` while `g` uses neither. The simplest thing might be to make an occurrence of an overloaded field in an expression return as free variables all the selectors it might refer to. This will sometimes fail to report unused local bindings: in the example, it will not spot that `T(foo)` is unused.

TODO this needs some thought in the new story. An overloaded label might not have anything to do with fields. Moreover, what if the typechecker solves a `HasField` constraint that was introduced without using the overloaded label syntax? Can we defer unused local binding reporting somehow?
