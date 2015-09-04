# Unlifted data types


This page describes the unlifted data types, i.e. algebraic data types which live in kind Unlifted rather than kind \*.

## Motivation


Bob Harper [ has written](https://existentialtype.wordpress.com/2011/04/24/the-real-point-of-laziness/):

>
> Haskell suffers from a paucity of types.  It is not possible in Haskell to define the type of natural numbers, nor the type of lists of natural numbers (or lists of anything else), nor any other inductive type!


The reason, of course, is that whenever you write `data T = T`, you define a type with not one, but two inhabitants: `T` and bottom. In many settings, the laziness that may creep into a type is undesirable. We can see this in many domains in Haskell:

1. The `NFData` type class allows users to force data into normal form, where it has no more thunks inside. NFData is highly desirable for parallel Haskell: thunks can cause problem with parallel computation by shifting computation from one thread to another (for this reason, the `Par` Monad requires `NFData` on its API). And in general, users of Haskell often apply `NFData` in order to eliminate thunks which may be retaining data.

1. In the implementation of mutable variables, it is extremely useful to be able to assume that a pointer you have is to the honest closure that contains the mutable field, not an indirection to that closure. Without this guarantee, code which writes to this mutable variable has to first check if the thunk is fully evaluated before actually performing the memory write. For this reason, the `MutVar#` primitive (which is a GC'd object) lives in kind \#.

1. Haskell already has the ability to specify a field of a constructor as strict, forcing it to be evaluated when it is constructed. Strictness enables such optimizations as unpacking, and other benefits. Why not also make it possible to specify a type as strict as a whole?


Our proposal is to allow a new kind Unlifted, a minor variant of the existing kind \#, available for all data types (making them unlifted, but NOT unboxed).

## Syntax


We define a new kind `Unlifted` (subject to bikeshedding), which represents unlifted but boxed data types.


All data declarations `data Foo a b = ...` currently define a type constructor `Foo :: * -> * -> *`. When `UnliftedDataTypes` is enabled, we can unlift this constructor with the bang-prefix operator: `!Foo :: * -> * -> Unlifted`. 


Unlifted types are "evaluated strictly". For example, in the following code:

```wiki
foo :: !Int -> !Int
foo x = x + 1

main :: IO ()
main = let y = foo (error "foo")
       in error "bar"
```


the output is the error "foo", not "bar", as it would be if y was bound using a strict pattern match, or if foo's type had been `Int# -> Int#`. In practice, the evaluation rules will follow from how Core manages kind \# today.

## Implementation


Should be simple, except maybe for syntax.

## FAQ

**Is `!Maybe !Int` allowed?** No, because `!Int` has kind `Unlifted` but `!Maybe` has kind `* -> Unlifted`. A data type declaration must be explicitly written to accept an unlifted type (`data StrictMaybe (a :: Unlifted) = SMaybe a`), or simply be strict in its field (`data StrictMaybe2 a = SMaybe2 !a`).

**Can I instantiate a polymorphic function to an unlifted type?** Not for now, but with "levity polymorphism" we could make polymorphic types take any type of kind \* or Unlifted, as these representations are uniform.

**What's the difference between `!Int` and `Int#`?**`!Int` is an unlifted, boxed integer; `Int#` is an unlifted, unboxed integer.

**Why aren't strict patterns enough?** A user can forget to write a strict pattern at a use-site. Putting a type in kind unlifted forces all use-sites to act as if they had strict patterns.

**How do I unlift a symbolic type constructor?** Syntax is currently unspecified. Suggestions welcome.

**In what order do my binders get evaluated?** Backwards, but we should fix this. See [\#10824](https://gitlab.haskell.org//ghc/ghc/issues/10824)

**Why do we need a new kind `Unlifted`, does `#` work OK?** Actually, it would; but with a new kind, we can make a distinction between types with uniform representation (boxed), and types without it (unboxed).

**Can I unlift newtypes/type synonyms/...?** This is probably OK, in which case this proposal should be called `UnliftedTypes` instead?
