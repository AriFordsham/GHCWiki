
This is a write-down of a discussion between me (nomeata) and SPJ at TLCA 2013, but my memory might have tricked me and I might have filled in some bits myself, so beware. It should enable to solve (parts of) [\#2110](https://gitlab.haskell.org//ghc/ghc/issues/2110).

## Goal


The programmer expects zero-cost conversions between a newtypes *N* and the type *T* it is based on. We want to allow the programmer to have zero-cost conversions between *C N* and *C T*. Requirements:

- It should be sound, i.e. have an implementation in Core with existing coercions, without new coercions or `unsafeCoerce`.
- It should respect module boundaries, i.e. not allow the user to create a function which he could not write using plain Haskell (and non-zero cost).
- (desirable:) It should be exportable and composable, i.e. the module of *N* should be able to specify that a conversion between *N* and *T* is allowed (or not), and the module of *C* should be able to specify if a coercion can be lifted (or not), and if both allow it, the user should be able to combine that even if the modules of *N* and *T* are independent.
- (desirable:) It should be possible to use such a coercion in one module in internal code, but not export it.
- (desirable:) It should be possible to add REWRITE rules that turn, for example `map N` into the zero-cost conversion from *C T* to *C N*.

## Use cases


To clarify these requirements, here some benchmarks; feel free to expand if you have further needs.

- Allow the user to define a function of type `C N -> C T` with a zero-cost model.
- Allow the author of `Data.Set` prevent a coercion `Set N -> Set T` (as it would break internal invariants).
- Allow the author of a escaping-ensuring `newtype HTML = HTML String` to use the coercion `[String] -> [HTML]` internally, but prevent the user of the library from doing that.

## Suggested interface

### Abstract `NT` type


There is a module provided by GHC with these definitions

```wiki
data NT a b -- abstract
coerce :: NT a b -> a -> b
refl :: NT a a
sym :: NT a b -> NT b a
trans :: NT a b -> NT b c -> NT a c
```


and the intention that `NT a b` is a witness that `a` and `b` have the same representation and that `coerce n` has zero runtime cost.


Besides the trivial `refl` values, values of type `NT` cannot be created by the user, but only by a special `deriving` statement giving the type of a function that creates `NT` values, but without a definition, e.g.

```wiki
deriving nt :: NT N T
deriving list :: NT a b -> NT [a] [b]
```


The compiler either rejects the declaration or creates an implementation automatically. It should only create an implementation if a non-zero-cost-implementation could be written by the user instead. (I expect that this means that the constructors of the return type are in scope.)

### `NT` type class


An earlier variant of the idea introduced a type type class for coercions, e.g.

```wiki
class NT a b where
  coerce :: a -> b 
```


This would allow an interface more in line with existing features, i.e. one would simply list this class in the `derving` clause of `newtype` definitions, or use `deriving instance NT a b => NT [a] [b]`. See also [this comment by sweirich](https://gitlab.haskell.org//ghc/ghc/issues/2110). Unfortunately, type classes leak, so it would be possible to use the machinery in internal modules but not have it available to users of the library.


Of course with an `NT` data type, it is possible to define this type class, e.g.

```wiki
class NT' a b where
  theNT :: NT a b
coerce' :: NT' a b => a -> b
coerce' = coerce theNT
```


and use it where more convenient than the function-based variant.

## Implementation


Quite naively, I expect that the definition of `NT` and related function in terms of Core is straight-forward (`newtype NT a b = a ~_R b`).


The code that does the `deriving` is maybe non-trivial, as it has to build the term from available newtype axioms (where the constructor is in scope), coercions given as parameters and the application of type constructors to coercions (again, where the data constructors are in scope). 
