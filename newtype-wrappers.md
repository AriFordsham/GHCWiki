Page Contents[Newtype wrappers](#Newtypewrappers)[The problem](#Theproblem)[Goal](#Goal)[Use cases](#Usecases)[The opportunity](#Theopportunity)[Proposal 1](#Proposal1)[Proposal 2](#Proposal2)[Status](#Status)[Proposal 3](#Proposal3)[Type soundness](#Typesoundness)

# Newtype wrappers


This page proposes **newtype wrappers**, a new feature for Haskell
indended to make newtypes more flexible and useful.  It tackles head-on 
the problem underlying [\#7542](https://gitlab.haskell.org//ghc/ghc/issues/7542) and [\#2110](https://gitlab.haskell.org//ghc/ghc/issues/2110).


Email thread [ here](http://www.haskell.org/pipermail/glasgow-haskell-users/2013-January/023455.html).

## The problem


Suppose we have

```wiki
newtype Age = MkAge Int
```


Then if `n :: Int`, we can convert `n` to an `Age` thus: `MkAge n :: Age`.
Moreover, this conversion is a type conversion only, and involves no runtime
instructions whatsoever.  This cost model -- that newtypes are free -- is important
to Haskell programmers, and encourages them to use newtypes freely to express
type distinctions without introducing runtime overhead.


Alas, the newtype cost model breaks down when we involve other data structures.
Suppose we have these declarations

```wiki
data T a   = TLeaf a     | TNode (Tree a) (Tree a)
data S m a = SLeaf (m a) | SNode (S m a) (S m a)
```


and we have these variables in scope

```wiki
x1 :: [Int]
x2 :: Char -> Int
x3 :: T Int
x4 :: S IO Int
```


Can we convert these into the corresponding forms where the `Int` is replaced by `Age`?
Alas, not easily, and certainly not without overhead.  

- For `x1` we can write `map MkAge x1 :: [Age]`. But this does not follow the newtype cost model: there will be runtime overhead from executing the `map` at runtime, and sharing will be lost too.  Could GHC optimise the `map` somehow?  This is hard; apart from anything else, how would GHC know that `map` was special?  And it it gets worse.

- For `x2` we'd have to eta-expand: `(\y -> MkAge (x2 y)) :: Char -> Age`.  But this isn't good either, because eta exapansion isn't semantically valid (if `x2` was bottom, `seq` could distinguish the two).  See [\#7542](https://gitlab.haskell.org//ghc/ghc/issues/7542) for a real life example.

- For `x3`, we'd have to map over `T`, thus `mapT MkAge x3`.  But what if `mapT` didn't exist?  We'd have to make it. And not all data types have maps. `S` is a harder one: you could only map over S-values if `m` was a functor.  There's a lot of discussion about this on [\#2110](https://gitlab.haskell.org//ghc/ghc/issues/2110).

## Goal


In summary: The programmer expects zero-cost conversions between a newtypes *N* and the type *T* it is based on. We want to allow the programmer to have zero-cost conversions between *C N* and *C T*. Requirements:

- It should be sound, i.e. have an implementation in Core with existing coercions, without new coercions or `unsafeCoerce`.
- It should respect module boundaries, i.e. not allow the user to create a function which he could not write using plain Haskell (and non-zero cost).
- (desirable:) It should be exportable and composable, i.e. the module of *N* should be able to specify that a conversion between *N* and *T* is allowed (or not), and the module of *C* should be able to specify if a coercion can be lifted (or not), and if both allow it, the user should be able to combine that even if the modules of *N* and *T* are independent.
- (desirable:) It should be possible to use such a coercion in one module in internal code, but not export it.
- (desirable:) It should be possible to add REWRITE rules that turn, for example, `map N` into the zero-cost conversion from *C T* to *C N*.

## Use cases


To clarify these requirements, here some benchmarks; feel free to expand if you have further needs.

- Allow the user to define a function of type `C N -> C T` with a zero-cost model.
- Allow the author of `Data.Set` prevent a coercion `Set N -> Set T` (as it would break internal invariants).
- Allow the author of a escaping-ensuring `newtype HTML = HTML String` to use the coercion `[String] -> [HTML]` internally, but prevent the user of the library from doing that.

## The opportunity


Clearly what we want is a way to "lift" newtype constructors (and dually deconstructors)
over arbitrary types, so that whenever we have some type `blah Int blah` we can convert it 
to the type `blah Age blah`, and vice versa.


Tantalisingly, [ System FC](http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/), GHC's internal Core language, already has exactly this!

- A newtype constructor turns into an FC cast:

  ```wiki
    MkAge x    turns into       x |> AgeNTCo
  where
    AgeNTCo :: Int ~ Age
  ```


The `|>` is a cast, and the `AgeNTCo` is a coercion axiom witnessng the equality of `Int` and `Age`. 

- Coercions can be lifted, so that

  ```wiki
    [AgeNTCo]       :: [Int] -> [Age]
    Char -> AgeNTCo :: (Char -> Int) ~ (Char -> Age)
    T AgeNTCo       :: T Int ~ T Age
    S IO AgeNTCo    :: S IO Int ~ S IO Age
  ```


So all we need is concrete syntax to allow you to ask for these lifed coercions in Haskell.  

## Proposal 1


The first possiblity involves a new top-level declaration:

```wiki
  newtype wrap w1 :: [Int] -> [Age])
  newtype wrap w2 :: (Char -> Int) -> (Char -> Age)
  newtype wrap w3 :: T Int -> T Age
  ..etc..
```


and dually

```wiki
  newtype unwrap u1 :: [Age] -> [Int])
  newtype unwrap u2 :: (Char -> Age) -> (Char -> Int)
  ..etc...
```


This brings into scope the variables `w1`, `w2`, etc, with the declared types. Applications of these wrappers
and unwrappers have the same cost model as newtype constructors themselves: they are free.


More precisely

- The type specified in a newtype wrap/unwrap declaration must be of the form `type1 -> type2`.

- `wrap` and `unwrap` are keywords, but only when they occur right after the keyword `newtype`.

- Wrappers/unwrappers can be polymorphic

  ```wiki
    newtype wrap foo :: [(Int,b)] -> [(Age,b)]
  ```

- Multiple "layers" of newtype can be wrapped at once (just as in `foreign` declarations). For example:

  ```wiki
    newtype Fun a = MkFun (a -> a)
    newtype Age = MkAge Int
    newtype wrap foo :: (Int -> Int) -> Fun Age
  ```

## Proposal 2


The second possibility is superficially simpler: just provided a new built-in constant with type

```wiki
  newtypeCast :: NTC a b => a -> b
```


Here `NTC` is a built-in type class that witnesses the (free) conversion between `a` and `b`.  Although it would not quite be implemented like this, we would have a built-in instance for each data type (but see Type Soundness below):

```wiki
  instance NTC a b => NTC [a] [b]
```


and two built-in instances for each newtype:

```wiki
  instance NTC Int b => NTC Age b
  instance NTC a Int => NTC a Age
```


So to solve a `NTC` constraint you unwrap all those newtypes (being careful about abstraction; see next section).  


This plan requires a bit more paddling under the water on GHC's part, especially during type inference, but it looks a lot more straightforward than I first thought.  Thanks to Roman Cheplyka for advocating this solution.


One issue with type classes is that they cannot be used only internally, so the third use case above would not be possible.

### Status


Joachim started to implement this approach. TODO

- Class `IsNT concr abstr` defined in ghc-prim.GHC.NT ✓
- Deriving code added. ✓
- Check for constructor visibility. ✓
- Check for data constructor argument convertibility. ✓ (but not very elegant yet)
- Make `IncoherentInstances`[ even more incoherent](http://www.haskell.org/pipermail/ghc-devs/2013-July/001771.html).
- Think about higher-order type arguments.
- Polish error messages.
- Port to the new role-based coercion.
- Add instances to base etc.


Warts and issues:

- The deriving code works in the typechecker and has to generate `HsExpr`, but the implementation is only possible in Core. Currently, a dummy value is inserted by the deriving code and later implemented by a core-to-core pass. Better solution: Add a `HsCore` data constructor to `HsExpr`, similar to `HsCoreTy` in `HsType`?


Code not yet pushed anywhere.

## Proposal 3


This is a write-down of a discussion between me (nomeata) and SPJ at TLCA 2013, but my memory might have tricked me and I might have filled in some bits myself, so beware.


Assume there is a module provided by GHC with these definitions

```wiki
data NT a b -- abstract
coerce :: NT a b -> a -> b
refl   :: NT a a
sym    :: NT a b -> NT b a
trans  :: NT a b -> NT b c -> NT a c
```


and the intention that `NT a b` is a witness that `a` and `b` have the same representation and that `coerce n` has zero runtime cost.


Besides the trivial `refl` values, values of type `NT` cannot be created by the user, but only by a special `deriving` statement giving the type of a function that creates `NT` values, but without a definition, e.g.

```wiki
deriving nt :: NT N T
deriving listNT :: NT a b -> NT [a] [b]
```


The compiler either rejects the declaration or creates an implementation automatically. It should only create an implementation if a non-zero-cost-implementation could be written by the user instead. (I expect that this means that the constructors of the return type are in scope.)


The “could you write it by hand” criteria implies that the expected arguments for the nt value depends on how they type parameters are used in the data constructors, e.g:

```wiki
data Foo a = Foo (Bar a)
deriving fooNT' :: NT a b -> NT (Foo a) (Foo b) -- not ok, especially if `Bar` is abstract
deriving fooNT :: NT (Bar a) (Bar b) -> NT (Foo a) (Foo b) -- ok
```


Question: `fooNT'` can be constructed from `fooNT` and a possible `barNT :: NT a b -> NT (Bar a) (Bar b)`. Should the compiler do this automatically, if `barNT` is in scope, or is that too much magic?


This solves the abstraction problem for `Data.Map`: The library author only exports `NT a b -> NT (Map k a) (Map k b)`, but not NT a b -\> NT (Map a v) (Map b v)\`.


All this amounts to exposing explicit System-FC coercions to the programmer; and that is precisely what we want to do.

- Explicit, because `Age` and `Int` really are different types, and so we can't silently convert between them.
- Explicit, so that we can control precisely which coercions are available (via export lists), and thus control abstraction.


Of course with an `NT` data type, it is possible to define this type class, e.g.

```wiki
class NT' a b where
  theNT :: NT a b
coerce' :: NT' a b => a -> b
coerce' = coerce theNT
```


and use it where more convenient than the function-based variant.


As to the implementation, I quite naively expect that the definition of `NT` and related function in terms of Core to be straight-forward (`newtype NT a b = a ~_R b`). The code that does the `deriving` is maybe non-trivial, as it has to build the term from available newtype axioms (where the constructor is in scope), coercions given as parameters and the application of type constructors to coercions (again, where the data constructors are in scope). 


Implementation started at [ https://github.com/nomeata/nt-coerce/](https://github.com/nomeata/nt-coerce/).


It was [ later argued](http://www.haskell.org/pipermail/ghc-devs/2013-July/001667.html) that the benefits over the type-class apporoach (Approach 2) do not warrant the extra syntactical complexity.

## Type soundness


This proposal rests on very similar foundations to the "newtype deriving" feature,
and suffers from *precisely* the same issue with type soundness; see [\#1496](https://gitlab.haskell.org//ghc/ghc/issues/1496), [\#5498](https://gitlab.haskell.org//ghc/ghc/issues/5498), [\#4846](https://gitlab.haskell.org//ghc/ghc/issues/4846), [\#7148](https://gitlab.haskell.org//ghc/ghc/issues/7148).  For example,
consider

```wiki
  type family F a
  type instance F Int = Int
  type instance F Age = Char

  data T a = MkT (F a)

  newtype wrap bad :: T Int -> T Age

  bogus :: Int -> Char
 bogus n = case (bad (MkT n)) of
              MkT c -> c
```


The problem is, as usual, the type function hiding inside T's definition.
The solution is described in [ Generative type abstraction and type-level computation](http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/).  It is still being implemented, alas,
but adding the newtype wrappers introduces no problems that we do not already have. See [Roles](roles) for more info.
