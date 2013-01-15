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

- For `x3`, we'd have to map over `T`, thus `mapT MkAge x3`.  But what if `mapT` didn't exist?  We'd have to make it. And not all data types have maps. `S` is a harder one: you could only map over S-values if `m` was a functor.  There's a lot of discussion abou this on [\#2110](https://gitlab.haskell.org//ghc/ghc/issues/2110).

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


So to solve a `NTC` constraint you unwwap all those newtypes (being careful about abstraction; see next section).  


This plan requires a bit more paddling under the water on GHC's part, especially during type inference, but it looks a lot more straightforward than I first thought.  Thanks to Roman Cheplyka for advocating this solution.

## Abstraction


Suppose we have

```wiki
	module Map( ... ) where
	  data Map a b = ...blah blah...

	module Age( ... ) where
	  newtype Age = MkAge Int
```


Now suppose we want a newtype wrapper like this

```wiki
	import Map
	import Age

	newtype wrap foo :: Map Int Bool -> Map Age Bool
```


Could we write `foo` by hand? (This is a good criterion, I think.) Only if we could see the data constructors of *both*`Map`*and*`Age`. 

- If we can't see the data constructor of `Age` we might miss an invariant that Ages are supposed to have.   For example, they might be guaranteed positive.

- If we can't see the data constructors of `Map`, we might miss an invariant of Maps. For example, maybe `Map` is represented as a list of pairs, ordered by the keys.  Then, if `Age` orders in the reverse way to `Int`, it would obviously be bad to substitute.


Invariants like these are difficult to encode in the type system, so we use "exporting the constructors" as a proxy for "I trust the importer to maintain invariants".  The "Internals" module name convention is a signal that you must be particularly careful when importing this module; runtime errors may result if you screw up.


One possible conclusion: if we have them at all, newtype wrappers should only work if you can see the constructors of *both* the newtype, *and* the type you are lifting over.  


But that's not very satisfactory either.  

- There are some times (like `IO`) where it \*does\* make perfect sense
  to lift newtypes, but where we really don't want to expose
  the representation. 

- Actually `Map` is also a good example: while `Map Age Bool` should
  not be converted to `Map Int Bool`, it'd be perfectly fine to convert
  `Map Int Age` to `Map Int Int`.

- The criterion must be recursive.  For example if we had

  ```wiki
  	 data Map a b = MkMap (InternalMap a b)
  ```

  It's no good just being able to see the data constructor `MkMap`; you need to
  see the constructors of `InternalMap` too.


The right thing is probably to use kinds, and all this is tantalisingly close to the system suggested in [ Generative type abstraction and type-level computation](http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/).  Maybe we should be able to *declare* Map to be indexed (rather than parametric) in its first parameter.


More thought required.

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
The solution is described in [ Generative type abstraction and type-level computation](http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/).  It is still not implemented, alas,
but adding the newtype wrappers introduces no problems that we do not already have.
