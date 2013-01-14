# Newtype wrappers


This page proposes **newtype wrappers**, a new feature for Haskell
indended to make newtypes more flexible and useful.  It tackles head-on 
the problem underlying [\#7542](https://gitlab.haskell.org//ghc/ghc/issues/7542) and [\#2110](https://gitlab.haskell.org//ghc/ghc/issues/2110).

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

## The proposal


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


So all we need is concrete syntax to allow you to ask for these lifed coercions in Haskell.  I couldn't think of a good way to do this, but now I have a proposal:

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
The solution is described in \[[ http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/](http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/) 
Generative type abstraction and type-level computation\].  It is still not implemented, alas,
but adding the newtype wrappers introduces no problems that we do not already have.
