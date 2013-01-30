# Overloaded list notation


This wiki page documens the design and
implementation of the GHC extension for overloading Haskell's list notation.


A preliminary implementation of the extension can be pulled from the following
repositories:

```wiki
git://github.com/achimkrause/ghc.git
git://github.com/achimkrause/packages-base.git
git://github.com/achimkrause/packages-dph.git
git://github.com/achimkrause/testsuite.git
```


We would very much welcome contributions. If you would like to make a change
in the current design and implementation, please document it (e.g., on this
wiki page) and/or send us a GHC patch or a pull request.

## Current Implementation


Let us briefly recap the notation for constructing lists. In Haskell, the list
notation can be be used in the following seven ways:

```wiki
[]          -- Empty list
[x]         -- x : []
[x,y,z]     -- x : y : z : []
[x .. ]     -- enumFrom x
[x,y ..]    -- enumFromThen x y
[x .. y]    -- enumFromTo x y
[x,y .. z]  -- enumFromThenTo x y z
```


When the `OverloadedLists` extension is turned on, the aforementioned seven
notations are desugared as follows:

```wiki
[]          -- fromListN 0 []
[x]         -- fromListN 1 (x : [])
[x,y,z]     -- fromListN 3 (x : y : z : [])
[x .. ]     -- fromList (enumFrom x)
[x,y ..]    -- fromList (enumFromThen x y)
[x .. y]    -- fromList (enumFromTo x y)
[x,y .. z]  -- fromList (enumFromThenTo x y z)
```


This extension allows programmers to use the list notation for construction of
structures like: `Set`, `Map`, `IntMap`, `Vector`, `Text`
and `Array`. The following code listing gives a few examples:

```wiki
['0' .. '9']             :: Set Char
[1 .. 10]                :: Vector Int
[("default",0), (k1,v1)] :: Map String Int
['a' .. 'z']             :: Text
```


List patterns are also overloaded. When the `OverloadedLists` extension is turned on, the
definitions

```wiki
f [] = ...
g [x,y,z] = ...
```


will be treated as

```wiki
f (toList -> []) = ...
g (toList -> [x,y,z]) = ...
```


GHC, during the typechecking and desugaring phases, uses whatever is in scope
with the names of `fromList`, `toList` and `fromListN` (i.e., `fromList`, `toList` and
`fromListN` are rebindable).


That said, the `GHC.Exts` module exports the `IsList` class that can
be used to overload `fromListN` and `fromListN` for different
structures. The type class is defined as follows:

```wiki
class IsList l where
  type Item l
  fromList  :: [Item l] -> l
  toList    :: l -> [Item l]

  fromListN :: Int -> [Item l] -> l
  fromListN _ = fromList  
```


The `IsList` class and its methods are intended to be used in
conjunction with the `OverloadedLists` extension. The `Item` type
function returns the type of items of the structure `l`. The fromList
function constructs the structure `l` from the given list of `Item l`.
The `fromListN` function takes the input list's length as a hint. Its
behaviour should be equivalent to `fromList`. The hint can be used for
more efficient construction of the structure `l` compared to
`fromList`. If the given hint is not equal to the input list's length the
behaviour of `fromListN` is not specified.


The instances of the `IsList` class should satisfy the following
property:

```wiki
fromList . toList = id
```


In the following, we give several example instances of the `IsList` type
class:

```wiki
instance IsList [a] where
  type Item [a] = a
  fromList = id
  toList   = id

instance (Ord a) => IsList (Set a) where
  type Item (Set a) = a
  fromList = Set.fromList
  toList   = Set.toList

instance (Ord k) => IsList (Map k v) where
  type Item (Map k v) = (k,v)
  fromList = Map.fromList
  toList   = Map.toList

instance IsList (IntMap v) where
  type Item (IntMap v) = (Int,v)
  fromList = IntMap.fromList
  toList   = IntMap.toList

instance IsList Text where
  type Item Text = Char
  fromList = Text.pack
  toList   = Text.unpack

instance IsList (Vector a) where
  type Item (Vector a) = a
  fromList  = Vector.fromList
  toList    = Vector.toList
  fromListN = Vector.fromListN

```

## Further GHC improvements/extensions that may benefit `OverloadedLists`

## Defaulting


Currently, the `IsList` class is not accompanied with defaulting rules.
Although feasible, not much thought has gone into how to specify the meaning
of the default declarations like: `default ([a])`

## Literal Lists


The current implementation of the `OverloadedLists` extension can be
improved by handling the lists that are only populated with literals in a
special way. More specifically, the compiler could allocate such lists
statically using a compact representation and allow `IsList` instances
to take advantage of the compact representation. Equipped with this capability
the `OverloadedLists` extension will be in a good position to subsume the
`OverloadedStrings` extension (currently, as a special case, string
literals benefit from statically allocated compact representation).


Somewhat related discussions:

```wiki
http://hackage.haskell.org/trac/ghc/ticket/5218
http://www.serpentine.com/blog/2012/09/12/the-case-of-the-mysterious-explosion-in-space/
http://www.mail-archive.com/haskell-cafe@haskell.org/msg101412.html
```