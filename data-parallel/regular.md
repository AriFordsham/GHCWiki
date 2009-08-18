# Shape Polymorphic Arrays and Delayed Arrays


The library provides a layer on top of DPH unlifted arrays to support multi-dimensional arrays, and operations 
like maps, folds, permutations, shifts and so on. The interface for delayed arrays is similar, but in contrast 
to operations on the former, any operation on a delayed array is not evaluated. To force evaluation, the programmer
has to explicitely convert a delayed array to a strict array. 


The current implementation of the library exposes some implementation details the user of the library shouldn't 
have to worry about. Once the design of the library is finalised, most of these will be hidden by distinguishing 
between internal types and representation types

## Strict Arrays, Delayed Array and Shape Data Type


Both strict and delayed arrays are parametrised with their shape - that is, their dimensionality and size 
of each dimension. The shape type class has the following interface:

```wiki
class (Show sh, U.Elt sh) => Shape sh where
  dim   :: sh -> Int           -- ^number of dimensions (>= 0)
  size  :: sh -> Int           -- ^for a *shape* yield the total number of 
                               -- elements in that array
  index :: sh -> sh -> Int     -- ^corresponding index into a linear, row-major 
                               -- representation of the array (first argument
                               -- is the shape)
  indexInv:: sh -> Int -> sh   -- ^given index into linear row major representation,
                               -- calculates index into array
  range      :: sh -> U.Array sh  -- ^all the valid indices in a shape. The following
                                  -- equality should hold: 
                                  -- map (index sh) (range sh) = [:0..(size sh)-1:]
  inRange    :: sh -> sh -> Bool  -- ^determines if a given index is in range
  zeroDim    :: sh                
  addDim     :: sh -> sh -> sh    -- ^adds two shapes of the same dimensionality
  modDim     :: sh -> sh -> sh    -- ^modulo operation lifted on shapes
  addModDim  :: sh -> sh -> sh

  last    :: (sh :*: Int) -> Int  -- ^yields the innermost dimension of a shape
  inits   :: (sh :*: Int) -> sh   -- ^removes the innermost dimension from a shape
```


Note that a `Shape` has to be in the type class `Elt` imported from `Data.Parallel.Array.Unboxed` so 
that it can be an element of  `Data.Parallel.Array.Unboxed.Array`.


The following instances are defined

```wiki
instance Shape () 
instance Shape sh => Shape (sh :*: Int) 
```


so we have inductively defined n-tuples of `Int` values to represent shapes. This somewhat unusual representation
is necessary to be able to inductively define operations on `Shape`. It should, however, be hidden from the library
user in favour of the common tuple representation.


For convenience, we provide type synonyms for dimensionality up to five:

```wiki
type DIM0 = ()
type DIM1 = (DIM0 :*: Int)
....
```

## Operations on Arrays and Delayed Arrays

### Array Creation and Conversion


Strict arrays are simply defined as record containing a flat data array and shape information:

```wiki
data Array dim e where
  Array { arrayShape    :: dim                -- ^extend of dimensions
        , arrayData     :: U.Array e          -- flat parallel array
        }               :: Array dim e
  deriving Show

toArray  :: (U.Elt e, Shape dim) => dim -> U.Array e -> Array dim e
fromArray:: (U.Elt e, Shape dim) => Array dim e -> U.Array e 
```


Delayed arrays, in contrast, in addition to the shape, only contain a function which, given an index,
yields the corresponding element.

```wiki
data DArray dim e where 
  DArray :: {dArrayShape::dim -> dArrayFn:: (dim -> e)} -> DArray dim e
```


Delayed arrays can be converted to and from strict arrays:
(TODO there needs to be an darray constructor function accepting the shape and the function as arguments)

```wiki
toDArray:: (U.Elt e, Array.Shape dim)   => Array.Array dim e -> DArray dim e
fromDArray:: (U.Elt e, Array.Shape dim) => DArray dim e      -> Array dim e
```

### Shape Invariant Computations on Arrays


The array operations described in this and the following subsection
are available on both strict and delayed arrays, and yield the same
result, with the exception that in case of delayed arrays, the result
is only calculated once its forced by calling `fromDArray`. No
intermediate array structures are ever created.


The library provides a range of operation where the dimensionality of
the result depends on the dimensionality of the argument in a
non-trivial matter, which we want to be reflected in the type system. 
Examples of such functions are generalised selection, which allows for 
extraction of subarrays of arbitrary dimension, and generalised replicate,
which allows replication of an array in any dimension (or dimensions). 


For selection, we can informally state the relationship between dimensionality of
the argument, the selector, and the result as follows:

```wiki
select:: Array dim e -> <select dim' of dim array> -> Array dim' e
```


To express this relationship, the library provides the index GADT,
which expresses a relationship between the inital and the projected
dimensionality. It is defined as follows:

```wiki
data Index a initialDim projectedDim where
  IndexNil   :: Index a () ()
  IndexAll   :: (Shape init, Shape proj) =>      
                   Index a init proj -> Index a (init :*: Int) (proj :*: Int)
  IndexFixed :: (Shape init, Shape proj) => a -> 
                   Index a init proj -> Index a (init :*: Int)  proj

type SelectIndex = Index Int
type MapIndex    = Index ()
```


Given this definition, the type of `select` now becomes

```wiki
select:: (U.Elt e, Shape dim, Shape dim') => Array dim e -> SelectIndex dim dim'  -> Array dim' e
```


Example:

```wiki
arr:: Array DIM3 Double
select arr (IndexFixed 3 (IndexAll (IndexAll IndexNil)))
```


The index type is also used to express the type of generalised replicate:

```wiki
replicate:: Array dim' e -> SelectIndex dim dim'  -> Array dim e
```


Even though the index type serves well to express the relationship
between the selector/multiplicator and the dimensionality of the
argument and the result array, it is somehow inconvenient to use, as
the examples demonstrate. This is therefore another example where we
need to add another layer to improve the usability of the library. 


Note that the library provides no way to statically check the pre- and
postconditions on the actual size of arguments and results. This has
to be done at run time using assertions.

## Array Operations


   
Backpermute and default backpermute are two general operations which allow 
the programmer to express all structural operations which reorder or extract
elements based on their position in the argument array:

```wiki
backpermute:: (U.Elt e, Shape dim, Shape dim') => 
  Array dim e -> dim' -> (dim' -> dim) -> Array dim' e

backpermuteDft::(U.Elt e, Shape dim, Shape dim') => 
  Array dim e -> e -> dim' -> (dim' -> Maybe dim) -> Array dim' e
```

```wiki
map:: (U.Elt a, U.Elt b, Shape dim) => (a -> b) -> Array dim a -> Array dim b

zip:: (U.Elt a, U.Elt b, Shape dim) => Array dim a -> Array dim b-> Array dim (a :*: b)

zipWith:: (U.Elt a, U.Elt b, U.Elt c, Shape dim) => 
          (a -> b -> c) -> Array dim a -> Array dim b-> Array dim c

mapFold:: (U.Elt e, Shape dim) => (e -> e-> e) -> e -> Array (dim :*: Int) e  -> Array dim  e

reshape:: (Shape dim', Shape dim, U.Elt e) => Array dim e -> dim' -> Array dim' e
```


The following operations could be (and in the sequential implementation indeed are)  expressed
in terms of backpermute and default backpermute. However, a programmer should aim at using more
specialised functions when provided, as they carry more information about the pattern of reordering. 
In particular in the parallel case, this could be used to provide significantly more efficient 
implementation which make use of locality and communication patterns.

```wiki
shift:: (Shape dim, U.Elt e) => Array dim e -> e -> dim -> Array dim e

rotate:: (Shape dim, U.Elt e) => Array dim e -> e -> dim -> Array dim e

tile::  (Shape dim, U.Elt e) => Array dim e -> dim -> dim -> Array dim e
```