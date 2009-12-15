
The library provides a layer on top of DPH unlifted arrays to support multi-dimensional arrays, and shape polymorphic 
operations for fast sequential and parallel execution. The interface for delayed arrays is similar, but in contrast 
to operations on the former, any operation on a delayed array is not evaluated. To force evaluation, the programmer
has to explicitly convert them to a strict array. 


The current implementation of the library exposes some implementation details the user of the library shouldn't 
have to worry about. Once the design of the library is finalised, most of these will be hidden by distinguishing 
between internal types and representation types.

## Strict Arrays, Delayed Array and Shape Data Type


Both strict and delayed arrays are parametrised with their shape - that is, their dimensionality and size 
of each dimension. The shape type class has the following interface:

### DArrays


DArrays are collections of values of \`primitive' type, which are
member of the class Data.Parallel.Array.Unlifted.Elt, which includes
all basic types like Float, Double, Bool, Char, Integer, and pairs
constructed with Data.Parallel.Array.Unlifted.(:\*:), including ().

### The Shape class


Values of class Shape serve two purposes: they can describe the dimensionality and 
size of an array (in which case we refer to them as 'shape'), and they can also refer 
to the position of a particular element in the array (in which case we refer to them
as an 'index'). It provides the following methods:

```wiki
class (Show sh, U.Elt sh, Eq sh, Rebox sh) => Shape sh where
  dim   :: sh -> Int           
  -- ^number of dimensions (>= 0)
  size  :: sh -> Int           
  -- ^for a *shape* yield the total number of  elements in that array
  toIndex :: sh -> sh -> Int     
  -- ^corresponding index into a linear representation of 
  -- the array (first argument is the shape)

  fromIndex:: sh -> Int -> sh   
  -- ^given index into linear row major representation, calculates 
  -- index into array                               

  range      :: sh -> U.Array sh
  -- ^all the valid indices in a shape. The following equality should hold: 
  -- map (toIndex sh) (range sh) = [:0..(size sh)-1:]

  
  inRange      :: sh -> sh -> Bool
  -- ^Checks if a given index is in the range of an array shape. I.e.,
  -- inRange sh ind == elem ind (range sh)

  zeroDim      :: sh
  -- ^shape of an array of size zero of the particular dimensionality

  intersectDim :: sh -> sh -> sh
  -- ^shape of an array of size zero of the particular dimensionality  

  next:: sh -> sh -> Maybe sh
  -- ^shape of an array of size zero of the particular dimensionality    
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


The multiparameter type class `Subshape sh sh'` contains all pairs of shapes `sh` and `sh'`, for which the dimensionality of `sh'` is
less than that of `sh`.  

```wiki
class (Shape sh, Shape sh') => Subshape sh sh' where
  addDim     :: sh -> sh' -> sh    
  modDim     :: sh -> sh' -> sh    
  inject     :: sh -> sh' -> sh
```


The method `addDim` adds the sizes of two shapes (or positions of two indices). If `sh'` is a strict subshape of
`sh`, the fields of `sh` are copied when no corresponding fields of `sh'` exist, accordingly for `modDim`

## Operations on Arrays and Delayed Arrays

### Array Creation and Conversion


Strict arrays are simply defined as record containing a flat data array and shape information:

```wiki
data Array dim e where
  Array:: { arrayShape    :: dim                -- ^extend of dimensions
          , arrayData     :: U.Array e          -- flat parallel array
           }  -> Array dim e
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

```wiki
toDArray:: (U.Elt e, Array.Shape dim)   => Array.Array dim e -> DArray dim e
fromDArray:: (U.Elt e, Array.Shape dim) => DArray dim e      -> Array dim e
```


the result of `toDArray` is a DArray which contains an indexing function into 
an array. In general, the function `dArrayFn` can be much more complex.  The function 
`forceDArray`  (should this be called `normalizeDArray`?) forces the evaluation `dArrayFn` on
every index of the range, and replaces `dArrayFn` by a simple indexing function into an array
of the result values. 

```wiki
forceDArray:: (U.Elt e, A.Shape dim) => DArray dim e -> DArray dim e
```

## Collection Oriented Operations

### Elementwise Application of Functions


The `map` operation takes a function over element types and applies it to every
data element of the DArray, which can have arbitrary dimensionality. Note that 
it is not possible to use this function to apply an operation for example to every
row or column of a matrix. We will discuss how this can be done later on.

`map:: (U.Elt a, U.Elt b, A.Shape dim) =>  (a -> b) -> DArray dim a -> DArray dim b`


Similarily, `zip` and `zipWith` apply to every data element in the array as well. Both arguments
have to have the same dimensionality (which is enforced by the type system). If they have a different
shape, the result will have the intersection of both shapes. For example, zipping an array of shape
`(() :*: 4 :*: 6)` and `(() :*: 2 :*: 8)` results in an array of shape `(() :*: 2 :*: 6)`.

```wiki
zipWith:: (U.Elt a, U.Elt b, U.Elt c, A.Shape dim) => 
  (a -> b -> c) -> DArray dim a -> DArray dim b-> DArray dim c
zip:: (U.Elt a, U.Elt b, A.Shape dim) => 
  DArray dim a -> DArray dim b-> DArray dim (a :*: b)
```


The function `fold` collapses the values of the innermost rows of  an array of at least dimensionality 1.

```wiki
fold :: (U.Elt e, A.Shape dim) => 
 (e -> e-> e) -> e -> DArray (dim :*: Int)  e  -> DArray dim e
```


Again, it's not possible to use `fold` directly to collapse an array along any other axis, but, as 
we will see shortly, this can be easily done using other functions in combination with `fold`.

```wiki

TODO: MISSING: description of mapStencil
```

### Reordering, Shifting, Tiling


Backpermute and default backpermute are two very versatile operations which allow 
the programmer to express all structural operations which reorder or extract
elements based on their position in the argument array:

```wiki
backpermute:: (U.Elt e, A.Shape dim, A.Shape dim') =>   
  DArray dim e -> dim' -> (dim' -> dim) -> DArray dim' e
backpermuteDft::(U.Elt e, A.Shape dim, A.Shape dim') => 
  DArray dim e -> e -> dim' -> (dim' -> Maybe dim) -> DArray dim' e
```


The function `backpermute` gets a source array, the shape of the new array, and
a function which maps each index of the new array to an index of the source array (and 
thus indirectly provides a value for each index in the new array). Default backpermute  is
additionally provided with a default value which is inserted in the array in cases where the
index function returns `Nothing`. (Remark: should probably be replaced by a default array instead of
default value for more generality)

`reshape arr newShape` returns a new array with the same value as the argument array, but a new shape. The
new shape has to have the same size as the original shape.

```wiki
reshape:: (Shape dim', Shape dim, U.Elt e) => DArray dim e -> dim' -> DArray dim' e
```

### Shape Polymorphic Computations on Arrays


The array operations described in this and the following subsection
are available on both strict and delayed arrays, and yield the same
result, with the exception that in case of delayed arrays, the result
is only calculated once its forced by calling `fromDArray` or `forceDArray`. No
intermediate array structures are ever created.


The library provides a range of operation where the dimensionality of
the result depends on the dimensionality of the argument in a
non-trivial manner, which we want to be reflected in the type system. 
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