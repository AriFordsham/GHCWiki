
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

### Representation, Order of Elements, and Lifted Values


As mentioned when introducing the functions `toIndex` and `range`, the following relationship should hold:

```wiki
map (toIndex sh) (range sh) = [:0..(size sh)-1:]
```


this means that, for example

```wiki
range (() :*: 2 :*: 3) =
   [() :*: 0 :*: 0, () :*: 0 :*: 1,  .....
```


A scalar value `c`is isomorphic to a zero-dimensional array

```wiki
  DArray () (\_ -> c)
```


Lifting a scalar value over a shape \`dim':

```wiki
Shape dim => DArray dim (\_ -> c)
```

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
data element of the DArray, which can have arbitrary dimensionality.

```wiki
map:: (U.Elt a, U.Elt b, A.Shape dim) =>  (a -> b) -> DArray dim a -> DArray dim b
```


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

### Shape Polymorphic Computations on Arrays


The library provides a range of operation where the dimensionality of
the result depends on the dimensionality of the argument in a
non-trivial manner, which we want to be reflected in the type system. 
Examples of such functions are generalised selection, which allows for 
extraction of subarrays of arbitrary dimension, and generalised replicate,
which allows replication of an array in any dimension (or dimensions). For example,
given a three dimensional matrix, we can use select to extract scalar element values,
rows, columns, as well as two dimensional matrices along any of the three axes.


For selection, we can informally state the relationship between dimensionality of
the argument, the selector, and the result as follows:

```wiki
select:: Array dim e -> <select dim' of dim array> -> Array dim' e
```


Another example for such a generalised function would be a generalised
`map`, which can apply a function to all elements, all rows, all
columns, or submatrices of different orientation of a multidimensional
array.


For the former example, we need a way to express the relationship between the
shape of the argument and the shape and orientation of the result, as well as
the numerical position of the structure (i.e., first, second, third element). 
In case of the generalised `map`, we don't need the numerical information, since
the operation will be applied to all elements, rows, columns etc. 


To express this dependency between input and output shape and orientation,
as well as possibly a concrete position,  the library provides the `Index` GADT, 
which expresses a relationship between the source and the projected dimension. 
It is defined as follows:

```wiki
data Index a initialDim projectedDim where
  IndexNil   :: Index a initialDim initialDim
  IndexAll   :: (Shape init, Shape proj) =>      
                   Index a init proj -> Index a (init :*: Int) (proj :*: Int)
  IndexFixed :: (Shape init, Shape proj) => a -> 
                   Index a init proj -> Index a (init :*: Int)  proj
```


To refer to a specific element, the type parameter `a` is instantiated with the type `Int`, otherwise
with the unit type:

```wiki
type SelectIndex = Index Int
type MapIndex    = Index ()
```


Given this definition, the type of `select` now becomes

```wiki
select:: (U.Elt e, Shape dim, Shape dim') => Array dim e -> SelectIndex dim dim'  -> Array dim' e
```


Example:

```wiki
arr:: Array (() :*: Int :*: Int :*: Int) Double

arr' :: () :*: Int :*: Int
arr' = select arr (IndexFixed 3 (IndexAll (IndexAll IndexNil))) 
```


We could generalise this further, to extract from any array `arr` which is at least one dimensional 
the third element:

```wiki
arr:: Shape dim => Array (dim :*: Int) Double

arr' :: Array dim Double
arr' = select arr (IndexFixed 3 IndexNil)
```


The index type is also used to express the type of generalised replicate

```wiki
replicate:: Array dim' e -> SelectIndex dim dim'  -> Array dim e
```


which, given an array, can be used to expand it along any dimension. For example,

```wiki
simpleReplicate:: (U.Elt e, Shape dim) => Array dim e -> Int -> Array (dim :*: Int) e
simpleReplicate arr n =
  replicate arr (IndexFixed n IndexNil)
```


replicates the argument array (which can of any dimensionality) `n` times and behaves
thus similarly to list replicate, whereas 

```wiki
elementwiseReplicate:: (U.Elt e, Shape dim) => 
  Array (dim :*: Int) e -> Int -> Array (dim :*: Int :*: Int) e
elementwiseReplicate arr n =
  replicate arr (IndexAll (IndexFixed n IndexNil))
```


replicates each element of an array `n` times (similarly to `map (replicate n)` on lists).


Even though the index type is well suited to express the relationship
between the selector/multiplicator and the dimensionality of the
argument and the result array, it is inconvenient to use, as the
examples demonstrate. We therefore do need to add another layer to
improve the usability of the library.


Note that the library provides no way to statically check the pre- and
postconditions on the actual size of arguments and results. This has
to be done at run time using assertions.

## \`Nesting' Array Functions


We already introduced the `map` function, which applies a given function to all data elements
of an array:

```wiki
map:: (U.Elt a, U.Elt b, A.Shape dim) =>  (a -> b) -> DArray dim a -> DArray dim b
```


We can't use this function, however, to apply a function to all columns, rows, or other sub-arrays of 
a multidimensional array, and generalising `map` to be able to handle this wouldn't make sense
in this framework. Consider, for example, a function `filter`, which takes a one-dimensional 
array and creates a new array containing only the even elements of the argument array. If we mapped
this function over all the rows of a two-dimensional array, the resulting structure would, in general,
not be a two dimensional array anymore, since each row might potentially have a different length. 
Therefore, we restrict the class of functions that can be mapped over sub-arrays to functions where 
the shape of the argument determines the shape of the result. All `mappable` functions can be implemented
such that they abstract over the exact dimensionality of their argument, and have the type

```wiki
f::(A.Shape dim, U.Elt e, U.Elt e') => 
  DArray (dim :*: Int ..... :*: Int) e ->  DArray (dim :*: Int :*: .... :*: Int) e'
```


and those functions can be trivially mapped since

```wiki
 map f = f
```


So, for example, we can write a mappable function which takes an array and selects every data element with 
an even index:

```wiki
  selectEven:: (A.Shape dim, U.Elt e) => DArray (dim :*: Int) e -> DArray (dim :*: Int) e 
  selectEven (DArray (sh :*: n ) f =
     DArray (sh :*: n `div` 2) (\(sh :*: n) -> f (sh :*: 2*n)
```

## Example 1: Matrix multiplication


As a simple example, consider matrix-matrix multiplication. We can either implement it by directly manipulating the array
function, or use the operations provided by the DArray library. Let as start with the former, which is more fairly similar to
what we would write using loops over array indices:

```wiki
mmMult1:: 
  DArray (() :*: Int :*: Int)  Double -> DArray (() :*: Int :*: Int)  Double -> DArray (() :*: Int :*: Int)  Double  
mmMult1 arr1@(DArray (() :*: m1 :*: n1) _) arr2@(DArray (() :*: m2 :*: n2) _) = 
  mapFold (+) 0 arrDP
  where 
    arrDP = DArray (():*: m1 :*: n2 :*:n1) 
       (\(() :*: i :*: j :*: k) -> (index arr1 (() :*: i :*: k)) * (index arr2 (() :*: k :*: j)))
```


In the first step, we create the intermediate three dimensional array which contains the products of all 
sums and rows, and in the second step, we collapse each of the rows to it's sum, to obtain the two dimensional
result matrix. It is important to note that the elements of `arrDP` are never all in memory (otherwise, the memory
consumption would be cubic), but each value is consumed immediately by `mapFold`. 


This implementation suffers from the same problem a corresponding C implementation would - since we access one
array row-major, the other column major, the locality is poor. Therefore, first transposing `arr2` and adjusting the
access will actually improve the performance by approximately 40%: 

```wiki
mmMult1:: 
  DArray (() :*: Int :*: Int)  Double -> DArray (() :*: Int :*: Int)  Double -> DArray (() :*: Int :*: Int)  Double  
mmMult1 arr1@(DArray (() :*: m1 :*: n1) _) arr2@(DArray (() :*: m2 :*: n2) _) = 
  mapFold (+) 0 arrDP
  where 
    arr2T = forceDArray $ transpose arr2
    arrDP = DArray (():*: m1 :*: n2 :*:n1) 
       (\(() :*: i :*: j :*: k) -> (index arr1 (() :*: i :*: k)) * (index arr2T (() :*: j:*: k)))
```


However, we do need to force the actual creation of the transposed array, otherwise, the change would have no effect at all. We therefore 
use `forceDArray`, which converts it into an array whose array function is a simple indexing operation (see description of `forceDArray` above). This means that the second version requires more memory, but this is offset by improving the locality for each of the multiplications. 

```wiki
-- mmMult:: (Array.RepFun dim, Array.InitShape dim, Array.Shape dim) => 
--   DArray (dim :*: Int :*: Int)  Double -> DArray (dim :*: Int :*: Int)  Double -> DArray (dim :*: Int :*: Int)  Double  
mmMult::
   DArray (() :*: Int :*: Int)  Double -> DArray (() :*: Int :*: Int)  Double -> DArray (() :*: Int :*: Int)  Double  
mmMult arr1@(DArray (sh :*: m1 :*: n1) fn1) arr2@(DArray (sh' :*: m2 :*: n2) fn2) = 
  assert ((m1 == n2) && (sh == sh')) $ 
    mapFold (+) 0 (arr1Ext * arr2Ext)
--  'fold' doesn't fuse at the moment, so mapFold is significantly faster
--  fold (+) 0 $ zipWith (*) arr1Ext arr2Ext
  where
    arr2T   = forceDArray $ transpose arr2  -- forces evaluation of 'transpose'
    arr1Ext = replicate arr1 (Array.IndexAll (Array.IndexFixed m2 (Array.IndexAll Array.IndexNil)))
    arr2Ext = replicate arr2T
                 (Array.IndexAll (Array.IndexAll (Array.IndexFixed n1 Array.IndexNil)))

```

## Open Questions

### Do we need array comprehension on DArrays?

### Wave computations

### Generalised Stencil Operation


MapStencil is currently not lifted, and doesn't run in parallel. Need to come up with a generalised version. 
