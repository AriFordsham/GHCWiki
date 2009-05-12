# Regular, Multi-dimensional parallel Arrays


The nested parallel arrays of DPH could be used to model regular arrays, as we could simply either create segment information using replicate, or define regular arrays in terms of flat parallel arrays and separately stored dimensionality information, and define operations on these arrays in a library in terms of the nested operations. However, there are two main reasons why this is unsatisfactory: convenience and efficiency. 

### Convenience


Languages like SAC, which provide high-level support for operations on multi-dimensional arrays, offer shape invariant operations. If we want to model this on a library level, we either have to give up type safety to a
large extend (for example, by encoding the shape as a list of integer values whose length is proportionate to its dimensionality) or use sophisticated language features like GADTs, which may impede the usability of the library for inexperienced users.

## The regular array type


 
Regular parallel arrays are similar to arrays in SAC, with one major
difference: array operations in DPH are fully typed, and consequently, what
is called 'shape invariant programming' in SAC works differently in DPH.


An multidimensional array is parametrised with its dimensionality and its
element type:

```wiki
(Ix (Shape dim), Elt a)  => Array dim a 

```


where Shape is a type family defined on tuples of integers, including nullary
tuples - arrays of which correspond to scalar values. So, for example

```wiki
  Array ()      Double    -- scalar double precision floating point value
  Array (3,2) Double   -- two dimensional array (matrix) of three rows, two columns
```


Internally, shapes are represented as nested pairs

```wiki
type family Shape dim
type instance Shape () = ()
type instance Shape (Int) = ((),Int)
type instance Shape (Int, Int) = (((),Int), Int)
```


Elements types are restricted to the element type of flat parallel
arrays, that it, primitive types like integers, boolean and floating
point numbers, and tuples.

## Operations

### Creating Arrays


A new arrays can be created from flat parallel arrays 

```wiki
fromNArray:: U.Elt r => U.Array r -> Array DIM1 r
```


and from scalar values:

```wiki
fromScalar::  U.Elt r => r -> Array DIM0 r
```

### Manipulating array values


All the shape invariant operations available on parallel arrays are also defined on regular arrays:

```wiki
map     :: (Elt a, Elt b) => (a -> b) -> Array dim a -> Array dim b
zipWith :: (Elt a, Elt b, Elt c) => (a -> b -> c) -> Array dim a -> Array dim b -> Array dim c
```


Parallel array operations which can change the shape are restricted to one dimensional arrays, to make sure that the 
result is still a regular array. 

```wiki
filter :: Elt a => (a -> Bool) -> Array DIM1 a -> Array DIM1 a
scan        :: Elt a => ((a, a) -> a)            -- combination function
              -> a                               -- default value
              -> Array (dim, Int) a              -- linear array
              -> (Array dim a, Array (dim, Int) a)
```


Manipulating the shape of arrays:

```wiki
reshape     ::(Ix (Shape dim), Ix (Shape dim')) =>
                 (Shape dim)                  -- new shape
              -> Array dim' a                 -- array to be reshaped
              -> Array dim a
```

### Changing the dimensionality of an array

#### The index type


Two operations we provide change the dimensionality of an argument
array, namely the generalised indexing function, which extracts
subarrays from an multidimensional array, and generalised replicate,
which expands the array along specified axes. To encode the
relationship between the argument's dimensionality and the result's dimensionality, 
we use the Index type. 

```wiki
(!) :: Elt e => Arr dim e -> Index dim dim' -> Arr dim' e

replicate   :: Index dim' dim                     -- ^specifies new dimensions
              -> Array dim a                      -- ^data to be replicated
              -> Array dim' a

```


where Index is defined as

```wiki
data Index initialDim projectedDim where
  IndexNil   :: Index () ()
  IndexAll   :: Index init proj -> Index (init, Int) (proj, Int)
  IndexFixed :: Int -> Index init proj -> Index (init, Int)  proj
```

#### Examples


To demonstrate the use of the Index type, consider the following replicates expressed in terms of generalised replicate:

```wiki
-- 'chunk replicate' - create a two dimensional array by replicating the one dimensional 
-- argument array n times
replicateC:: Int -> Array DIM1 a -> Array DIM2 a
replicateC n arr = RArray.replicate       (IndexFixed n (IndexAll IndexNil))  arr

-- create a two dimensional array by replicating each element n times
replicateL:: Int -> Array DIM1 a -> Array DIM2 a
replicateL n arr = RArray.replicate (IndexAll (IndexFixed n IndexNil))  arr


replicateC2:: Int -> Array DIM2 a -> Array DIM3 a
replicateC2 n arr = RArray.replicate       (IndexFixed n (IndexAll (IndexAll IndexNil)))  arr
 

replicateLL:: Int -> Array DIM2 a -> Array DIM3 a
replicateLL n arr = RArray.replicate (IndexAll (IndexAll (IndexFixed n IndexNil)))  arr
```


The use of the index type is not very intuitive, and it should be
hidden from the user, preferably by offering syntactic support similar to SACs dot-notation.
