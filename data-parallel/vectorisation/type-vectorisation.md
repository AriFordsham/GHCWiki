## Type Vectorisation


The transformation of types includes the generation of lifted types, the pairing of scalar with lifted computations, and closure conversion.

### Unboxed types


Unboxed types and functions defined in `GHC.Prim` need to be treated specially during vectorisation.  This is as we cannot have `PA` instances for unboxed types and the transformation needs to know which functions from `GHC.Prim` can be safely parallelised (e.g., its fine to run many `(+#)` in parallel, whereas this is not really advisable for calls to side-effecting RTS functions).  Indeed, we might regard unboxed types and functions from `GHC.Prim` as the place where we make the transition from implementing vectorisation decisions in package ndp to hard-coding them into the compiler.  It is probably a good idea to eventually move as much as possible of the hardcoded information into `primops.txt.pp`, but for the moment, we simply hardcode everything in the modules in `vectorise/`.


To treat unboxed types properly, we cannot simply use the type constructor `PArr` wherever we need a flattened array; instead, we define a type translation `t^` that treats unboxed types specially; e.g., `Int#^ = UArr Int`.


We need to represent functions whose argument and/or result type are unboxed different from functions over boxed types.  The reason is the non-standard kinding rule implemented in GHC for `(->)`, which allows that the two argument type variables are instantiated to unboxed values iff the application of `(->)` is saturated.  We can't defined a second type constructor with that property unless we extend the `TypeRep.Type` representation.  We also can't simply use a type synonym for a vectorised type function constructor, because we must be able to partially apply it.

### Vectorisation

TODO

- Types `t1* :-> t2*` and `t1* :=> t2*` include `PArr t1*` and `PArr t2*`; so, we can only use them if we have `PA` instances for these types.


The type transformation rules achieve two goals: (1) they replace original type constructors and variables by their vectorised variants, where those are available, and (2) they alter the representation of functions:

```wiki
T*                   = T_V  , if T_V exists
                     = T    , otherwise
a*                   = a
(t1 -> t2)*
 | isUbxFun (t1->t2) = (t1* -> t2*) :|| (t1^ -> t2^)
 | otherwise         = t1* :-> t2*
(t1 t2)*             = t1* t2*
(forall a.t)*        = forall a.t*
```


When encountering saturated function space applications , we need to distinguish those that involve unboxed types, as we need to remain to be compatible with `(->_v) = (:->)` for boxed types.  (In other words, the distinction cannot simply be based on whether an application is saturated or not, it really needs to be one the basis of the kinds of types involved.)

#### Fixed data constructor mapping

```wiki
(->_v) = (:->)
[::]_v = PArr
```

### Lifting


The lifting of types into vector space is, for all boxed monotypes, denoted by the array family constructor `PArr`.  However, we need to handle the lifting of unboxed types and the extension of signatures with `PA` dictionaries explicitly:

```wiki
Int#^                 = UArr Int
Float#^               = UArr Float
Double#^              = UArr Double
..and so on for other unboxed types..

(forall a.t)^         = forall a. PA a -> t^
t^                    = PArr t*
```


As a consequence, we cannot have impredicative instantiations of `[::]`, but this doesn't seem to be a significant restriction.

### `PArr` family instances


Remember that `PArr` is defined over vectorised types:

```wiki
newtype instance PArr (f :|| (arr -> brr))
  = PArrUFun (f :|| (ACls arr brr))
newtype instance PArr (a :-> b) = PArrFun (a :=> b)
```

### Examples

```wiki
(Int  -> Int)* = Int_V :-> Int_V
(Int# -> Int)* = (Int# -> Int) :|| (UArr Int -> PArr Int)

[:Int  -> Int:]* = PArr (Int_V :-> Int_V)
[:Int# -> Int:]* = PArr ((Int# -> Int) :|| 
                         (UArr Int -> PArr Int))
```


So, we have the `PArrUFun` type instance

```wiki
PArrUFun :: ((Int# -> Int) :|| ACls (UArr Int) (PArr Int))
         -> PArr ((Int# -> Int) :|| (UArr Int -> PArr Int))
```


which constructs parallel arrays containing functions of type `Int# -> Int`.

**Problem:** Pairing an array closure (that only contains a lifted function) with a scalar function that takes no environment seems useless.  We cannot use that scalar function to extract a single element from the array closure, as it cannot make any use of that single element from the environment array.  On the other hand, we cannot include the scalar function into the closure when the function manipulates unboxed tyes...or can we?!?
