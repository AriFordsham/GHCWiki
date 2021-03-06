## Type-Level Operations


Currently, we provide the following type-level operations on natural numbers:

```wiki
(<=) :: Nat -> Nat -> Prop    -- Comparison
(+)  :: Nat -> Nat -> Nat     -- Addition
(*)  :: Nat -> Nat -> Nat     -- Multiplication
(^)  :: Nat -> Nat -> Nat     -- Exponentiation
```


Notes:

- `(<=)` is a 2-parameter class (that's what we mean by the "kind" Prop),
- `(+)`, `(*)`, and `(^)` are type functions.
- Programmers may not provide custom instances of these classes/type-families.


The operations correspond to the usual operations on natural numbers.

## Inverse Operations


Our system does not have explicit functions for subtraction, division, logs, or roots.  However, we can get essentially the same functionality by combining the existing type functions with (implicit or explicit) equality constraints.  Consider, for example, the following type:

```wiki
bytesToWords :: Array (8 * w) Word8 -> Array w Word64
```


In this type we are basically dividing the size of the input array by 8.  Note, however, that we have expressed this by specifying that the array has to be a multiple of 8, which avoids the need for partiality.

## Solving Constraints


There is a set of built-in instances, defining the behavior of each the type-level operations.  These instances are consistent with the theory of arithmetic on natural numbers but they are not complete (i.e., GHC is not perfect at math).  This means that GHC might reject some programs
because it cannot solve all the necessary constraints, even though the constraint can be solved in the general theory of natural numbers.  The most common cause of this is when a programmer writes down a type signature, but GHC infers a slightly different type for the implementation.  Now, GHC needs to check that the specified type is compatible with the implementation.  If it fails to do this, then the program will be rejected.  The usual work-around in such situations is to modify the type signature so that it lists explicitly the constraints that GHC could not solve.  If you encounter the same problem often, please consider sending an e-mail to the GHC mailing list to let us know.  We might be able to teach GHC some more math!
