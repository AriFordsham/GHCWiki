## Vectorisation


This page describes our approach to implementing vectorisation by extending our earlier implementation of closure conversion.  A central aspect is the ability to mix modules compiled with vectorisation with modules compiled without vectorisation.

### General strategy


Vectorisation is only going to **add** additional code and data structures to a module.  All the sequential code remains unmodified.  In particular, while vectorised code will call unvectorised code, there are no explicit static calls of unvectorised code into vectorised code.  Nevertheless, dynamic control flow can move from unvectorised to vectorised code by way of vectorised functional values being passed to higher-order unvectorised code.


NB: This is a significant departure from our earlier plan, where the original definition of a function `f` would be modified to call its vectorised variant `f_v` to do the actual work.  We changed our mind on this, as the new configuration appears to be simpler to implement.


From the policy of unvectorised code never directly calling vectorised code, it follows that the `Main` module of a program needs to be compiled with vectorisation enabled if the program is to make any use of vectorised code at all.  Moreover, as `Main.main` is an `IO` function and we certainly don't want to vectorise the `IO` monad, vectorisation will need to *partially vectorise* expressions by vectorising any subexpressions it can vectorise in any expression that cannot be vectorised in its entirety.  Consider the following example:

```wiki
main :: IO ()
main
  = do
      [nStr] <- getArgs
      let n = read nStr :: Int
      print $ sumP [:i*i | i <- [:1..n:]:]
```


Let us assume that the functions `getArgs`, `print`, and `read` are defined in modules that have not been vectorised.  When vectorising `main`, we want to use the vectorised versions of the functions `sumP`, `mapP` (implied by the comprehension), and `enumFromToP` (implied by the array constructor).  However, all the rest of the code will remain largely unchanged.  (What *largely unchanged* means precisely, we still have to define.)  In fact, there will be two versions of `main`.  The original `main` function that, according to our policy, does not use any vectorised code and `main_v` that has all the array code properly vectorised and all the enclosing `IO` code largely unchanged.  In order to make use of vectorisation, the runtime system will have to invoke `main_v`, not `main`.  Moreover, the code calling `main_v` will have to first set up the thread gang and whatever other extra initialisation is needed.  Whether to execute `main` or `main_v` on program start up is determined by whether the `-fvect` option is present during linking or not.

### Two array libraries


We have to different array libraries:

1. The library in `GHC.PArr`, which defines the wired in array constructor `[:.:]`.  It implements `[:.:]` as a parametric data type represented by vanilla boxed arrays.  It does not involve any type class and also no indexed types.  This code is used whenever arrays are mentioned in unvectorised code (i.e., in both all code of unvectorised modules and in the original versions of functions in vectorised modules).
1. The library in package ndp, which defines a type class `PA` and its associated data type `PArr`.   The type `PArr` implements a flattened array representation for all types `t` for which there is a `PA t` instance.


Vectorisation transforms all uses of functions from `GHC.PArr` into uses of package ndp.  It can obviously only do that for computations manipulating values for whose type we have `PA` instances.

### Transformations


We have a number of transformations that together realise vectorisation:

- [Type vectorisation:](data-parallel/vectorisation/type-vectorisation) Similar as in closure-conversion, we have to convert types.  And similar as in closure conversion, the main effect here is the representation of functions.  In addition to the use of closure instead of plain functions, we need towo versions of each function: (1) a scalar version and (2) a version lifted into vector space.
- [Code vectorisation:](data-parallel/vectorisation/code-vectorisation) This includes closure conversion and the pairing of scalar and lifted code.
- Code lifting:? This converts operations on types `t` into operations on types `[:t:]`.

---

#### TODO


Items that need to be addressed on the page:

- Mixed use of the CC isomorphism strategy and a `PA` class for arrays.
- Introduction of `PA` dictionary arguments right after each big lambda in vectorised code.
- Treatment of unboxed values and functions from `GHC.Prim`.
- Transformation schemata
- Examples
