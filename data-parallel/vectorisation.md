## Vectorisation


This page describes our approach to implementing vectorisation by extending our earlier implementation of closure conversion.  A central aspect is the ability to mix modules compiled with vectorisation with modules compiled without vectorisation.

### General strategy


Vectorisation is only going to **add** additional code and data structures to a module.  All the sequential code remains unmodified.  In particular, while vectorised code will call unvectorised code, there are no explicit static calls of unvectorised code into vectorised code.  Nevertheless, dynamic control flow can move from unvectorised to vectorised code by way of vectorised functional values being passed to higher-order unvectorised code.


NB: This is a significant departure from our earlier plan, where the original definition of a function `f` would be modified to call its vectorised variant `f_v` to do the actual work.  We changed our mind on this, as the new configuration appears to be much simpler to implement.


From the policy of unvectorised code never directly calling vectorised code, it follows that the `Main` module of a program needs to tbe compiled with vectorisation enabled if the program is to make any use of vectorised code at all.  Moreover, as `Main.main` is an `IO` function and we certainly don't want to vectorise the `IO` monad, vectorisation will need to *partially vectorise* expressions by vectorising any subexpressions it can vectorise in any expression that cannot be vectorised in their entirety.  Consider the following example:

```wiki
main :: IO ()
main
  = do
      [nStr] <- getArgs
      let n = read nStr :: Int
      print $ sumP [:i*i | i <- [:1..n:]:]
```


Let us assume that the functions `getArgs`, `print`, and `read` are defined in modules that have not been vectorised.  When vectorising `main`, we want to use the vectorised versions of the functions `sumP`, `mapP` (implied by the comprehension), and `enumFromToP` (implied by the array constructor).  However, all the rest of the code will remain largely unchanged.  (What *largely unchanged* means precisely, we still have to define.)  In fact, there will be two versions of `main`.  The original `main` function that, according to our policy, does not use any vectorised code and `main_v` that has all the array code properly vectorised and all the enclosing `IO` code largely unchanged.  In order to make use of vectorisation, the runtime system will have to invoke `main_v`, not `main`.  Moreover, the code calling `main_v` will have to first set up the thread gang and whatever other extra initialisation is needed.  Whether to execute `main` or `main_v` on program start up should probably be determined by whether the `-fvect` option is present during linking or not.
