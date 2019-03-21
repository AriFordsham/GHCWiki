Hi, I've recently my problem to Haskell IRC related to Quantified Constraints and I've been told that my problem might actually be a bug with GHC.
I've also been given a minimal reproducible snippet:

```
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
import GHC.Exts (Constraint)

class (forall a. A t a => A t [a]) => B t where
    type A t a :: Constraint

instance B t => B [t] where
    type A [t] a = A t a
```
This results in the following error:
```
    • Could not deduce: A t [a]
        arising from the superclasses of an instance declaration
      from the context: B t
        bound by the instance declaration
        at /home/bgavran/code/CompositionalDeepLearning/src/Test.hs:5:10-21
      or from: A [t] a
        bound by a quantified context
        at /home/bgavran/code/CompositionalDeepLearning/src/Test.hs:1:1
    • In the instance declaration for ‘B [t]’
  |
5 | instance B t => B [t] where type A [t] a = A t a
  |          ^^^^^^^^^^^^
```

Now, this is about as much as I can help - this is already well beyond my level of understanding and I hope the error messages are meaningful to somebody here. 
I'm also not sure if this bug was already reported or if I've missed a step in reporting.
Also feel free to change the title to a more descriptive one - I'm not sure how to precisely describe the bug.

This is on GHC 8.6.4