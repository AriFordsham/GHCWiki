# Design discussion for improvements to GHCi's `:type` command


This page, created on April 25, 2016, is to discuss the need (if any) to improve GHCi's `:type` command to deal with different use cases and desired output.

## Problem 1: variables available for visible type application


As discussed at length in [\#11376](https://gitlab.haskell.org//ghc/ghc/issues/11376), `:type` instantiates the inferred type before generalizing and reporting to the user. Here is the example: Given `  bar :: forall a b. Show a => a -> b -> a`, what should `:type bar @Int` show (with `-fprint-explicit-foralls`)?

1. `forall b. Show Int => Int -> b -> Int`
1. `forall b. Int -> b -> Int`
1. `forall {b}. Int -> b -> Int`
1. `Int -> b -> Int`


We choose (3), because (1) has an unsolved `Show Int`, (2) is quite hard to arrange for (and may not be fully specified), and (4) is ill-scoped. [\#11376](https://gitlab.haskell.org//ghc/ghc/issues/11376) has much more discussion.


However, the choice of (3) means that there is no way to discover the specified type variables of a type, where "specified" here refers to the type variables available for visible type application. For example, the type `foldr` is parameterized by three type variables, `t :: * -> *`, `a :: *` and `b :: *`, *in that order*, and these variables are available for visible type application. However, with instantiation and generalization, there is no guarantee that the variables' ordering will be maintained, and GHC reports the variables as unavailable for visible type application.

## Problem 2: types can be too general


With the addition of generalization over `Foldable` and `Traversable`, many common functions have very general types. For example:

```wiki
> :t mapM
mapM
  :: forall {t :: * -> *} {m :: * -> *} {a} {b}.
     (Traversable t, Monad m) =>
     (a -> m b) -> t a -> m (t b)
> :t length
length :: forall {t :: * -> *} {a}. Foldable t => t a -> Int
```


Even for me (Richard E.), a somewhat experienced functional programmer, I find these types hard to think about without instantiating. 

## Design questions


Running example: `foo :: forall a m t. (Show a, Monad m, Foldable t) => t a -> m a`. We assume `-fprint-explicit-foralls` throughout.

1. What should the default behavior of `:type` be? Possible answers:

  1. As it is now, deeply instantiating the type and then generalizing.

    - `:t foo @Int` produces `forall {t :: * -> *} {m :: * -> *}. (Monad m, Foldable t) => t Int -> m Int`.
    - `:t foo @Int []` produces `forall {m :: * -> *}. Monad m => m Int`
    - Pros:

      - The type reported is the type that would be inferred for a variable assigned to the given expression.
      - No unsolved constraints.
      - Fully general.
    - Cons:

      - Loses information about specified type variables. (Note that the variables are listed in braces and out of order in the first example output.)
  1. No deep instantiation of the type, only generalize.

    - `:t foo @Int` produces `forall (m :: * -> *) (t :: * -> *). (Show Int, Monad m, Foldable t) => t Int -> m Int`.
    - `:t foo @Int []` produces `forall {m :: * -> *}. Monad m => m Int`.
    - Pros:

      - The specified variables are correct.
      - Any unspecialized variable (such as `m` in the second example) is still given with its kind, but we can see that, e.g., `foo @Int [] @IO` would not work.
    - Cons:

      - If you've used visible type application (but only if you've used visible type application), there may be unsolved constraints, like `Show Int`.
      - The type shown is not what would be assigned to a variable. This may be surprising to users.
  1. No instantiation or generalization.

    - `:t foo @Int` produces `forall (m :: * -> *) (t :: * -> *). (Show Int, Monad m, Foldable t) => t Int -> m Int`
    - `:t foo @Int []` produces `m Int`.
    - Pros:

      - Shows precisely the type of the expression.
    - Cons:

      - No kind for `m` provided in the second example.
      - No `Monad m` constraint provided in the second example.
  1. Specialize the type, as per Design Question 2, below.

    - Pros:

      - Beginners get easy-to-read types.
    - Cons:

      - The type returned from `:type` is not fully general.

1. If we provide a way for GHC to specialize the types, how should we do so? Possible answers:

  1. Apply the monomorphism restriction.

    - `length`'s type is reported as `[a] -> Int`.
    - `mapM` leads to a type error, because `Monad m` cannot be defaulted.
    - Pros:

      - Dead simple to implement.
      - Brief to specify. (Though users may not fully understand the specification.)
      - Customizable via `default` declarations.
      - Only 1 specialization is reported.
    - Cons:

      - Rather limited in applicability only to those types which contain all defaultable constraints.
      - Only 1 specialization is reported.
  1. Default all so-called "interactive" classes, where the interactive classes are: `Num`, `Show`, `Eq`, `Ord`, `Foldable`, and `Traversable`. The default list of default types in GHCi is `()`, `[]`, `Integer`, `Double`.

    - `length`'s type is reported as `[a] -> Int`.
    - `mapM`'s type is reported as `Monad m => (a -> m b) -> [a] -> m [b]`.
    - Pros:

      - Easy enough to specify and implement.
      - Customizable.
      - Only 1 specialization is reported.
    - Cons:

      - Only 1 specialization is reported.
  1. Provide multiple specializations of the types, in a way yet to be specified.

    - `length`'s type might be reported as `[a] -> Int` or `Maybe a -> Int`.
    - `mapM`'s type might be reported as `(a -> [b]) -> [a] -> [[b]]` or `(a -> Maybe b) -> [a] -> Maybe [b]` or `(a -> IO b) -> [a] -> IO [b]` or `(a -> IO b) -> Maybe a -> IO (Maybe b)` or ...
    - Pros:

      - Lots of examples make it easier for the user to generalize internally and understand what is going on.
    - Cons:

      - Verbose.
      - Potentially a great many types to be reported.
      - Not clear how to specify this.

1. How do we provide the non-default behavior? Possible answers:

  1. Have `:type` provide multiple types.

    - Pros:

      - Easy for users to be aware of this.
      - Easy to remember what command to use.
    - Cons:

      - Verbose output to a common operation.
      - Could possibly break tools that read the output of `:type`.
  1. Use `:info` to provide the extra information.

    - Pros:

      - No new commands.
    - Cons:

      - Requires a redesign of `:info`, as `:info` does not print the full type of, e.g., data constructors, class methods, record selectors, etc.
      - Makes `:info` even more verbose.
      - Unavailable for  
