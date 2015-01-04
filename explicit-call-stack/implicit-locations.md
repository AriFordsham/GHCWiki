# Implicit locations


This is a lightweight proposal to help with error-finding; see the [ExplicitCallStack](explicit-call-stack) summary page.

## Specification of the feature


Suppose we write

```wiki
f :: [a] -> Int
f []     = error ("Failure at " ++ show (?location :: Location))
f (x:xs) = ...
```


The idea is that `(?location :: Location)` expands to the current source location.


It is written in the syntax of [implicit parameters](http://www.haskell.org/ghc/docs/7.8.2/html/users_guide/other-type-extensions.html#implicit-parameters) because the intention is that it can (if you wish) be passed implicitly from the caller, like this:

```wiki
f :: (?location :: Location) => [a] -> Int
f []     = error ("Failure at " ++ show ?location)
f (x:xs) = ...
```


Now the displayed location will be the location of `f`'s *call site*, rather than the location of the call to `show` inside `f`.  In fact `Location` is a value that captures a *stack* of source-locations, so that you get to see both.


So `?location :: Location` is a rather special implicit parameter, but the *only* special-ness is in the typechecker.  Specifically, when solving a "wanted" constraint `w :: (?location :: Location)`:

- If there is an enclosing "given" constraint `g :: (?location :: Location)`, usually from a type signature, the wanted constraint is solved from the given one, by pushing on `w`'s source location, something like

  ```wiki
    w = push <w's srcloc> g
  ```

- If there is no enclosing "given" constraint, the wanted constraint is solved with a unit stack:

  ```wiki
    w = push <w's srcloc> <emptyloc>
  ```


So the type checker will never report an insoluble constraint `(?location :: location)`.


The `Location` type is abstract, but has functions to:

- Turn it into a list of `SrcLoc`
- From a `SrcLoc` you can get line number, character position, file name, module name, package name, etc.


(The names are entirely up for grabs here.)

## Discussion

- This design makes it possible to grab the current source location. But it *also* makes it convenient to get the call site location, if that's what you want, in a very similar way to [ExplicitCallStack/FindingTheNeedle](explicit-call-stack/finding-the-needle).  It doesn't do everything, by any means, and is probably best regarded as a slight enhancement to a basic "give me the source location" mechanism.  But it it's extremely simple and cheap.

- The implicit parameter story answers the open questions in the finding-the-needle paper. (You may not like the answer, but the answer is at least clear.)  For example, consider a CAF

  ```wiki
  xs :: [Int]
  xs = ...blah....
  ```

>
> Here we will not propagate a stack from `xs`'s useage site.  But if we wrote
>
> ```wiki
> xs :: (?location :: Location) => [Int]
> xs = ...blah...
> ```
>
>
> then we would.  Of course, that would mean `xs` was evaluated once per usage site, but at least that fact becomes 100% clear.

>
> Similarly, overloading behaves predictably. Consider
>
> ```wiki
> class C a where
>   op :: a -> a
> instance C Int where
>   op 0 = ....crash...
>   op n = .....
> ```
>
>
> Since `op`'s signature does not have `(?location :: Location`), the crash does not have access to `op`'s call site. You would have to change the class signature to get that.  Doing so might be fine if it's your class, awkward if it's a library class, and impossible if it's a Prelude class.

- There's an open design choice for functions with an inferred type, e.g.

  ```wiki
  f [] = ....(?location :: Location)...
  f (x:xs) = ...
  ```

  Do we infer a constraint `(?location :: Location)`, and thereby inherit information from the call site, but impose a small runtime overhead?  Or do we solve the constraint locally, and refrain from passing call-site information.  I'm inclined to the latter. You pay only if you ask.

- I've been writing `?location :: Location`, but it's the type name that matters. It'd be fine to use any name for the implicit parameter itself, e.g. `?loc :: Location`

- GHC has number of other special cases in the constraint solver: for `Coercible`, `Has` and `Upd` (overloaded record fields), so another special case there is not a big deal.  There are no other implications for the compiler whatsoever.

## Implementation Details


These notes are based on the proposed implementation at [ https://phabricator.haskell.org/D578](https://phabricator.haskell.org/D578).

- We add a new module `GHC.Location` that exports two datatypes

  1. `SrcLoc`, a single source location including package/module/file names and a source span, and
  1. `CallStack`, a stack of `SrcLoc`s.
    Both datatypes are currently kept abstract so only GHC can create new values. I think this is the "right" thing to do as it makes the types more trustworthy, but perhaps there's a good argument for allowing users to update `SrcLoc`s and `CallStack`s.

- GHC completely ignores the name of an implicit CallStack parameter, e.g.

  ```wiki
  f = show (?loc :: CallStack)
  ```

  and

  ```wiki
  g = show (?location :: CallStack)
  ```

  are both valid. But furthermore, in

  ```wiki
  f :: (?loc :: CallStack) => IO ()
  f = print (?location :: CallStack)
  ```

  the printed call-stack will **also include**`f`s call-site. This last example might be unsettling to some. I think the behavior will ease creation of cross-package call-stacks, but could be convinced that it veers too far from what users would expect of ImplicitParams.

- Responding to the open question above, GHC will **never** infer an implicit CallStack constraint.
