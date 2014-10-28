# The magic `oneShot` function

## Synopsis


This proposal proposes to add a function

```wiki
GHC.Exts.oneShot :: (a -> b) -> (a -> b)
```


to the library. Semantically, it is the identity, but in addition it tell the compiler to assume that `oneShot f` is called at most once, which allows it to optimize the code more aggressively.

## Motivation


The idea first came up in the context of making foldl a good consumer: [ticket:7994\#comment:7](https://gitlab.haskell.org//ghc/ghc/issues/7994)


With the implementation

```wiki
foldl k z xs = foldr (\v fn -> oneShot (\z -> fn (k z v))) id xs z
```


the regular arity analysis of GHC would be able to clean up after fusing this with a consumer. In this sense, the `oneShot` is an alternative to [CallArity](call-arity) (but they are not mutually exclusive, and we probably want both).


Later, David Feuer creatd more good consumers that would rely on such eta-expansion to produce good code, such as `scanl`. Using `oneShot` in these would make this transformation more reliable, in particular when [CallArity](call-arity) fails.


Nofib shows (only) one case where – in the presence of Call Arity – this definition yields better results: `fft2` improves allocations by 1.5%. TODO Why does Call Arity fail here?

## Implementation


GHC already supports one-shot lambdas, see `setOneShotLambda` in `Id.lhs`. We propose to implement `oneShot` as a built in function, similar to `lazy` etc. The branch `wip/oneShot` contains a first prototype: source:ghc/compiler/basicTypes/MkId.lhs?rev=wip/oneShot\#L1124


The crucial bit is to apply `setOneShotLambda` on the lambda’s binder in the unfolding, and to inline `oneShot` aggressively.

## Challenges

### Preservation of `setOneShotLambda` in Core2Core transformations


Previously, the `oneShotInfo` was not very valuable: It was determined by the compiler itself (TODO Where exactly?), so not much is lost if a transformation would reset the flag, as a later phase could re-calculate it.


Now, the `oneShotInfo` can come from the use as well, and resetting it would lose the information irrevocably. Therefore, transformations need to be more careful about preserving it, while keeping it correct.


For example the a CSE run could transform

```wiki
   let f = \x[OneShot] -> ...x...
       g = \x[OneShot] -> ...x...
   in f () + g ()
```


to

```wiki
   let f = \x[OneShot] -> ...x...
   in f () + g ()
```


but now the `OneShot` flag is not true any more. So likely it should be reset. OTOH, this might contradict the user’s intentions. Should CSE be aware of this and avoid CSE’ing these function? Or maybe leave the `OneShot` in place, even if incorrect at first glance, on the grounds that the only effect an incorrect `OneShot` annotation has will be to un-do the CSE?


Observed placed where the flag may be lost:

- Unconditionally in `CoreTidy`, easily fixed.
- *anything else?*


The good things is that this can be tackled incrementally. Unless we give hard guarantees about the effect of `oneShot` (which we don’t), resetting it does not make programs go wrong, and not slower than if there is no `oneShot` annotation at all. So we improve over the previous state, and can keep improving as we make the transformation pay closer attention to this flag.

### Preservation of `setOneShotLambda` across module boundaries


For the motivational cause to work the `oneShot` information needs to prevail in unfoldings and across interfaces.


The cleanest solution is to serialize the `OneShot` bit in interface files. To that end, the constructor

```wiki
IfaceLam :: IfaceBndr -> IfaceExpr -> IfaceExpr
```


needs to be changed to 

```wiki
IfaceLam :: IfaceOneShot -> IfaceBndr -> IfaceExpr -> IfaceExpr
```


with

```wiki
data IfaceOneShot = IfaceNoOneShot | IfaceOneShot 
```


The current `wip/oneShot` branch uses a different solution (avoiding inlining `oneShot` in unfoldings and rules), but this is not nice and fragile.

## Wild, unverified ideas


Can the IO state hack be avoided if `oneShot` is used in the right places in library code, e.g. in IO’s definition of `>>=`?
