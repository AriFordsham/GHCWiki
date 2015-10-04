# Monad of no `return` Proposal (MRP)


TLDR: To complete the AMP, turn `Monad(return)` method into a top-level binding aliasing `Applicative(pure)`.

## Current Situation


With the implementation of Functor-Applicative-Monad Proposal (AMP)\[1\] and
(at some point) the MonadFail proposal (MFP)\[2\] the AMP class hierarchy
becomes

```
classFunctor f  where
    fmap    ::(a -> b)-> f a -> f b


classFunctor f =>Applicative f  where
    pure    :: a -> f a
    (<*>):: f (a -> b)-> f a -> f b
 
    (*>):: f a -> f b -> f b
    u *> v  =…(<*):: f a -> f b -> f a
    u <* v  =…classApplicative m =>Monad m  where(>>=):: m a ->(a -> m b)-> m b

    return  :: a -> m a
    return  = pure

    (>>):: m a -> m b -> m b
    m >> k  =…classMonad m =>MonadFail m  where
    fail    ::String-> m a
```


Consequently, the `Monad` class is left with a now redundant `return`
method as a historic artifact, as there's no compelling reason to
have `pure` and `return` implemented differently.


Traditionally, `return` is often used where `pure` would suffice
today, forcing a `Monad` constraint even if a weaker `Applicative`
would have sufficed.


As a result, language extensions like `ApplicativeDo`\[3\] have to
rewrite `return` to weaken its `Monad m =>` constraint to
`Applicative m =>` in order to benefit existing code at the cost
of introducing magic behavior at the type level.


Finally, this redundancy becomes even more significant when viewed in
light of the renewed Haskell standardisation process\[7\]: The next
Haskell Report will almost certainly incorporate the AMP (and MFP)
changes, and there's no justification for the Report to retain
`return` as a method of `Monad`. A good reason would have been to
retain backward compatibility with Haskell 2010. However, as the AMP
superclass hierarchy requires `Monad` instances to be accompanied by
`Applicative` instances (which aren't part of Haskell 2010, c.f. \[6\]),
backward compatibility with Haskell 2010 goes out the window when it
comes to defining `Monad` instances (unless via use of `-XCPP` or
similar).  Consequently, meeting the high bar for a formal document
such as the Haskell Report demands that `Monad` shall not carry a
redundant `return` method that serves no purpose anymore. Moreover,
getting `return` out of the way is desirable to facilitate
standardising potential candidates such as the earlier mentioned
`ApplicativeDo` in the future and avoids the technical debt incurred
by keeping around this language wart.

## Proposed Change


Remove `return` as a method from the `Monad` class and in its place
define a top-level binding with the weaker `Applicative` typeclass
constraint:

```
-- | Legacy alias for 'pure' return::Applicative f => a -> f a
return= pure
```


This allows existing code using `return` to benefit from a weaker
typeclass constraint as well as cleaning the `Monad` class from a
redundant method in the post-AMP world.


A possible migration strategy is described further below.

## Compatibility Considerations


Generalizing the type signature of a function from a `Monad`
constraint to its superclass `Applicative` doesn't cause new
type-errors in existing code.


However, moving a method to a top-level binding obviously breaks code
that assumes `return` to be a class method. Foremost, code that
defines `Monad` instances it at risk:

### Instance Definitions


Code defining `return` as part of an instance definition
breaks. However, we had the foresight to provide a default
implementation in `base-4.8` for `return` so that the following
represents a proper minimal instance definition post-AMP:

```
instanceFunctorFoowhere
    fmap g foo  =…instanceApplicativeFoowhere
    pure x      =…
    a1 <*> a2   =…instanceMonadFoowhere
    m >>= f     =…-- NB: No mention of `return`
```


Consequently, it is possible to write forward-compatible instances
that are valid under this proposal starting with GHC 7.10/`base-4.8`.


Heuristically `grep`ing through Hackage source-code reveals a
non-negligible number of packages defining `Monad` instances with
explicit `return` definitions\[4\]. This has a comparable impact to the
AMP, and similarly will require a transition scheme aided by compiler
warnings.

### Module Import/Export Specifications


A second source of incompatibility may be due to
`import`s. Specifically module import that assert `return` to be a
method of `Monad`, e.g.:

```
importControl.Monad(Monad((>>=),return))
```


or

```
importPreludehiding(Monad(..))importControl.Monad(Monad(..)) as Monadf=Monad.return ()
```


The dual situation can occur when re-exporting `return` via module
export specifications.


However, given that `return` is exported by `Prelude` and the examples
above are rather artificial, we don't expect this to be a major source
of breakage in the case of `return`. In fact, a heuristic grep\[5\] over
Hackage source-code revealed only 21 packages affected.

### Example for writing compatible code

```
instanceFunctorFoowhere
    fmap g foo  =…instanceApplicativeFoowhere
    pure x      =…
    a1 <*> a2   =…instanceMonadFoowhere
    m >>= f     =…#if!(MIN_VERSION_base(4,8,0))
    return = pure
#endif
```

## Migration Strategy


The migration strategy is straightforward:

**Phase 1***(GHC 8.0)*: Implement new warning in GHC which gets

>
> triggered when `Monad` instances explicitly override the
> default `return` method implementation.

**Phase 2***(GHC 8.2 or later)*: When we're confident that the

>
> majority of Hackage has reacted to the warning (with the help of
> Stackage actively pursuing maintainers to update their packages) we
> turn the `return` method into a top-level binding and remove the
> warning implemented in Phase 1 from GHC again.

## Discussion period


A discussion period of three weeks (until 2015-10-15) should be enough
to allow everyone to chime in as well as leave enough time to make the
required preparations for GHC 8.0 should this proposal pass as we hope.

---

- \[1\]: [ https://wiki.haskell.org/Functor-Applicative-Monad_Proposal](https://wiki.haskell.org/Functor-Applicative-Monad_Proposal)
- \[2\]: [ https://wiki.haskell.org/MonadFail_Proposal](https://wiki.haskell.org/MonadFail_Proposal)
- \[3\]: [ https://ghc.haskell.org/trac/ghc/wiki/ApplicativeDo](https://ghc.haskell.org/trac/ghc/wiki/ApplicativeDo)
- \[4\]: [ https://gist.github.com/hvr/b0e34463d85b58f169d9](https://gist.github.com/hvr/b0e34463d85b58f169d9)
- \[5\]: [ https://gist.github.com/hvr/afcd040783d980594883](https://gist.github.com/hvr/afcd040783d980594883)
- \[6\]: [ https://ghc.haskell.org/trac/ghc/ticket/9590](https://ghc.haskell.org/trac/ghc/ticket/9590)
- \[7\]: [ https://mail.haskell.org/pipermail/haskell-prime/2015-September/003936.html](https://mail.haskell.org/pipermail/haskell-prime/2015-September/003936.html)