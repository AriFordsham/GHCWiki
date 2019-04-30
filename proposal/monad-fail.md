[[_TOC_]]

# `MonadFail` proposal (MFP)

A couple of years ago, we proposed to make `Applicative` a superclass of `Monad` which successfully killed the single most ugly thing in Haskell as of GHC 7.10.

Now, it's time to tackle the other major issue with `Monad` `fail` being a part of it.

For further information, feel free to ask the people responsible for or involved in this proposal:

- David Luposchainsky aka @quchen, dluposchainsky at the email service of Google
- Franz Thoma aka @fmthoma
- Herbert Valerio Riedel aka @hvr

## Overview

* **The problem** - reason for the proposal
* **MonadFail class** - the solution
* **Discussion** - explaining our design choices
* **Adapting old code** - how to prepare current code to transition smoothly
* **Estimating the breakage** - how much stuff we will break
* **Transitional strategy** - how to break as little as possible while transitioning
* **Current status**

## The problem

Currently, the `<-` symbol is unconditionally desugared as follows:

```hs
do pat <- computation     >>>     let f pat = more
   more                   >>>         f _ = fail "..."
                          >>>     in  computation >>= f
```

The problem with this is that `fail` cannot (!) be sensibly implemented for many monads, for example `Either`, `State`, `IO`, and `Reader`. In those cases it defaults to `error` As a consequence, in current Haskell, you can not use `Monad` polymorphic code safely, because although it claims to work for all `Monad` , it might just crash on you. This kind of implicit non-totality baked into the class is ''terrible''.

The goal of this proposal is adding the `fail` only when necessary and reflecting that in the type signature of the `do` block, so that it can be used safely, and more importantly, is guaranteed not to be used if the type signature does not say so.

## `MonadFail` class

To fix this, introduce a new typeclass:

```hs
class Monad m => MonadFail m where
    fail :: String -> m a
```

Desugaring can now be changed to produce this constraint when necessary. For this, we have to decide when a pattern match can not fail; if this is the case, we can omit inserting the `fail` call.

The most trivial examples of unfailable patterns are of course those that match anywhere unconditionally,

```hs
do x <- action     >>>     let f x = more
   more            >>>     in  action >>= f
```

In particular, the programmer can assert any pattern be unfailable by making it irrefutable using a prefix tilde:

```haskell
do ~pat <- action     >>>     let f ~pat = more
   more               >>>     in  action >>= f
```

A class of patterns that are conditionally failable are `newtype` , and single constructor `data` types, which are unfailable by themselves, but may fail if matching on their fields is done with failable patterns.

```haskell
data Newtype a = Newtype a

-- "x" cannot fail
do Newtype x <- action            >>>     let f (Newtype x) = more
   more                           >>>     in  action >>= f

-- "Just x" can fail
do Newtype (Just x) <- action     >>>     let f (Newtype (Just x)) = more
   more                           >>>         f _ = fail "..."
                                  >>>     in  action >>= f
```

`ViewPatterns` are as failable as the pattern the view is matched against. Patterns like `(Just -> Just x)` should generate a `MonadFail` constraint even when it's "obvious" from the view's implementation that the pattern will always match. From an implementor's perspective, this means that only types (and their constructors) have to be looked at, not arbitrary values (like functions), which is impossible to do statically in general.

```haskell
do (view ->  pat) <- action     >>>     let f (view ->  pat) = more
   more                         >>>         f _ = fail "..."
                                >>>     in  action >>= f

do (view -> ~pat) <- action     >>>     let f (view -> ~pat) = more
   more                         >>>     in  action >>= f
```

A similar issue arises for `PatternSynonyms` which we cannot inspect during compilation sufficiently. A pattern synonym will therefore always be considered failable.

```haskell
do PatternSynonym x <- action     >>>     let f PatternSynonym x = more
   more                           >>>         f _ = fail "..."
                                  >>>     in  action >>= f
```

[Edward Kmett: We have the contents of the pattern synonym available to us at the definition site. With some work we should be able to expose it enough that the compiler can see through it:

```haskell
pattern Foo a b = Bar a 0 b
pattern Baz a b c <- Quux a b c
```

Both of those tell us the "real" desugaring as just another pattern we could recurse into.]

## Discussion

* What laws should `fail` follow?
  * **Left zero**: `∀ s f. fail s >>= f ≡ fail s`.
  * **Right zero**: `∀ v s. v >> fail s ≡ fail s`.
* What is the relationship to `MonadPlus`?
  * As the laws above indicate, `fail` is a close relative of `mzero`. We could suggest a default definition of `fail _ = mzero`, which shows the intended usage and effect of the `MonadFail` class.
  * However, we should not remove `fail` and use only `mzero` instead.
    * Not all types with `Monad` instances have `MonadPlus` instances.
    * Some types do use the `String` argument to `fail`. For example, a parser might fail with a message involving positional information. `Binary` uses `fail` as their only interface to fail a decoding step.
    * Some types have different definitions for `mzero` and `fail`. Although `STM` is `MonadPlus` it uses the default `fail = error`. It should therefore not get a `MonadFail` instance.
* Rename `fail`?
  * **No.** Old code might use `fail` explicitly and we should avoid breaking it. The Report talks about `fail` and we have a solid migration strategy that does not require a renaming.
* Remove the `String` argument?
  * **No.** The `String` might help error reporting and debugging. `String` may be ugly, but it's the de facto standard for simple text in GHC. No high performance string operations are to be expected with `fail` so this breaking change would in no way be justified. Also note that explicit `fail` calls would break if we removed the argument.
* How sensitive would existing code be to subtle changes in the strictness behaviour of `do` notation pattern matching?
  * **It doesn't.** The implementation does not affect strictness at all, only the desugaring step. Care must be taken when fixing warnings by making patterns irrefutable using `~` as that ''does'' affect strictness. (Cf. difference between lazy/strict State)
* Do we need a class constraint (e.g. `Monad`) on `MonadFail`?
  * **Yes.** The intended use of `fail` is for desugaring `do`-notation, not generally for any `String -> m a` function. Given that goal, we would rather keep the constraints simple as `MonadFail m =>` rather than the somewhat redundant `(Monad m, MonadFail m) =>`.
* Can we relax the class constraint from `Monad` to `Applicative`?
  * We don't necessarily have to choose now. Since `Applicative` is a superclass of `Monad`, it is possible to change the superclass for `MonadFail` to `Applicative` later. This will naturally require a migration period, and the name will, of course, become misleading.
  * For the sake of discussion, let's use the following definition:`class Applicative f => ApplicativeFail f where fail :: String -> f a`
  * **Pros**
    * `ApplicativeDo` is coming, and `fail` may be useful to combine pattern matching and `Applicative` code.
    * If the `Monad` constraint is kept, that would force `Applicative` code with pattern matching to be `Monad` code.
  * **Cons**
    * The constraints for `Monad` code using `fail` become `(Monad m, ApplicativeFail m) =>` instead of the simpler `MonadFail m =>`. If we expect the common use of `fail` to be in `Monad` — not `Applicative` — `do`-notation, this leaves us with more verbose constraints.
  * Here are alternative definitions (with names open to debate) that would allow us to keep the constraints simple:
    * `class Applicative f => ApplicativeFail f where failA :: String -> f a`
    * `class ApplicativeFail m => MonadFail m where fail :: String -> m a; fail = failA`
    * Since we do not have much experience using `ApplicativeDo`, it is not yet clear that this large of a change is useful.
* Which types with `Monad` instances will not have `MonadFail` instances?
  * `base`: `Either`
  * `transformers`:
  * `stm`: `STM`
* What `MonadFail` instances will be created?
  * `base`: `IO`
  * `transformers`:
    * Proposal for an `Either` instance using `Monad` instance in `Control.Monad.Trans.Error`:```instance MonadFail (Either String) where fail = Left```

## Adapting old code

* Help! My code is broken because of a missing `MonadFail` instance!

  Here are your options:
  1. Write a `MonadFail` instance (and bring it into scope). The [fail package](https://hackage.haskell.org/package/fail) provides a forward-compatible `MonadFail` class for GHC versions prior to GHC 8.0
     ```haskell
     import Control.Monad
     -- Control.Monad.Fail import will become redundant in GHC 8.8+
     import qualified Control.Monad.Fail as Fail

     instance Monad Foo where
       (>>=) = <...bind impl...>
       -- NB: `return` defaults to `pure` since GHC 7.10

     #if !(MIN_VERSION_base(4,13,0))
       -- Monad(fail) will be removed in GHC 8.8+
       fail = Fail.fail
     #endif

     instance Fail.MonadFail Foo where
       fail = <...fail implementation...>
     ```
  2. Change your pattern to be irrefutable
  3. Emulate the old behaviour by desugaring the pattern match by hand:
     ```haskell
     do Left e <- foobar
        stuff
     ```
     becomes
     ```haskell
     do x <- foobar
        e <- case x of
           Left e' -> e'
           Right r -> error "Pattern match failed" -- Boooo
        stuff
     ```
     The point is you'll have to do your dirty laundry yourself now if you have a value that ''you'' know will always match, and if you don't handle the other patterns you'll get incompleteness warnings, and the compiler won't silently eat those for you.
* Help! My code is broken because you removed `fail` from `Monad` but my class defines it!

  At the very least, you'll need to remove the `fail` implementation from your `Monad` instance on GHC 8.8 or later. If you wish to support older versions of GHC as well, consider guarding your `fail` implementation behind CPP, as shown in the `Monad Foo` example of above. Also consider migrating your old `Monad.fail` implementation to a new `MonadFail` instance for others' benefit.

## Esimating the breakage

Using our initial implementation, I compiled stackage-nightly, and grepped the logs for the warnings. Assuming my implementation is correct, the number of "missing `MonadFail` warnings generated is 487. Note that I filtered out `[]` `Maybe` and `ReadPrec` since those can be given a `MonadFail` instance from within GHC, and no breakage is expected from them.
The build logs can be found [https://www.dropbox.com/s/knz0i979skam4zs/stackage-build.tar.xz?dl=0 here]. Search for "failable pattern" to find your way to the still pretty raw warnings.

Here are some commands you might find interesting for exploring the logs:

```bash
# List all packages generating warnings (57 of them)
grep "is used in the context" ''     | \
    grep -v '(‘\[|Maybe|ReadPrec)'   | \
    perl -pe 's#^(.'')\.log.''$#\1#' | \
    uniq -u

# Histogram of the breaking contexts (mostly IO and parsers)
grep "is used in the context" ''                     | \
    grep -v '(‘\[|Maybe|ReadPrec)'                   | \
    perl -pe 's#^.''in the context ‘([^ ]+).''$#\1#' | \
    sort                                             | \
    uniq -c                                          | \
    sort -rg
```

## Transitional strategy

The roadmap is similar to the [https://github.com/quchen/articles/blob/master/applicative_monad.md AMP], the main difference being that since `MonadFail` does not exist yet, we have to introduce new functionality and then switch to it.

* GHC 8.0 / base-4.9
  * Add module `Control.Monad.Fail` with new class `MonadFail(fail)` so people can start writing instances for it. `Control.Monad` only re-exports the class `MonadFail` but not its `fail` method. NB: At this point, `Control.Monad.Fail.fail` clashes with `Prelude.fail` and `Control.Monad.fail`.
  * Add a language extension `-XMonadFailDesugaring` that changes desugaring to use `MonadFail(fail)` instead of `Monad(fail)` This has the effect that typechecking will infer a `MonadFail` constraint for `do` blocks with failable patterns, just as it is planned to do when the entire thing is done.
  * Add a warning when a `do` block that contains a failable pattern is desugared, but there is no `MonadFail` instance in scope: "Please add the instance or change your pattern matching." Add a flag to control whether this warning appears, but leave it off by default.
  * Add a warning when an instance implements the `fail` function (or when `fail` is imported as a method of `Monad` , as it will be removed from the `Monad` class in the future. (See also [https://ghc.haskell.org/trac/ghc/ticket/10071 GHC #10071]). Leave it off by default.
* GHC 8.2
  * ''(nothing happens)''
* GHC 8.4
  * ''(maybe)'' Turn on the warning about missing `MonadFail` instances that we added in 8.0 by default.
* GHC 8.6
  * Switch `-XMonadFailDesugaring` on by default.
  * Warnings are still issued if the desugaring extension has been explicitly disabled.
  * ''(maybe)'' Turn on the warning about explicit definition of `fail` in Monad that we added in 8.0 by default.
* GHC 8.8
  * Remove `-XMonadFail` leaving its effects on at all times.
  * Remove `fail` from `Monad`
  * Instead, re-export `Control.Monad.Fail.fail` as `Prelude.fail` and `Control.Monad.fail`
  * `Control.Monad.Fail` is now a redundant module that can be considered deprecated.

## Current status

* [https://wiki.haskell.org/ZuriHac2015 ZuriHac 2015 (29.5. - 31.5.)]: Franz Thoma (@fmthoma) and me (David Luposchainsky aka @quchen) started implementing the MFP in GHC.
  * Desugaring to the new `fail` can be controlled via a new language extension, `MonadFailDesugaring`
  * If the language extension is turned off, a warning will be emitted for code that would break if it was enabled.
  * Warnings are emitted for types that ''have'' a ''MonadFail'' instance. This still needs to be fixed.
  * The error messages are readable, but should be more so. We're still on this.
* 2015-06-09: Estimated breakage by compiling Stackage. Smaller than expected.
* 2015-06-09 (late): Published. People seem to like the idea, with a couple of pain points remaining.
* 2015-06-16: [https://github.com/quchen/articles/blob/master/monad_fail_update1.md Update 1 posted.]
* 2015-09-18: [https://phabricator.haskell.org/D1248 Patch nearly finished. Some nontrivial tests still fail.]
* 2015-11-17: [https://github.com/ghc/ghc/commit/233d1312bf15940fca5feca6884f965e7944b555 MFP phase 1 merged.]