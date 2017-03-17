# The semantics of precise exceptions


This page captures some thinking about the semantics of exceptions (early 2017)


See

- The main wiki page: [Exceptions](exceptions).
- The [FixingExceptions](fixing-exceptions) discussion.

## Precise and imprecise exceptions


David helpfully divides exceptions into

- **Imprecise**: just as described by [ A Semantics for Imprecise Exceptions](https://www.microsoft.com/en-us/research/publication/a-semantics-for-imprecise-exceptions/).  An imprecise exception can be raised anywhere, by `raise# :: Exception -> a`, or by failures like divide-by-zero.  (I'm going to ignore the structure of exception values, call them all `Exception`.)

>
> As discussed in the paper, an imprecise exception is a disaster scenario, not an alternative return.  We can catch them (in the IO monad) and try some alternative action.

>
> Imprecise exceptions should be considered to be a bug in the program.  They should not be used for control flow or for conditions that happen when things are working correctly.  Still, we might want to recover from an imprecise exception.

- **Precise**:  raised in the IO monad, by `throwIO :: Exception -> IO a`, or perhaps by an exception thrown by a foreign function call.

>
> Precise exceptions are a bit less of a disaster; e.g. they are used to report "file does not exist" on file-open.  (Is this a good thing? SimonM: one can debate whether I/O errors should be represented as explicit values, e.g. `Either DoesNotExist FileContents`, but in practice there are many different error conditions that can occur when doing I/O, many of which are rare, so it's convenient to not have to deal with them explicitly.)


Asynchronous exceptions are an orthogonal concept: the notion of precision doesn't apply to asynchronous exceptions which may occur at any time regardless of the expression being evaluated.

## The semantics of precise exceptions


One way to give a semantics to precise exceptions is by using an operational semantics, as described in [ Tackling the awkward squad](https://www.microsoft.com/en-us/research/publication/tackling-awkward-squad-monadic-inputoutput-concurrency-exceptions-foreign-language-calls-haskell/).  But GHC does not reflect that operational semantics internally; instead it expresses IO using state-token-passing.  That makes an awkward discontinuity when tryign to reason about GHC optimisations.


Another, suggested by David, is this: regard the IO monad as if it were implemented by an exception monad:

```wiki
type IO a = State# RealWorld -> (# State# RealWorld, Either Exception a #)
```


We won't *actually* do this, but we could *behave as if* we did.
In particular, `raiseIO#` does *not* return bottom; it behaves as if it was defined thus

```wiki
raiseIO# exn s = (# s, Left exn #)
```


There is more to say; see `catchThrowIO` in [FixingExceptions](fixing-exceptions).

### Precise exceptions and strictness


This view of precise exceptions gives us a principled way to answer questions about strictness and precise exceptions: just write it in the form above, and use the imprecise-exceptions paper.


For example, something like

```wiki
f x = throwIO exn >> x
```


would mean

```wiki
f1 x s = case raiseIO# exn s of
          (s', Left exn) -> (s', Left exn)
          (s', Right _)  -> x s'
```


which is obviously non-strict in `x`.
On the other hand

```wiki
f2 x = error "foo" >> x
```


would turn into

```wiki
f2 x = case raise# exn of
          (s', Left exn) -> (s', Left exn)
          (s', Right _)  -> x s'
```


and that *is* strict in `x` because `raise#`*does* return bottom, just as described in the imprecise-exceptions paper.

### Primops that cannot fail


Is this function strict in `x`?

```wiki
fRead :: IO a -> IO a
fRead x = readMutVar r >> x
```


We can make a choice here.  Reading a mutable varaible can't fail (in the IO monad).  So we could define the primop (i.e. make it behave) like this:

```wiki
readMutVar# :: MutVar# s a -> State# s -> (# State# s, a #)

readMutVar :: MutVar (State# RealWorld) a -> IO a
readMutVar (MV r) s = case readMutVar# r s of
                        (# s', v #) -> (# s', Right v #)
```


The primop can't fail; and the wrapper `readMutVar` expresses that explicitly by always returning `Right`.
Now `fRead x` woudl be strict in `x`.


Alternatively , we could define the primop like this

```wiki
readMutVar# :: MutVar# s a -> State# s -> (# State# s, Either Exception a #)
```


so that the primop looks as if it can fail; now `fRead` will be lazy.


Simon PJ strongly prefers the former, but it's a free choice.

### Implementing precise exceptions


Earlier I said that we want to make `IO a` "behave as if" it was implemented with a sum type.
We don't really want to add all those `Left/Right` pattern matches in Core; and the implementation does not have them either (because we walk the stack instead).   But, if we are going to take this implementation short-cut, we need to think carefully.

- The "behave as if" is the origin of the "IO hack" in the demand analyser `DmdAnal.hs`:

  ```wiki
  {- Note [IO hack in the demand analyser]
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  There's a hack here for I/O operations.  Consider

       case foo x s of { (# s', r #) -> y }

  Is this strict in 'y'? Often not! If foo x s performs some observable action
  (including raising an exception with raiseIO#, modifying a mutable variable, or
  even ending the program normally), then we must not force 'y' (which may fail
  to terminate) until we have performed foo x s.

  Hackish solution: spot the IO-like situation and add a virtual branch,
  as if we had
       case foo x s of
          (# s, r #) -> y
          other      -> return ()
  So the 'y' isn't necessarily going to be evaluated
  ```

  But at least we now understand it better.  It would be better if we had a more robust way to signal the need for it than just "unboxed tuple with a `State# RealWorld` token".   Maybe we need a special variant of unboxed pairs.

- The `can_fail` attribute of a primop could perhaps indicate whether the primop can fail, and hence its strictness behaviour.

- I am keen that `case throwIO# exn s of { ... -> BIG }` should be able to discard the `BIG` case alternative; and the same should be true of functions that wrap `throwIO#`.  But since `throwIO#` is no longer bottoming (in the imprecise sense), that won't happen without more work.  We want to say "`throwIO#` guarantees to return `Left`" and record that in its strictness signature, and that of its wrapping functions.

---

## Copying the state token


Quite separately from all this, there is another challenge with the state-passing implementation of the IO monad.  Look at `Note [Transformations affected by can_fail and has_side_effects]` in `PrimOp.hs`, especially

```wiki
* Duplication.  You cannot duplicate a has_side_effect primop.  You
  might wonder how this can occur given the state token threading, but
  just look at Control.Monad.ST.Lazy.Imp.strictToLazy!  We get
  something like this
        p = case readMutVar# s v of
              (# s', r #) -> (S# s', r)
        s' = case p of (s', r) -> s'
        r  = case p of (s', r) -> r

  (All these bindings are boxed.)  If we inline p at its two call
  sites, we get a catastrophe: because the read is performed once when
  s' is demanded, and once when 'r' is demanded, which may be much
  later.  Utterly wrong.  Trac #3207 is real example of this happening.
```


I (SLPJ) now think that it's utterly wrong to use the `has_side_effect` attribute for this purpose.


The real culprit is this:

- If we take an expression `(...s...)` where `s` is a state token, and duplicate it, then we have destroyed the linear behaviour of the state, and Many Bad Thing will happen.

*So it's nothing to do with primops.*  We should not inline `p` in the example above (except if it has a unique call site) because it has a free `s`.  And this is true not only of IO state tokens but *any* state token; and happily they all have type `State# t` for some `t`.


So I propose that we deal with this duplication issue by think about free state tokens, and treat that as a completely orthogonal issue to the question of primops and their properties.  Indeed I think that might mean we could get rid of `has_side_effects` on primops altogether.