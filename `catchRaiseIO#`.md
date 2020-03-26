David F argues for a variant of `catch#` that only catches precise exception, working title `catchRaiseIO#`.

# Discussion

Consider

```hs
getLine >>= print . read @Int
```

David: Catching precise exceptions means you'll catch problems reading from stdin or writing to stdout, but not exceptions thrown from another thread or errors thrown by read. But my real reason for wanting it is simply that I like the idea of pretending that the precise exception mechanism layers EitherT into the IO type. If we only have a catch-everything `catch#`, we don't get that.

SG: Then why not catch the precise exceptions you are interested in directly? That would be much more explicit.
The only scenario in which I would find this useful is when an exception is thrown both precisely and imprecisely, which arguably seems like bad design. Wanting to catch all precise exceptions implies that we don't know in advance which precise exceptions can be thrown from an action, and that the information that we catched a precise exception rather than an imprecise one somehow matters in that context.

# Semantics

I (David Feuer) believe that precise exceptions should implement the following model.

```
newtype IO a = IO {unIO :: State# RealWorld -> (# State# RealWorld, Either SomeException a #)
instance Monad IO where
  return a = IO $ \s -> (# s, Right a #)
  m >>= f = IO $ \s -> case unIO m s of
    (# s', Left e #) -> (# s', Left e #)
    (# s', Right a #) -> unIO (f a) s'

throwIO :: SomeException -> IO a
throwIO e = IO $ \s -> (# s, Left e #)

-- The name 'catchIO' is, sadly, taken by a less interesting function already.
-- This function will ultimately be implemented in terms of a 'catchRaiseIO#' primop.
catchThrowIO :: IO a -> (SomeException -> IO a) -> IO a
catchThrowIO m f = IO $ \s ->
  case unIO m s of
    (# s', Left e #) -> unIO (f e) s'
    good -> good
```

SG: How would imprecise exceptions play into this? What is the spec for `throw`/`catch`? Why do we reify precise but not imprecise exceptions? Imprecise exceptions would correspond to a layer of [`Validation`](http://hackage.haskell.org/package/validation-1.1/docs/Data-Validation.html), I guess.

# Implementation

The RTS must flag exceptions thrown by `raiseIO#` and catch only those by `catchRaiseIO#`.