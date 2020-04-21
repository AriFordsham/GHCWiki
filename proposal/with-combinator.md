# Replacing `touch#` with the `keepAlive#` combinator

There are a collection of related tickets;

* #17760 is the one about `keepAlive#`
* !2961 (a first attempt) has useful commentary
* #17746, #18061: `withForeignPtr` is unsafe
* #18040 is a bit similar: swapping unsafe operations with `touch#`
* #13104 is about `runRW#` and join points
* #15127 is similar, also `runRW#`


Background
------------
Today GHC offers `touch#` to ensure object liveness in the presence of
references living outside of the heap:

```haskell
touch# :: a -> State# RealWorld -> State# RealWorld
```
This is often useful when passing GC-managed objects (typically pinned byte
arrays) to foreign functions through foreign data structures. For instance,
the POSIX vectored I/O interface exposes this interface:

```c
struct iovec {
    void  *iov_base;    /* Starting address */
    size_t iov_len;     /* Number of bytes to transfer */
};

ssize_t readv(int fd, struct iovec iovs[iov_count], int iov_count);
```
We may, for instance, wish to populate `iov_base` with a pinned `ByteArray`:

```haskell
data IoVector = IoVector { iov_base :: Addr, iov_len :: CSize }
instance Storable IoVector

foreign import ccall "readv" c_readv
    :: CInt -> Ptr IoVector -> CInt -> IO CSize

doRead :: Fd -> [IoVector] -> IO CSize
doRead fd vecs = withArray vecs $ \vecsPtr -> readv fd vecsPtr (length vecs)

read :: Fd    -- ^ file descriptor from which to read
     -> CSize -- ^ length to read
     -> IO ByteArray
read fd len = do
  arr <- newPinnedByteArray 42
  result <- doRead fd [IoVector (byteArrayContents arr) len]
  touch# arr
```

However, this is dangerously susceptible to being dropped by the simplifier.
For instance, if `doRead` were change such that the simplifier concluded that
it will fail to return (e.g.  because it is of the form `forever ...`), then it
is tempted to drop the continuation containing `touch#`. This results in the
garbage collector inappropriately freeing `arr`, resulting in catastrophe.

It caused #14346 (`allocaBytes` and `allocaBytesAligned`) and #17746 (`withForeignPtr`).

Mitigation
----------

A way to mitigate the issue is to ensure that functions using `touch#` can't be simplified by using ``NOINLINE`` pragmas.

* #14346 has been fixed this way in 8.2 and 8.6 by adding a NOINLINE pragma to `allocaBytes[Aligned]` functions (cf 56590db07a776ce81eb89d4a4d86bd0f953fb44e).

* #17746: adding a NOINLINE pragma to `withForeignPtr` fixes the issue but the price in performance is very high. An alternative is to rewrite as follows so that only the second field of the `ForeignPtr` is allocated and kept alive (instead of the whole `ForeignPtr`). Sadly it still has a huge impact on performance metrics (`withForeignPtr` is used a lot, especially in `ByteString` implementation).

```haskell
withForeignPtr fo io
  = let !(ForeignPtr addr r) = fo
        IO fio               = io (Ptr addr)
    in IO $ \s -> keepAlive# r fio s

{-# NOINLINE keepAlive# #-}
keepAlive# :: a -> (State# RealWorld -> (# State# RealWorld, b #)) -> State# RealWorld -> (# State# RealWorld, b #)
keepAlive# a m s =
  case m s of
    (# s', r #) -> (# touch# a s', r #)

```



Fixing the issue properly with a new `keepAlive#` primop?
----------------------------------------------------

To fix the issue it has been proposed (#14375, !2566) to make the `keepAlive#`
function above a primop of this form:

```haskell
keepAlive# :: forall (r :: RuntimeRep) (a :: TYPE r).
         a
         -- ^ the value to preserve
      -> (State# s -> (# State s, r #))
         -- ^ the continuation in which the value will be preserved
      -> State# s -> (# State# s, r #)
```

`keepAlive# a k` evaluates `k a`, ensuring that `a` remains alive throughout the evaluation.

If we rewrite the `test` example above with `keepAlive#`, we get:

```haskell
test :: IO ()
test = do
    arr <- newPinnedByteArray 42
    keepAlive# arr (unIO (doSomething (byteArrayContents arr)))
```

This construction the compiler can't mangle as there is no continuation to drop.

Option A: Naive code generation of `keepAlive#`
------------------------------------------

The next question is what code should `keepAlive#` produce.

One simple strategy would be to lower `keepAlive#` as an out-of-line primop of the form:

```c
// the out-of-line entry code for keepAlive#:
keepAlive#_entry(closure, cont) {
    W_[Sp-0] = closure;
    W_[Sp-8] = keepAlive#_ret;
    Sp -= 16;
    ENTER(cont);
}

// the return code for the stack frame pushed by keepAlive#:
keepAlive#_ret() {
    Sp += 16;
}
```

However, this is suboptimal as it requires that the continuation be stack
allocated.

Option B: A slightly better code generation strategy
----------------------------------------------------

One way to eliminate the cost added by `keepAlive#` is to rewrite it to the existing `touch#` primitive (which has no runtime cost) late enough in the compilation process that the simplifier can't drop it (avoiding #14375). For instance, we might give `keepAlive#` a similar treatment to that of `runRW#`, which is lowered in `CorePrep` to an application of `realWorld#`.

Under such a scheme, `CorePrep` would look for applications of the form `keepAlive# x k s` and rewrite them to,
```haskell
case k s of (# s', y #) ->
case touch# x s' of s'' ->
(# s'', y #)
```
With out current STG-to-STG pipeline the `touch#` here should have
the desired effect of keeping `x` alive until `k` has finished.

Option C: Improved code generation for continuation primops
-----------------------------------------------------------

It turns out that several existing primops (e.g. `catch#`,
`atomically#`) have a continuation-passing structure similar to that of `keepAlive#` and suffer from
this same suboptimal code generation. Consequently, there have been a few
proposals (largely orthgonal to the present `keepAlive#` proposal) on improving the
state of code generation for these operations.

Issue #16098 proposes an strategy (with an implementation in !2567) for
improving code generation for the above-mentioned continuation-passing primops.
Specifically, we extend STG to allow the continuation arguments of such primops
to non-ANF form. For instance, `keepAlive# x (\s -> expr)` would be a valid
application.

The STG-to-Cmm pass could then lower this application by emitting code to push
a catch stack-frame, then proceed immediately to emit the code for the
continuation.

Option D: A better improved code generation approach
----------------------------------------------------

While discussing the proposal in #16098 on IRC, @andreask raised the concern
that dropping ANF will inevitably complicate passes that work with STG.
He suggested this alternative:

We introduce a magic primop, used only in STG:
```haskell
pushKeepAlive# :: forall (r :: RuntimeRep) (a :: TYPE r).
             a -> State# s -> State# s
```

In Core-to-STG we rather lower `keepAlive# x cont` as:
```haskell
case pushKeepAlive# x of () { _ ->
  let join j = cont
  in jump j
}
```
Finally, the STG-to-C-- pass will have special logic for lowering case analyses on `pushKeepAlive#`, essentially unfolding its C-- definition into the use-site. Such a case analysis will result in C-- like:
```c
// Push a pushWith_ret stack frame carrying a reference to `x`
W_[Sp] = stg_pushWith_ret;
W_[Sp+WDS(1)] = R1;

// Jump to the continuation `cont`
jump j;
```
The return code for the `stg_pushWith_ret` frame would be defined in the RTS thusly:
```c
stg_pushWith_ret() {
    Sp += WDS(2);  // Pop the pushWith frame
    return;        // Return to the next frame
}
```
pushing the same `keepAlive#_ret` stack frame seen above.

This gives us the same efficient code generation without losing the advantages
offered by ANF (albeit with the disadvantage of weakening the separation of concerns provided by the STG abstraction).

Option E: tag Core case-expression with kept-alive variables
------------------------------------------------------------

Suppose we have this Haskell primitive:

```haskell
keepAlive# :: a -> b -> b
```

And that it gets desugared into the following Core:

```haskell
keepAlive# (Var k) b ===> case {k} b of b' -> { DEFAULT -> b' }
keepAlive# a       b ===> case a of k -> { DEFAULT -> case {k} b of b' -> { DEFAULT -> b' } }
```

Where `{k}` in `case {k} scrut of ...` is a set of variable names to keep
alive while evaluating `scrut`. Let's call it the keep-alive set.

Most Core optimizations work as before: we just have to take into account that
variables in keep-alive sets are free variables of case-expressions.

We carry keep-alive sets into STG's case-expressions.

In Cmm we can have a `void keepAlive#(StgClosure * a)` pseudo-instruction. In
`GHC.StgToCmm.Expr.cgCase`, we generate `keepAlive#` instructions just after the
evaluation of the scrutinee.

In `GHC.CmmToAsm`, we just have to drop every Cmm `keepAlive#` instruction: the
code-generator would have ensured that its argument is alive down to this
instruction and that's exactly what we want.

### Transformations


Most transformations are unchanged (we will need to check them when we will add
the keep-alive field). We need to take the keep-alive set into account (e.g. to
avoid floating in a binding bounding one of the keep-alive variable).

One change is that we can't discard case-expressions with non empty keep-alive
sets. E.g.

```haskell
-- x is in WHNF, has no side effects, etc. (cf GHC.Core.Opt.Simplify.rebuildCase)
case {k} x of e { DEFAULT -> alt }  ====/===> let e = x in alt
```

Instead we provide a new `kept-alive-whnf-single-case` transformation:

```haskell
-- x is in WHNF, has no side-effects, etc. and is not void#
case {k} x of e { DEFAULT -> alt }  ==> case {k} void# of _ { DEFAULT -> let e = x in alt}
```


### Improving `runRW#`

runRW# allows us to conjure a `State# s` token out of thin air (e.g. to
implement `unsafePerformIO`). The problem is that we drop the final state:

```haskell

unsafePerformIO :: IO a -> a
unsafePerformIO m = unsafeDupablePerformIO (noDuplicate >> m)

unsafeDupablePerformIO  :: IO a -> a
unsafeDupablePerformIO (IO m) = case runRW# m of (# _, a #) -> a
```

So if we inlined `runRW#` too early, we would discard some effects:

```haskell
foo5 = unsafeDupablePerformIO (print "Foo" >> return 5)
==> case runRW# (print "Foo" >> return 5) of (# s, a #) -> a
==> case runRW# (\s -> case print' "Foo" s of s' -> (# s', 5 #)) of (# s, a #) -> a

-- inlining runRW#
==> case (case print' "Foo" RealWorld of s' -> (# s', 5 #)) of (# s, a #) -> a

-- case-of-case
==> case print' "Foo" RealWorld of s' -> case (# s', 5 #) of (# s, a #) -> a

-- case-of-WHNF-thing
==> case print' "Foo" RealWorld of _ -> 5

-- dead code elimination
==> 5 -- BAD: we don't print anymore
```

So currently we don't do it. Hence codes using `runRW#` lack many optimizations.
See https://gitlab.haskell.org/ghc/ghc/issues/15127

But now with `keepAlive#` we could make the final state alive and directly
expose a Core Case-expression amenable to optimizations:

```haskell
-- New definition with keepAlive#
unsafeDupablePerformIO  :: IO a -> a
unsafeDupablePerformIO (IO m) = case runRW# m of (# s, a #) -> keepAlive# s a

foo5 = unsafeDupablePerformIO (print "Foo" >> return 5)
==> case runRW# (print "Foo" >> return 5) of (# s, a #) -> a
==> case runRW# (\s -> case print' "Foo" s of s' -> (# s', 5 #)) of (# s, a #) -> keepAlive# s a

-- inlining runRW#
==> case (case print' "Foo" RealWorld of s' -> (# s', 5 #)) of (# s, a #) -> keepAlive# s a

-- case-of-case
==> case print' "Foo" RealWorld of s' -> case (# s', 5 #) of (# s, a #) -> keepAlive# s a

-- case-of-known-constructor
==> case print' "Foo" RealWorld of s -> keepAlive# s 5

-- by definition of keepAlive# in Core
==> case print' "Foo" RealWorld of s -> case {s} 5 of b -> b

-- kept-alive-whnf-single-case
==> case print' "Foo" RealWorld of s -> case {s} void# of _ { DEFAULT -> 5 }
```

Now suppose we would case on the result of a `runRW#` application (cf #15217):

```haskell
case foo5 of I# i -> I# (efficientStuff i)
```

With the late inlining of `runRW#` we couldn't optimize this code. But now we
can:

```haskell
case foo5 of I# i -> I# (efficientStuff i)

==> case (case print' "Foo" RealWorld of s -> case {s} void# of _ { DEFAULT -> 5}) of I# i -> I# (efficientStuff i)

-- case-of-case
==> case print' "Foo" RealWorld of s -> case (case {s} void# of _ { DEFAULT -> 5}) of I# i -> I# (efficientStuff i)

-- case-of-case
==> case print' "Foo" RealWorld of s -> case {s} void# of _ { DEFAULT -> case 5 of I# i -> I# (efficientStuff i) }

-- case-of-known-constructor
==> case print' "Foo" RealWorld of s -> case {s} void# of _ { DEFAULT -> I# (efficientStuff 5#) }
```


### "Using" primops

In #4096 a `use#` primop very similar to our `keepAlive#` primop is suggested.
The issue was: "I don't know how to implement this, though, because use# would
have to be able to return arbitrary (unboxed) types and the code generator
doesn't really seem to support this.".

With our desugaring of `keepAlive#` into Core Case-expressions, we don't have
this issue as the primop never reaches the code generator.

### `withForeignPtr`

`withForeignPtr` currently provides an unsafe interface (#17746):

```haskell
withForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
withForeignPtr fo io
  = do r <- io (unsafeForeignPtrToPtr fo)
       touchForeignPtr fo
       return r
```

The problem is that if "io" is detected to never return (e.g. infinite
"forever" loop exited with an exception), `touchForeignPtr` is discarded and the
GC is free to collect "fo" while its internal pointer is used by `io`.

Suppose we rewrite it as follow:

```haskell
withForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
withForeignPtr fo io = IO $ \s -> keepAlive# fo (unIO (io (unsafeForeignPtrToPtr fo)) s)
```

It would get desugared into:

```haskell
withForeignPtr fo io
= IO $ \s -> keepAlive# fo (unIO (io (unsafeForeignPtrToPtr fo)) s)

==> IO $ \s -> case {fo} unIO (io (unsafeForeignPtrToPtr fo)) s of r { DEFAULT -> r}

-- float-out
==> let p      = unsafeForeignPtrToPtr fo
        IO act = io p
    in IO $ \s -> case {fo} act s of r { DEFAULT -> r}

-- `act s` can't escape the `case {fo}` if it performs side-effects
```

So even if we used forever on it, it wouldn't collect the foreign pointer too
early:

```haskell
forever :: IO a -> IO b
forever a = let a' = a `thenIO` a' in a'

thenIO :: IO a -> IO b -> IO b
thenIO (IO m) k = IO (\ s -> case m s of (# new_s, _ #) -> unIO k new_s)

forever (withForeignPtr fo io)

-- inline & float-out
==> let p      = unsafeForeignPtrToPtr fo
        IO act = io p
        a'     = IO $ \s -> case (case {fo} act s of r { DEFAULT -> r}) of (# new_s, _ #) -> unIO a' new_s
    in a'

-- case-of-case
==> let p      = unsafeForeignPtrToPtr fo
        IO act = io p
        a'     = IO $ \s -> case {fo} act s of r { DEFAULT -> case r of (# new_s, _ #) -> unIO a' new_s }
    in a'

-- simplify
==> let p      = unsafeForeignPtrToPtr fo
        IO act = io p
        a'     = IO $ \s -> case {fo} act s of (# new_s, _ #) -> unIO a' new_s
    in a'
```
