# Replacing `touch#` with the `with#` combinator

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
    in IO $ \s -> with# r fio s

{-# NOINLINE with# #-}
with# :: a -> (State# RealWorld -> (# State# RealWorld, b #)) -> State# RealWorld -> (# State# RealWorld, b #)
with# a m s =
  case m s of
    (# s', r #) -> (# touch# a s', r #)

```



Fixing the issue properly with a new `with#` primop?
----------------------------------------------------

To fix the issue it has been proposed (#14375, !2566) to make the `with#`
function above a primop of this form:

```haskell
with# :: a
         -- ^ the value to preserve
      -> (State# s -> (# State s, r #))
         -- ^ the continuation in which the value will be preserved
      -> State# s -> (# State# s, r #)
```

`with# a k` evaluates `k`, ensuring that `a` remains alive throughout the evaluation. 

If we rewrite the `test` example above with `with#`, we get:

```haskell
test :: IO ()
test = do
    arr <- newPinnedByteArray 42
    with# arr (unIO (doSomething (byteArrayContents arr)))
```

This construction the compiler can't mangle as there is no continuation to drop.

Naive code generation of `with#`
-----------------------------------

The next question is what code should `with#` produce.

One simple strategy would be to lower `with#` as an out-of-line primop of the form:

```c
// the out-of-line entry code for with#:
with#_entry(closure, cont) {
    W_[Sp-0] = closure;
    W_[Sp-8] = with#_ret;
    Sp -= 16;
    ENTER(cont);
}

// the return code for the stack frame pushed by with#:
with#_ret() {
    Sp += 16;
}
```

However, this is suboptimal as it requires that the continuation be stack
allocated.

It turns out that several existing primops (e.g. `catch`,
`atomically`) follow a similar continuation-passing style and suffer from
this same suboptimal code generation. Consequently, there have been a few
proposals (largely orthgonal to the present `with#` proposal) on improving the
state of affairs here.

Improved code generation for continuation primops
-------------------------------------------------

Issue #16098 proposes an strategy (with an implementation in !2567) for
improving code generation for the above-mentioned continuation-passing primops.
Specifically, we extend STG to allow the continuation arguments of such primops
to non-ANF form. For instance, `with# x (\s -> expr)` would be a valid
application.

The STG-to-Cmm pass could then lower this application by emitting code to push
a catch stack-frame, then proceed immediately to emit the code for the
continuation.

A better improved code generation approach
------------------------------------------

While discussing the proposal in #16098 on IRC, @andreask raised the concern
that dropping ANF will inevitably complicate passes that work with STG.
He suggested this alternative:

In Core-to-STG we rather lower `with# x cont` as:
```haskell
case with#_prim x of () { _ ->
  let join j = cont
  in jump j
}
```
Where `with#_prim` is a primop known by the STG-to-Cmm pass which lowers to
pushing the same `with#_ret` stack frame seen above.

This gives us the same efficient code generation without losing the advantages
offered by ANF (albeit with the disadvantage of blatantly violating the STG abstraction).
