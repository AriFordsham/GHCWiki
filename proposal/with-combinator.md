# Replacing `touch#` with the `keepAlive#` combinator

There are a collection of related tickets;

* #17760 is the one about `keepAlive#`
* !2961 (a first attempt) has useful commentary
* !3131 is a second attempt, dependent on !3113 (runRW)
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
keepAlive# :: forall (r :: RuntimeRep) (a :: TYPE r) b.
         b
         -- ^ the value to preserve
      -> (State# s -> (# State s, a #))
         -- ^ the continuation in which the value will be preserved
      -> State# s -> (# State# s, a #)
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


Avoiding allocations with `keepAlive#`
--------------------------------------

It turns out that `touch#` is used quite widely, especially as part of `withForeignPtr`. Moreover, many of these uses are extremely computationally light. For instance, consider `GHC.IO.Buffer.readWord8Buf`:

```haskell
readWord8Buf :: RawBuffer Word8 -> Int -> IO Word8
readWord8Buf arr ix = withForeignPtr arr $ \p -> peekByteOff p ix
```

This is small enough to be inlined and will therefore be typically compiled to
a single memory read instruction when used in a strict context. While it is
trivial to implement `withForeignPtr` in terms of `keepAlive#`, it is very
important that we do not add overhead to this sort of usage.

Unfortunately, the naive implementation of `withForeignPtr` in terms of
`keepAlive#` will prevent GHC from eliminating the allocation of an `W8#` in a
strict usage of `readWord8Buf`. For instance, consider a program like this:

```haskell
strlen :: RawBuffer Word8 -> IO Int
strlen = go 0
  where
    go !n !buf = do
      c <- readWord8Buf buf n
      if c == 0 then return n else go (n+1) buf
```

With today's `withForeignPtr` (implemented in terms of the fragile `touch#`) we
get a very tight loop for `go`'s worker with no allocations in the hot part of
the loop. However, with an implementation based upon `keepAlive#` we will end
up with something like the following:

```haskell
$wgo :: Int# -> ForeignPtr Word8 -> IO Int
$wgo n# buf = IO $ \s0 -> 
    case keepAlive# buf (\s1 -> 
           case readWord8OffAddr# buf 0# of { Unit# n# -> W8# n# }
         ) of
     W8# n# ->
       case n# of
         0# -> ...
         _  -> ...
```

That is, while previously case-of-known constructor was able to eliminate the
allocation of the `W8#` intermediate, with `keepAlive#` we can no longer do so.
To fix this we need to somehow push the outer `case` (scrutinizing the `Word8`)
into the continuation of `keepAlive#`. We recently implemented precisely this
optimisation for `runRW#` in #15127. We will need to do similarly for
`keepAlive#`. We call this optimisation *strict-context float-in*.

Note that for this optimisation to be possible `keepAlive#` *must* be
polymorphic in the levity of the result.


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

One way to eliminate the cost added by `keepAlive#` is to rewrite it to the
existing `touch#` primitive (which has no runtime cost) late enough in the
compilation process that the simplifier can't drop it (avoiding #14375). For
instance, we might give `keepAlive#` a similar treatment to that of `runRW#`,
  which is lowered in `CorePrep` to an application of `realWorld#`.

Under such a scheme, `CorePrep` would look for applications of the form `keepAlive# x k s` and rewrite them to,
```haskell
case k s of (# s', y #) ->
case touch# x s' of s'' ->
(# s'', y #)
```
With out current STG-to-STG pipeline the `touch#` here should have
the desired effect of keeping `x` alive until `k` has finished.


However, there is a problem here: we have introduced a levity-polymorphic
binder, `y`. This is disallowed by Core. That being said, operationally
everything is fine as `touch#` is a no-op during code generation.

Also we'd want
```
K[ keepAlive x (\s.e) ]]  -->   keepAlive x (\x. K[[e]])
```
rather like `runST`.


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

Producing this lowering in STG-to-Cmm is a bit tricky since we work exclusively
with abstract stack areas. We essentially need to treat the `keepAlive#` frame as
an update frame, pushing it above everything else in the `Old` stack region. However
it appears that this may break invariants within the code generator.


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
Finally, the STG-to-C-- pass will have special logic for lowering case analyses
on `pushKeepAlive#`, essentially unfolding its C-- definition into the
use-site. Such a case analysis will result in C-- like:
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
offered by ANF (albeit with the disadvantage of weakening the separation of
concerns provided by the STG abstraction).

Option E: tag Core case-expression with kept-alive variables
------------------------------------------------------------

Suppose we have this Haskell primitive:

```haskell
keepAlive# :: a -> b -> b
```

And that it gets desugared into the following Core:

```haskell
keepAlive# (Var k) b ===> case {k} b of b' -> { DEFAULT -> b' }
keepAlive# a       b ===> compile time error
```

Where `{k}` in `case {k} scrut of ...` is a set of variable **names** to keep alive while evaluating `scrut`. Let's call it the keep-alive set.

**The semantics is clear: variables in the keep-alive set must be kept alive until the scrutinee is evaluated.**

Compilation is straightforward:
- STG: keep-alive sets are carried into STG's case-expressions exactly like in Core
- Cmm: keep-alive sets are desugared into `touch#` pseudo-instructions placed just after case scrutinee evaluation code
- CmmToAsm: we drop `touch#` pseudo-ops as we currently do

Advantages of this approach:

1. most optimizations (case-of-case, case-of-known constructor, w/w...) can
   work as before. BUT we need to tweak them a little to take keep-alive sets
   into account (see below)
2. we don't have to deal with a continuation as would be the case if we kept
   `keepAlive#` as a magic function in Core
3. performance is as good as it is today (no stack frame, most optimizations
   still work, cf (1) 
4. most changes to the compiler are located into `GHC.Core.Opt.*`.

A drawback of this approach is that Core transformations have to be reviewed to
check that they are still valid when we take keep-alive sets into account. BUT
we would probably have to do this anyway for other approaches because we really
really want to perform some Core transformations over `keepAlive#`.

Let's review some analyzes and transformations:

1. freevars: `freeVars(case {k} scrut of alts) = k + freeVars(case scrut of alts)`

2. case-of-case

```haskell
case {k} (case {k2} x of .. -> a; ... -> b) of
  C0 .. -> e1
  C1 .. -> e2

===>

case {k,k2} x of
   ... -> case {k} a of { C0 .. -> e1; C1 ... -> e2 }
   ... -> case {k} b of { C0 .. -> e1; C1 ... -> e2 }
```

3. case-of-known-constructor (or anything in WHNF)

```haskell
case {k} whnf of ... ===> case whnf of ...
```

4. Note that we can't discard case-expressions with non-empty keep-alive sets. E.g.

```haskell
case {k} x of e { DEFAULT -> alt }  ====/===> let e = x in alt
```

Option F: `noDiv`
-----------------

Another way to avoid #17760 (and implement the `keepAlive` interface described
above) is to introduce a primitive to hide the fact that an expression may
diverge from the simplifier.

Specifically, we introduce a magic operation:

```haskell
noDiv :: forall (r :: RuntimeRep) (a :: TYPE r).
         (State# RealWorld -> a) -> State# RealWorld -> a
noDiv k s0 =
    case {- forgetting divergence -} k s0 of r -> r
```

Semantically this is an identity. However, it carries the caveat that
`noDiv f s` will evaluate the result of `f s` ignoring the fact that it may diverge.
The `State` token here ensures that no effects can be floated outside of the `noDiv` scope.

This primitive allows one to implement `keepAlive#` as follows:
```haskell
keepAlive# :: forall a r.
              a
           -> (State# RealWorld -> (# State# RealWorld, r #))
           -> State# RealWorld
           -> (# State# RealWorld, r #)
keepAlive# x f s0 =
  case noDiv f s0 of
    (# s1, r #) ->
      case touch# x s1 of
        s2 -> (# s2, r #)
{-# INLINE keepAlive #-}
```
Note the `INLINE` pragma here; this is perfectly safe since we rely entirely on `noDiv`
for the safety of this operation.

However, there is a wrinkle: the simplification described in the "Avoiding allocations with
`keepAlive#`" section above poses a problem here. Let's again consider the case
of `strlen`'s worker written in terms of `keepAlive#`:

```haskell
$wgo :: Int# -> ForeignPtr Word8 -> IO Int
$wgo n# buf = IO $ \s0 -> 
    case keepAlive# buf (\s1 -> 
           case readWord8OffAddr# buf 0# of { Unit# n# -> W8# n# }
         ) of
     W8# n# ->
       case n# of
         0# -> ...
         _  -> ...
```

Inlining `keepAlive` we get:

```haskell
$wgo :: Int# -> ForeignPtr Word8 -> IO Int
$wgo n# buf = IO $ \s0 -> 
    case
      (case
         noDiv s0 (\s1 -> case readWord8OffAddr# buf 0# of { Unit# n# -> W8# n# })
       of
         (# s1, r #) -> case touch# x s1 of s2 -> (# s2, r #)
      )
    of
      W8# n# ->
        case n# of
          0# -> ...
          _  -> ...
```

Here we see a problem: the strict-context float-in transform will push the context enclosing the `noDiv`,

```haskell
case 
  _
of
  (# s1, r #) -> case touch# x s1 of s2 -> (# s2, r #)
```

*into* the `noDiv`. Consequently the `touch#` will end up *inside* the `noDiv`.
At this point the demand analyser may conclude that the `touch#` is unreachable
and we end up back in the situation we are trying to avoid.


Option G: `noDivFinal`
----------------------

Here we try to fix up the issues of Option F by moving the "final" continuation
(e.g. the `touch#`) into a separate argument of `noDiv`. We call this operation
`noDivFinal`:
```haskell
noDivFinal :: forall (r :: RuntimeRep) (a :: TYPE r).
              (State# RealWorld -> State# RealWorld)
           -> (State# RealWorld -> (# State# RealWorld, a #))
           -> State# RealWorld
           -> (# State# RealWorld, a #)
noDivFinal k final s0 =
    case {- forgetting divergence -} k s0 of (# s1, r #) ->
    case final s1 of s2 ->
    (# s2, r #)
```

The definition here will be inlined during CorePrep.

Errata: Skip everything below. This doesn't work for the same reason that
Option B doesn't work: it requires a levity polymorphic binder, `r`.

With this we can implement `keepAlive#` as:
```haskell
keepAlive# :: forall a r.
              a
           -> (State# RealWorld -> (# State# RealWorld, r #))
           -> State# RealWorld
           -> (# State# RealWorld, r #)
keepAlive# x f s0 =
    noDivFinal (touch# x) f s0
```

Here we can freely float strict-contexts into the `first `


