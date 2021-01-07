Unique supply implementation
============================

The unique supply implementation has been reworked in 6c771aafa30e261f6822b3ddddbe66f8a55f307c. The following notes are about the previous implementation:

```haskell
{- Note [How the unique supply works]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The basic idea (due to Lennart Augustsson) is that a UniqSupply is
lazily-evaluated infinite tree.

* At each MkSplitUniqSupply node is a unique Int, and two
  sub-trees (see data UniqSupply)

* takeUniqFromSupply :: UniqSupply -> (Unique, UniqSupply)
  returns the unique Int and one of the sub-trees

* splitUniqSupply :: UniqSupply -> (UniqSupply, UniqSupply)
  returns the two sub-trees

* When you poke on one of the thunks, it does a foreign call
  to get a fresh Int from a thread-safe counter, and returns
  a fresh MkSplitUniqSupply node.  This has to be as efficient
  as possible: it should allocate only
     * The fresh node
     * A thunk for each sub-tree

Note [Optimising the unique supply]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The inner loop of mkSplitUniqSupply is a function closure

     mk_supply :: IO UniqSupply
     mk_supply = unsafeInterleaveIO $
                 genSym      >>= \ u ->
                 mk_supply   >>= \ s1 ->
                 mk_supply   >>= \ s2 ->
                 return (MkSplitUniqSupply (mask .|. u) s1 s2)

It's a classic example of an IO action that is captured
and the called repeatedly (see #18238 for some discussion).
It turns out that we can get something like

  $wmkSplitUniqSupply c# s
    = letrec
        mk_supply
          = \s -> unsafeDupableInterleaveIO1
                    (\s2 -> case noDuplicate# s2 of s3 ->
                            ...
                            case mk_supply s4 of (# s5, t1 #) ->
                            ...
                            (# s6, MkSplitUniqSupply ... #)
      in mk_supply s

This is bad because we allocate that inner (\s2...) every time.
Why doesn't full laziness float out the (\s2...)?  Because of
the state hack (#18238).

So for this module we switch the state hack off -- it's an example
of when it makes things worse rather than better.  And we use
multiShotIO (see Note [multiShotIO]) thus:

     mk_supply = multiShotIO $
                 unsafeInterleaveIO $
                 genSym      >>= \ u ->
                 ...

Now full laziness can float that lambda out, and we get

  $wmkSplitUniqSupply c# s
    = letrec
        lvl = \s2 -> case noDuplicate# s2 of s3 ->
                     ...
                     case unsafeDupableInterleaveIO
                              lvl s4 of (# s5, t1 #) ->
                     ...
                     (# s6, MkSplitUniqSupply ... #)
      in unsafeDupableInterleaveIO1 lvl s

This is all terribly delicate.  It just so happened that before I
fixed #18078, and even with the state-hack still enabled, we were
getting this:

  $wmkSplitUniqSupply c# s
    = letrec
        mk_supply = \s2 -> case noDuplicate# s2 of s3 ->
                           ...
                           case mks_help s3 of (# s5,t1 #) ->
                           ...
                           (# s6, MkSplitUniqSupply ... #)
        mks_help = unsafeDupableInterleaveIO mk_supply
           -- mks_help marked as loop breaker
      in mks_help s

The fact that we didn't need full laziness was somewhat fortuitious.
We got the right number of allocations. But the partial application of
the arity-2 unsafeDupableInterleaveIO in mks_help makes it quite a
bit slower.  (Test perf/should_run/UniqLoop had a 20% perf change.)

Sigh.  The test perf/should_run/UniqLoop keeps track of this loop.
Watch it carefully.

Note [multiShotIO]
~~~~~~~~~~~~~~~~~~
The function multiShotIO :: IO a -> IO a
says that the argument IO action may be invoked repeatedly (is
multi-shot), and so there should be a multi-shot lambda around it.
It's quite easy to define, in any module with `-fno-state-hack`:
    multiShotIO :: IO a -> IO a
    {-# INLINE multiShotIO #-}
    multiShotIO (IO m) = IO (\s -> inline m s)

Because of -fno-state-hack, that '\s' will be multi-shot. Now,
ignoring the casts from IO:
    multiShotIO (\ss{one-shot}. blah)
    ==> let m = \ss{one-shot}. blah
        in \s. inline m s
    ==> \s. (\ss{one-shot}.blah) s
    ==> \s. blah[s/ss]

The magic `inline` function does two things
* It prevents eta reduction.  If we wrote just
      multiShotIO (IO m) = IO (\s -> m s)
  the lamda would eta-reduce to 'm' and all would be lost.

* It helps ensure that 'm' really does inline.

Note that 'inline' evaporates in phase 0.  See Note [inlineId magic]
in GHC.Core.Opt.ConstantFold.match_inline.

The INLINE pragma on multiShotIO is very important, else the
'inline' call will evaporate when compiling the module that
defines 'multiShotIO', before it is ever exported.
-}

-- | Unique Supply
--
-- A value of type 'UniqSupply' is unique, and it can
-- supply /one/ distinct 'Unique'.  Also, from the supply, one can
-- also manufacture an arbitrary number of further 'UniqueSupply' values,
-- which will be distinct from the first and from all others.
data UniqSupply
  = MkSplitUniqSupply {-# UNPACK #-} !Int -- make the Unique with this
                   UniqSupply UniqSupply
                                -- when split => these two supplies

mkSplitUniqSupply :: Char -> IO UniqSupply
-- ^ Create a unique supply out of thin air. The character given must
-- be distinct from those of all calls to this function in the compiler
-- for the values generated to be truly unique.

-- See Note [How the unique supply works]
-- See Note [Optimising the unique supply]
mkSplitUniqSupply c
  = mk_supply
  where
     !mask = ord c `shiftL` uNIQUE_BITS

        -- Here comes THE MAGIC: see Note [How the unique supply works]
        -- This is one of the most hammered bits in the whole compiler
        -- See Note [Optimising the unique supply]
        -- NB: Use unsafeInterleaveIO for thread-safety.
     mk_supply = multiShotIO $
                 unsafeInterleaveIO $
                 genSym      >>= \ u ->
                 mk_supply   >>= \ s1 ->
                 mk_supply   >>= \ s2 ->
                 return (MkSplitUniqSupply (mask .|. u) s1 s2)

multiShotIO :: IO a -> IO a
{-# INLINE multiShotIO #-}
-- See Note [multiShotIO]
multiShotIO (IO m) = IO (\s -> inline m s)


```