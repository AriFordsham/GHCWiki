# Adding new primitive operations to GHC Haskell: An amateur explains


Quoting from "The Glasgow Haskell Copiler User's Guide, Version 6.2",
section 7.2, "GHC is built on a raft of primitive data types and
operations".
Here, I walk the reader through the steps of adding a particular
new primitive operation to GHC Haskell,
one that calls the GMP function `mpz_powm`.
This is for non-.NET platforms, since .NET uses
`BigInteger` rather than GMP.


I should warn the reader that I, in writing this piece, am a rank
amateur in many of the involved issues, including working with the
GHC compiler, git, and gitlab. Hence, I will often digress in various
directions, reflecting that this is a learning experience for me.

## Why do I want to add new primitive operations to GHC Haskell?


My basic interest is computing with rather large integers. This means
integers with hundreds or even thousands of decimal digits.
Haskell's `Integer` type fits this purpose
perfectly. However, I am ultimately concerned about performance and
some of the more complex algorithms for operating on large integers
require the use of operations, such as shifting, that cannot
be implemented efficiently in standard Haskell.


The `Integer` operations of GHC Haskell for non-.Net platforms use GMP.
And, actually, GMP includes some of the functions on large integers that I
am looking for. So, adding new primitive operations that call
these GMP functions solves my problem.

## Why do I not use FFI?


Quite simply because I have so far been unable to figure out how
to pass `Integer` operands efficiently between Haskell and C.
I am still, however, in the market for a solution of this nature.

## Why do I need `mpz_powm`?


The GMP `mpz_powm` function computes "(BASE raised to EXP) modulo MOD"
for some integers BASE, EXP, and MOD. So this is really not the sort
of low-level function (like shifting) that I am looking for. But it
provides a nice example for the present text. And also allows me to
compare my Haskell implementation of the same function with the GMP
implementation and thereby gain some general insight into the overhead
incurred when using Haskell rather than GMP called directly from C.

## Basics: Versions of GHC


The GHC that I am working on is ghc-6.4.1-src.tar.bz2 that I
downloaded 2006-March-01. I also found it valuable to consult the
version that I downloaded from the darcs respository on 2006-March-21.

## Steps required to add a new primop


The steps are actually described in "ghc/compiler/prelude/primops.txt.pp"
of the 2006-March-21 darcs version of GHC as follows: 

```wiki
-- To add a new primop, you currently need to update the following files:
--
--      - this file (ghc/compiler/prelude/primops.txt.pp), which includes
--        the type of the primop, and various other properties (its
--        strictness attributes, whether it is defined as a macro
--        or as out-of-line code, etc.)
--
--      - if the primop is inline (i.e. a macro), then:
--              ghc/compiler/AbsCUtils.lhs (dscCOpStmt)
--                defines the translation of the primop into simpler
--                abstract C operations.
--
--      - or, for an out-of-line primop:
--              ghc/includes/StgMiscClosures.h (just add the declaration)
--              ghc/rts/PrimOps.cmm     (define it here)
--              ghc/rts/Linker.c       (declare the symbol for GHCi)
--
--      - the User's Guide
```


The part about `ghc/compiler/AbsCUtils.lhs` is no longer valid, at least I
cannot figure out the details related to this. But is any case, the
present piece
is about adding `Integer` primitive operations and they are not inline
(as we will see in a minute), so I don't care right now.


The part about the User's Guide is also not accurate: In "The Glasgow
Haskell Copiler User's Guide, Version 6.2", section 7.2, I read that


>
>
> There used to be long section about \[the primitives\] here in the User
> Guide, but it became out of date and wrong information is worse than
> none.
>
>


So that's a relief.

## Digression: How to refer to things?


The basic mechanism is, of course, the hypertext link. Unfortunately, I cannot
seem to find a way of referring to things that won't easily break. It is
comforting to know that others appear to have the same problem: I noted the
other day in passing a letter from Simon Marlow (I believe it was) that
explained how he had now eliminated a level (ghc) of the directory tree. As
a result, several links in [DebuggingGhcCrashes](debugging-ghc-crashes) are now broken.


(And please forgive me here: Although an amateur in many respects, I am a
professional programmer and I have had years of experience dealing with
multiple versions of things, relations that shouldn't break, etc. So I
know that the solution is not easy. But I need some guidance in this case:
Should I just quietly go and correct these few links that are wrong?
The problem is that I know I could find dozens, if not hundreds, of broken
links, so I could spend the rest of my life doing this sort of thing.)

## `ghc/compiler/prelude/primops.txt.pp` changes


This file "should first be preprocessed" and is then "processed by the
utility program genprimopcode to produce a number of include files
within the compiler"


In this file we have, for example, the following:

```wiki
  primop   IntegerQuotOp   "quotInteger#" GenPrimOp
     Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#, ByteArr# #)
     {Rounds towards zero.}
     with out_of_line = True
```


which is the basis of the `quot::Integer->Integer->Integer` function.
A Haskell `Integer` "is represented as a pair consisting of an `Int#`
representing the number of 'limbs' in use and the sign, and a
`ByteArr#` containing the 'limbs' themselves."


Mimicking `IntegerQuotOp`, we add:

```wiki
  primop   IntegerPowerModOp   "powerModInteger#" GenPrimOp
     Int# -> ByteArr# -> Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#, ByteArr# #)
     {Base raised to a power modulo a modulus.}
     with out_of_line = True
```


for our power operation: It needs three `Integer` operands and returns one
`Integer` result, hence the type has an `Int#` and a `ByteArr#`
for each.

## `ghc/includes/StgMiscClosures.h` changes


In this file, we should just "add the declaration". Hence, we add the line

```wiki
   RTS_FUN(powerModIntegerzh_fast);
```


somewhere suitable in the (alphabetically sorted) list of other symbols.
The name used here (`powerModIntegerzh_fast`) is what is called
the Z-encoding of the primitive operation `powerModInteger#`.
A nice description of Z-encoding can be found in [DebuggingGhcCrashes](debugging-ghc-crashes).

## `ghc/rts/PrimOps.cmm` changes


This is where the code that actually implements (out of line)
primitive operations go. In this file we find, for example:

```wiki
  #define GMP_TAKE2_RET1(name,mp_fun)                     \
  name                                                    \
  {                                                       \
    CInt s1, s2;                                          \
    W_ d1, d2;                                            \
                                                          \
    /* call doYouWantToGC() */                            \
    MAYBE_GC(R2_PTR & R4_PTR, name);                      \
                                                          \
    s1 = W_TO_INT(R1);                                    \
    d1 = R2;                                              \
    s2 = W_TO_INT(R3);                                    \
    d2 = R4;                                              \
                                                          \
    MP_INT__mp_alloc(mp_tmp1) = W_TO_INT(StgArrWords_words(d1));  \
    MP_INT__mp_size(mp_tmp1)  = (s1);                     \
    MP_INT__mp_d(mp_tmp1)     = BYTE_ARR_CTS(d1);         \
    MP_INT__mp_alloc(mp_tmp2) = W_TO_INT(StgArrWords_words(d2));  \
    MP_INT__mp_size(mp_tmp2)  = (s2);                     \
    MP_INT__mp_d(mp_tmp2)     = BYTE_ARR_CTS(d2);         \
                                                          \
    foreign "C" mpz_init(result1);                        \
                                                          \
    /* Perform the operation */                           \
    foreign "C" mp_fun(result1,mp_tmp1,mp_tmp2);          \
                                                          \
    RET_NP(TO_W_(MP_INT__mp_size(result1)),               \
           MP_INT__mp_d(result1) - SIZEOF_StgArrWords);   \
  }
```


defining a macro which is called, for example, like this:

```wiki
  GMP_TAKE2_RET1(quotIntegerzh_fast,     mpz_tdiv_q)
```


to provide an implementation of the `quotInteger#` primitive
operation. So we go ahead and define

```wiki
  #define GMP_TAKE3_RET1(name,mp_fun)                             \
  name                                                            \
  {                                                               \
    CInt s1, s2, s3;                                              \
    W_ d1, d2, d3;                                                \
                                                                  \
    /* call doYouWantToGC() */                                    \
    MAYBE_GC(R2_PTR & R4_PTR & R6_PTR, name);                     \
                                                                  \
    s1 = W_TO_INT(R1);                                            \
    d1 = R2;                                                      \
    s2 = W_TO_INT(R3);                                            \
    d2 = R4;                                                      \
    s3 = W_TO_INT(R5);                                            \
    d3 = R6;                                                      \
                                                                  \
    MP_INT__mp_alloc(mp_tmp1)     = W_TO_INT(StgArrWords_words(d1));      \
    MP_INT__mp_size(mp_tmp1)      = (s1);                         \
    MP_INT__mp_d(mp_tmp1)         = BYTE_ARR_CTS(d1);             \
    MP_INT__mp_alloc(mp_tmp2)     = W_TO_INT(StgArrWords_words(d2));      \
    MP_INT__mp_size(mp_tmp2)      = (s2);                         \
    MP_INT__mp_d(mp_tmp2)         = BYTE_ARR_CTS(d2);             \
    MP_INT__mp_alloc(mp_tmp3)     = W_TO_INT(StgArrWords_words(d3));      \
    MP_INT__mp_size(mp_tmp3)      = (s3);                         \
    MP_INT__mp_d(mp_tmp3)         = BYTE_ARR_CTS(d3);             \
                                                                  \
    foreign "C" mpz_init(result1);                                \
                                                                  \
    /* Perform the operation */                                   \
    foreign "C" mp_fun(result1,mp_tmp1,mp_tmp2,mp_tmp3);          \
                                                                  \
    RET_NP(TO_W_(MP_INT__mp_size(result1)),                       \
           MP_INT__mp_d(result1) - SIZEOF_StgArrWords);           \
  }
```


and a call

```wiki
  GMP_TAKE3_RET1(powerModIntegerzh_fast, mpz_powm)
```


(Actually, defining a macro for this single call may seem needless,
but it is not particularly more troublesome, and leaves some reduced
work ahead.)


We have also added

```wiki
  section "bss" {
    mp_tmp3:
      bits8 [SIZEOF_MP_INT];
  }
```


to cater for the (new) situation of having to deal with 3 `Integer`
operands.


(Caring for efficiency, I am somewhat dismayed by the remark

```wiki
  /* ToDo: this is shockingly inefficient */
```


describing the PrimOps.cmm implementation of `Integer` primitive operations.
But that will have to be dealt with later.)

## `ghc/rts/Linker.c` changes


In this file, we should just "declare the symbol for GHCi".
Adding

```wiki
        SymX(powerModIntegerzh_fast)            \
```


takes care of that.

## `libraries/base/GHC/Num.lhs` changes


The internal `Integer` representation is defined here as:

```wiki
  data Integer
     = S# Int#                            -- small integers
  #ifndef ILX
     | J# Int# ByteArray#                 -- large integers
  #else
     | J# Void BigInteger                 -- .NET big ints

  foreign type dotnet "BigInteger" BigInteger
  #endif
```


that is, for the non-.NET situation envisioned here, either a single
precision integer or a GMP integer represented by a limb count and a limb
pointer. The `Integer` functions defined in this file provide an interface
between the primitive operations defined in terms of this internal `Integer`
representation and the externally visible `Integer` operations in GHC.Num
(like `quotInteger`). So, similar to

```wiki
  quotInteger :: Integer -> Integer -> Integer
  quotInteger ia 0
    = error "Prelude.Integral.quot{Integer}: divide by 0"
  quotInteger a@(S# (-LEFTMOST_BIT#)) b = quotInteger (toBig a) b
  quotInteger (S# a) (S# b) = S# (quotInt# a b)
  {- Special case disabled, see remInteger above
  quotInteger (S# a) (J# sb b)
    | sb ==# 1#  = S# (quotInt# a (word2Int# (integer2Word# sb b)))
    | sb ==# -1# = S# (quotInt# a (0# -# (word2Int# (integer2Word# sb b))))
    | otherwise  = zeroInteger
  -}
  quotInteger ia@(S# _) ib@(J# _ _) = quotInteger (toBig ia) ib
  quotInteger (J# sa a) (S# b)
    = case int2Integer# b of { (# sb, b #) ->
      case quotInteger# sa a sb b of (# sq, q #) -> J# sq q }
  quotInteger (J# sa a) (J# sb b)
    = case quotInteger# sa a sb b of (# sg, g #) -> J# sg g
```


we define

```wiki
  powerModInteger :: Integer -> Integer -> Integer -> Integer
  powerModInteger a@(S# _) b c = powerModInteger (toBig a) b c
  powerModInteger a b@(S# _) c = powerModInteger a (toBig b) c
  powerModInteger a b c@(S# _) = powerModInteger a b (toBig c)
  powerModInteger (J# sa a) (J# sb b) (J# sc c)
    = case powerModInteger# sa a sb b sc c of (# sr, r #) -> ( J# sr r )
```


to gain access to our power modulo operation.

## "Make" the changes

**First: make clean! ** GHC doesn't correctly track changes to the `GHC.Prim`, so it won't recompile enough things if you just type `make`, so you need to `make clean` first.  (strictly speaking you don't need to clean the stage 1 compiler, so if you know what you're doing you might want to try cleaning just the right bits).


Then we `make all` in the top-level directory. The error message that
we get is

```wiki
GHC/Num.lhs:507:9: Not in scope: `powerModInteger#'
```


So dependencies are missing. I have not bothered to track down
why this happens in detail. But, guided by wise persons, I simply say
`make clean` in both `ghc/compiler` and `libraries/base`
and that helps: The compiler builds and actually works, including the
newly implemented primitive operation `powerModInteger`.
