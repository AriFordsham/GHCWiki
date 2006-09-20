# Replacing GMP: Bignum libraries, Licensing and Implementation

### Introduction


This task was started following [ Task \#601](http://hackage.haskell.org/trac/ghc/ticket/601), while these Notes were requested by [ Simon Peyton-Jones](http://www.haskell.org/pipermail/glasgow-haskell-users/2006-August/010676.html).


GHC currently implements the Integer and Fractional types by using the [ The GNU MP Bignum Library](http://swox.com/gmp/) (GMP) which supports arbitrary precision mathematical calculations.  GMP is fast, memory efficient, and offers many high level signed integer functions (140 of them), as well as many rational and floating point arithmetic functions.  The current GHC implementation only uses those functions necessary for the Prelude.  


GMP memory is integrated with the RunTime System's (RTS's) Garbage Collector (GC).  GMP memory is allocated from the GC heap, so values produced by GMP are under the control of the RTS and its GC.  The current implementation is memory efficient wile allowing the RTS and its GC to maintain control of GMP evaluations.


If you want to help with replacing GMP or do it yourself, you will have to work with the GC and RTS system.  The parts you will have to modify are written in C and C--, with configuration and assembly done through the Makefiles.  You should have an understanding of:

- how the GC works and how memory from GMP is integrated with it;
- some C-- (this is fairly basic if you know C well, the only real documentation on C-- itself is in the [ C-- manual (PDF)](http://cminusminus.org/extern/man2.pdf), from cminusminus.org; the implementation of C-- for GHC is performed by several Haskell modules in the directory [ compiler/cmm](http://darcs.haskell.org/ghc/compiler/cmm/) of the HEAD branch); and,
- makefiles and configuration scripts.


Other basic recommended reading is:

- The GHC Commentary: [ The Native Code Generator](http://www.cse.unsw.edu.au/~chak/haskell/ghc/comm/the-beast/ncg.html); and,
- The GHC Commentary: [ Style Guidelines for RTS C code](http://www.cse.unsw.edu.au/~chak/haskell/ghc/comm/rts-libs/coding-style.html).

#### *Caveat*

>
> Beware!  The main interest here is replacing GMP--GHC is still belongs to the University of Glasgow and those in charge still retain the purview to accept or reject a proposed solution.

### Reasons for Replacing GMP as the Bignum library


There are several problems with the current GMP implementation:

1. Licensing

>
> GMP is licensed under the [ GNU Lesser General Public License](http://www.gnu.org/copyleft/lesser.html) (LGPL), a kind of "copyleft" license.  According to the terms of the LGPL, paragraph 5, you may distribute a program that is designed to be compiled and dynamically linked with the library under the terms of your choice (i.e., commercially) but if your program incorporates portions of the library, if it is linked statically, then your program is a "derivative"--a "work based on the library"--and according to paragraph 2, section c, you "must cause the whole of the work to be licensed" *under the terms of the LGPL* (including for free).  

>
> The LGPL licensing for GMP is a problem for the overall licensing of binary programs compiled with GHC because most distributions (and builds) of GHC use static libraries.  (Dynamic libraries are currently distributed only for OS X.)  The LGPL licensing situation may be worse: even though [ The Glasgow Haskell Compiler License](http://cvs.haskell.org/cgi-bin/cvsweb.cgi/fptools/ghc/LICENSE?rev=1.1.26.1;content-type=text%2Fplain) is essentially a "free software" license (BSD3), according to paragraph 2 of the LGPL, GHC must be distributed under the terms of the LGPL!

1. Memory Structure; Simultaneous Access to GMP by Foreign (C) code in the Same Binary

>
> In the current GMP implementation, GMP is configured to use GHC's GC memory and GMP can only have one allocator for memory.  Since any single binary containing Haskell code compiled with GHC contains the RTS and GMP, C code in the same binary cannot use GMP.  This problem was noted in [ bug Ticket \#311](http://hackage.haskell.org/trac/ghc/ticket/311).  The Simon Peyton-Jones suggested that a simple renaming of GHC-GMP functions would solve this problem and Bulat Ziganshin suggested simply using an automated tool to do this.  See [ Replacement for GMP](http://www.haskell.org/pipermail/glasgow-haskell-users/2006-August/010679.html).  Different function names would make GMP into a separate, custom GHC library leaving the C part of the program free to use GMP.

>
> GHC does not have a custom-modified version of GMP (in fact, GHC uses the system build of GMP if that is available).  The memory configuration of GMP uses GMP's [ Custom Allocation](http://swox.com/gmp/manual/Custom-Allocation.html#Custom-Allocation) routines.  Alternative libraries may not have this facility built in.

1. Other Improvements to Integer

>
> Most of the suggestions in this section come from discussions in the glasgow-haskell-users list thread [ returning to Cost of Integer](http://www.haskell.org/pipermail/glasgow-haskell-users/2006-July/010654.html).  In particular, [ John Meacham's suggestion](http://www.haskell.org/pipermail/glasgow-haskell-users/2006-July/010660.html) to use a ForeignPtr to data held by the normal GMP system library and store the value in an unboxed Int if the number of significant digits in Integer could fit into the size of an Int.  For those who are curious, a guide to GHC primitives is available (in an unformatted version) in ghc/compiler/prelude/primops.txt.pp; here is a link to [ CVS version of primops.txt.pp](http://darcs.haskell.org/ghc/compiler/prelude/primops.txt.pp).  (See The GHC Commentary [ Primitives](http://www.cse.unsw.edu.au/~chak/haskell/ghc/comm/rts-libs/primitives.html) for a description of primops.txt.pp; and a highly recommended introduction directly related to GMP is [AddingNewPrimitiveOperations](adding-new-primitive-operations).) In primops.txt.pp, you might want to search for the text "section "The word size story."", and especially the text "section "Integer\#"".   The Haskell definition of Integer is in [ /packages/base/GHC/Num.lhs](http://darcs.haskell.org/packages/base/GHC/Num.lhs).

>
> The current GMP implementation of Integer is:
>
> ```wiki
> data Integer	
>    = S# Int#              -- small integers
> #ifndef ILX
>    | J# Int# ByteArray#   -- large integers
> #else
>    | J# Void BigInteger   -- .NET big ints
> ```
>
>
> where the Int\# counts the number of [ limbs](http://swox.com/gmp/manual/Nomenclature-and-Types.html#Nomenclature-and-Types) (a GMP term referring to parts of a multi-precision number that fit into a 32 or 64 bit word, depending on the machine) and the ByteArr\# is the actual array in RTS-GC memory holding the limbs.  The sign of the Int\# is used to indicate the sign of the number represented by the ByteArr\#.  

>
> This current implementation of Integer means that there are two separate constructors for small and large Integers (S\# Int\# and J\# Int\# ByteArr\#).  The suggestion discussed by [ John Meacham](http://www.haskell.org/pipermail/glasgow-haskell-users/2006-August/010670.html), [ Lennart Augustsson](http://www.haskell.org/pipermail/glasgow-haskell-users/2006-August/010664.html), [ Simon Marlow](http://www.haskell.org/pipermail/glasgow-haskell-users/2006-August/010677.html) and [ Bulat Ziganshin](http://www.haskell.org/pipermail/glasgow-haskell-users/2006-August/010687.html) was to change the representation of Integer so the Int\# does the work of S\# and J\#: the Int\# could be either a pointer to the Bignum library array of limbs or, if the number of significant digits could fit into say, 31 bits, to use the extra bit as an indicator of that fact and hold the entire value in the Int\#, thereby saving the memory from S\# and J\#.  

> [ Bulat Ziganshin and John Meacham](http://www.haskell.org/pipermail/glasgow-haskell-users/2006-August/010688.html) noted a few problems with a 30bit Int: 

- interoperability between Haskell and other languages, especially C, would be more difficult so you would have to define a new primitive, say \#Int30 for the representation; and,
- representing a Haskell constructor (the Int\#) inside a pointer--a bit-size constructor--would limit the number of constructors you would be able to have (depending on the size of a pointer object, say the C99 uintptr_t, on a particular machine).

### Overview of the Current GMP Implementation


Esa Ilari Vuokko, who at one time attempted to replace GMP with [ LibTomMath](http://math.libtomcrypt.com/), posted several messages with good notes on the current implementation.  Much of what is on this page is derived from those notes.  See, [ Replacement for GMP(3)](http://www.haskell.org/pipermail/glasgow-haskell-users/2006-August/010669.html) and [ Replacement for GMP(4)](http://www.haskell.org/pipermail/glasgow-haskell-users/2006-August/010674.html).

#### Files related to GMP in the GHC Compiler Source Code


Note: references are relative to the main directory of the source distribution; links below are to the darcs repository at [ http://darcs.haskell.org/ghc](http://darcs.haskell.org/ghc).

- [ configure.ac](http://darcs.haskell.org/ghc/configure.ac) (*Modify*: remove GMP related material; replace with MP library requirements)

- [ compiler/prelude/primops.txt.pp](http://darcs.haskell.org/ghc/compiler/prelude/primops.txt.pp)   (*Modify*: Integer material)
- [ compiler/prelude/PrelNames.lhs](http://darcs.haskell.org/ghc/compiler/prelude/PrelNames.lhs) (*Reference*: integerTyConName and similar)
- [ compiler/prelude/TysPrim.lhs](http://darcs.haskell.org/ghc/compiler/prelude/TysPrim.lhs)   (*Reference*)

- [ includes/Cmm.h](http://darcs.haskell.org/ghc/includes/Cmm.h) (*Modify*: cpp test for `#if SIZEOF_mp_limb_t != SIZEOF_VOID_P `)
- [ includes/MachRegs.h](http://darcs.haskell.org/ghc/includes/MachRegs.h) (*Reference*: general; unrelated to GMP: may be starting point for vectorized Cmm (currently only -fvia-c allows auto-vectorization))
- [ includes/mkDerivedConstants.c](http://darcs.haskell.org/ghc/includes/mkDerivedConstants.c) (*Modify*: references to GMP `__mpz_struct`: `struct_size(MP_INT)`, `struct_field(MP_INT,_mp_alloc)`, `struct_field(MP_INT,_mp_size)`, `struct_field(MP_INT,_mp_d)` and `ctype(mp_limb_t)`.  Note: mp_limb_t generally == unsigned long)
- [ includes/Regs.h](http://darcs.haskell.org/ghc/includes/Regs.h) (*Modify*: references to MP_INT, `#include "gmp.h"`; Reference: Stg registers, etc.)
- [ includes/Rts.h](http://darcs.haskell.org/ghc/includes/Rts.h) (*Modify*: reference to `#include "gmp.h"`, `extern` declarations to `__decodeDouble` and `__decodeFloat`; References to various Stg types and macros)
- [ includes/StgMiscClosures.h](http://darcs.haskell.org/ghc/includes/StgMiscClosures.h) (*Modify*: references to `RTS_FUN(...Integer)` PrimOps; *Reference*: Weak Pointers, other Stg closures)

- [ rts/Linker.c](http://darcs.haskell.org/ghc/rts/Linker.c) (*Modify*: `SymX(__gmpn...)` and related GMP functions)
- [ rts/Makefile](http://darcs.haskell.org/ghc/rts/Makefile) (*Modify*: building GMP library)
- [ rts/PrimOps.cmm](http://darcs.haskell.org/ghc/rts/PrimOps.cmm) (*Modify*: remove GMP references; NOTE: optimisation of `/* ToDo: this is shockingly inefficient */`, see discussion below)
- [ rts/StgPrimFloat.c](http://darcs.haskell.org/ghc/rts/StgPrimFloat.c) (*Modify*: `__encodeDouble`, `__encodeFloat` and `decode` versions defined here refer to GMP; might optimise with bitwise conversion instead of union; conversion depends on whether replacement MP library uses floating point, etc.)
- [ rts/Storage.c](http://darcs.haskell.org/ghc/rts/Storage.c) (*Modify*: `stgAllocForGMP`, `stgReallocForGMP` and `stgDeallocForGMP`; may use as reference for implementation if replacement MP library uses GHC-garbage collected memory)
- [ rts/gmp (directory)](http://darcs.haskell.org/ghc/rts/gmp/) (*Modify*: recommended to remove entirely, i.e., do not add conditional compilation for users who want to keep on using GMP)

#### Optimisation Opportunities


(1) The "shockingly inefficient" operation of this code:

```wiki
/* ToDo: this is shockingly inefficient */

#ifndef THREADED_RTS
section "bss" {
  mp_tmp1:
    bits8 [SIZEOF_MP_INT];
}

section "bss" {
  mp_tmp2:
    bits8 [SIZEOF_MP_INT];
}

section "bss" {
  mp_result1:
    bits8 [SIZEOF_MP_INT];
}

section "bss" {
  mp_result2:
    bits8 [SIZEOF_MP_INT];
}
#endif
```


should be obvious.  There are at least two possible alternatives to this:

>
> (a) wrap the replacement MP-library array/structure for arbitrary precision integers in a closure so you do not have to rebuild the struct from on each MP-library call; or

>
> (b) use ForeignPtr (in Cmm, Weak Pointers--difficult to implement) to foreign threads holding the the struct/array

### Benchmarks for Multi-Precision Libraries


The benchmarks below were made with unmodified multi-precision libraries for Integral Arithmetic compiled using Apple gcc 4.0.1 with optimisation settings: -O3 -ftree-vectorize -falign-loops=16.  The tests performed Multiplication, Squaring, Powers (up to 7) and Division each 1,000,000 times at the base level of bit-precision (the number of bits in the operands).  Higher levels of precision performed incrementally fewer rounds: the base level (1,000,000 / (i \* 3)) where i is the number for the round, incremented from 0.  For example, at a bit-precision of 512 (second bit-precision test), the number of rounds was (1,000,000 / (1 \* 3)) = (1,000,000 / 3) = 333,333 rounds.  Multi-precision libraries may use unsigned chars, unsigned ints, unsigned long ints, unsigned long long ints or doubles, so the actual number of "words" in each multi-precision array may differ; for multi-precision real numbers using doubles, integer precision was calculated at 48.3 bits of real precision per double, rounded up to 49.  (49 bits conservatively equates to about 9 decimal digits of precision, see, e.g., [ What Every Computer Scientist Should Know about Floating-Point Arithmetic](http://docs.sun.com/source/806-3568/ncg_goldberg.html).)  Libraries tested were:

- [ ARPREC](http://crd.lbl.gov/~dhbailey/mpdist/)
- [ OpenSSL's BN](http://www.openssl.org/) (part of libcrypto)
- [ GMP](http://swox.com/gmp/)
- [ LibTomMath](http://math.libtomcrypt.com/)
- [ Crypto++](http://www.eskimo.com/~weidai/cryptlib.html) a cryptographic library in C++, the Integer class
- [ Botan](http://botan.randombit.net/) a cryptographic library in C++, 
- [ MPI](http://www.cs.dartmouth.edu/~sting/mpi/)
- [ MAPM](http://www.tc.umn.edu/~ringx004/mapm-main.html)


Crypto++, Botan, MPI and MAPM showed performance far below ARPREC, OpenSSL's BN, GMP and LibTomMath, so results are only shown for the last four.  Note that there are other libraries available for arbitrary precision arithmetic other than those mentioned or tested here.  Most of those other libraries are licensed under the GPL, while the remainder, such as the [ decNumber](http://www2.hursley.ibm.com/decimal/decnumber.html) library (free, under the [ ICU license](http://www-306.ibm.com/software/globalization/icu/license.jsp)) are designed and tuned for operations that would be difficult to translate into Haskell's Integer primitive.

not handled: Image

not handled: Image

not handled: Image

not handled: Image