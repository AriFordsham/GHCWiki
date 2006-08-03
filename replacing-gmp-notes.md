# Replacing GMP: Bignum libraries, Licensing and Implementation

### Introduction


This task was started following [ Task \#601](http://hackage.haskell.org/trac/ghc/ticket/601), while these Notes were requested by [ Simon Peyton-Jones](http://www.haskell.org/pipermail/glasgow-haskell-users/2006-August/010676.html).


GHC currently implements the Integer and Fractional types by using the [ The GNU MP Bignum Library](http://swox.com/gmp/) (GMP) which supports arbitrary precision mathematical calculations.  GMP is fast, memory efficient, and offers many high level signed integer functions (140 of them), as well as many rational and floating point arithmetic functions.  The current GHC implementation only uses those functions necessary for the Prelude.  


GMP memory is integrated with the RunTime System's (RTS's) Garbage Collector (GC).  GMP memory is allocated from the GC heap, so values produced by GMP are under the control of the RTS and its GC.  The current implementation is memory efficient wile allowing the RTS and its GC to maintain control of GMP evaluations.

### Reasons for Replacing GMP as the Bignum library


There are several problems with the current GMP implementation:

1. Licensing

>
> GMP is licensed under the [ GNU Lesser General Public License](http://www.gnu.org/copyleft/lesser.html) (LGPL), a kind of "copyleft" license.  According to the terms of the LGPL, paragraph 5, you may distribute a program that is designed to be compiled and dynamically linked with the library under the terms of your choice (i.e., commercially) but if your program incorporates portions of the library, if it is linked statically, then your program is a "derivative"--a "work based on the library"--and according to paragraph 2, section c, you "must cause the whole of the work to be licensed" *under the terms of the LGPL* (including for free).  

>
> The LGPL licensing for GMP is a problem for the overall licensing of binary programs compiled with GHC because most distributions (and builds) of GHC use static libraries.  (Dynamic libraries are currently distributed only for OS X.)  The LGPL licensing situation may be worse: even though [ The Glasgow Haskell Compiler License](http://cvs.haskell.org/cgi-bin/cvsweb.cgi/fptools/ghc/LICENSE?rev=1.1.26.1;content-type=text%2Fplain) is essentially a "free software" license (BSD3), according to paragraph 2 of the LGPL, GHC must be distributed under the terms of the LGPL!

1. Memory Structure: Simultaneous Access to GMP by Foreign (C) code in the Same Binary

>
> In the current GMP implementation, GMP is configured to use GHC's GC memory, so C code in the same binary as GHC-compiled Haskell code cannot access GMP separately.  This problem was noted in [ bug Ticket \#311](http://hackage.haskell.org/trac/ghc/ticket/311).  Simon Peyton-Jones suggested that a simple renaming of GHC-GMP functions would solve this problem and Bulat Ziganshin suggested simply using an automated tool to do this.  See [ Replacement for GMP](http://www.haskell.org/pipermail/glasgow-haskell-users/2006-August/010679.html).

>
> The custom-memory configuration of GMP uses GMP's [ Custom Allocation](http://swox.com/gmp/manual/Custom-Allocation.html#Custom-Allocation) routines.  Alternative libraries may not have this facility built in.

1. Other Improvements to Integer

>
> Most of the suggestions in this section come from discussions in the glasgow-haskell-users list thread [ returning to Cost of Integer](http://www.haskell.org/pipermail/glasgow-haskell-users/2006-July/010654.html).  In particular, [ John Meacham's suggestion](http://www.haskell.org/pipermail/glasgow-haskell-users/2006-July/010660.html) to use a ForeignPtr to data held by the normal GMP system library and store the value in an unboxed Int if the number of significant digits in Integer could fit into the size of an Int.  For those who are curious, a guide to GHC primitives is available (in an unformatted version) in ghc/compiler/prelude/primops.txt.pp; here is a link to [ CVS version of primops.txt.pp](http://cvs.haskell.org/cgi-bin/cvsweb.cgi/fptools/ghc/compiler/prelude/primops.txt.pp?rev=1.37;content-type=text%2Fplain).  You might want to search for the text "section "The word size story."", and especially the text "section "Integer\#"". 

>
> The current GMP implementation of Integer is:
>
> ```wiki
> data Integer = Int# ByteArr#
> ```
>
>
> where the Int\# counts the number of [ limbs](http://swox.com/gmp/manual/Nomenclature-and-Types.html#Nomenclature-and-Types) (a GMP term referring to parts of a multi-precision number that fit into a 32 or 64 bit word, depending on the machine) and the ByteArr\# is the actual array in RTS-GC memory holding the limbs.  The sign of the Int\# is used to indicate the sign of the number represented by the ByteArr\#.  

>
> This current implementation of Integer means that all Integers are at least the size of an Int\# plus a ByteArr, even if there is only one limb (which would fit into an Int, as Int is also the representation of a machine word in current implementations).  The suggestion discussed by John Meacham, [ Lennart Augustsson](http://www.haskell.org/pipermail/glasgow-haskell-users/2006-August/010664.html), [ Simon Marlow](http://www.haskell.org/pipermail/glasgow-haskell-users/2006-August/010677.html) and [ Bulat Ziganshin](http://www.haskell.org/pipermail/glasgow-haskell-users/2006-August/010687.html) was to change the representation of Integer so the Int\# could be either a pointer to the Bignum library array of limbs or, if the number of significant digits could fit into say, 31 bits, to use the extra bit as an indicator of that fact and hold the entire value in the Int\#, thereby saving the memory from the ByteArr\# and increasing the speed with an unboxed Int\#.  

### Overview of the Current GMP Implementation


Esa Ilari Vuokko, who at one time attempted to replace GMP with [ LibTomMath](http://math.libtomcrypt.com/), posted several messages with good notes on the current implementation.  Much of what is on this page is derived from those notes.  See, [ Replacement for GMP(3)](http://www.haskell.org/pipermail/glasgow-haskell-users/2006-August/010669.html) and [ Replacement for GMP(4)](http://www.haskell.org/pipermail/glasgow-haskell-users/2006-August/010674.html).
