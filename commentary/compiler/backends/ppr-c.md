# GHC Commentary: The C code generator


Source: [compiler/cmm/PprC.hs](/trac/ghc/browser/ghc/compiler/cmm/PprC.hs)


This phase takes [Cmm](commentary/compiler/cmm-type) and generates plain C code.  The C code generator is very simple these days, in fact it can almost be considered pretty-printing.


There are some slight subtleties:

- [info tables](commentary/rts/heap-objects#info-tables), which are expressed in Cmm as being laid out before the entry code for a
  closure, are compiled into separate top-level structures in the generated C, because C has no support for laying out data
  next to functions.  The desired layout is reconstructed in the assembly file by the [Evil Mangler](commentary/evil-mangler),
  or not if we're compiling unregisterised (see [TABLES_NEXT_TO_CODE](commentary/rts/heap-objects#)).

## Header files


GHC was changed (from version 6.10) so that the C backend no longer uses header files specified by the user in any way.  The `c-includes` field of a `.cabal` file is ignored, as is the `-#include` flag on the command-line.  There were several reasons for making this change:


This has several advantages:
  

- `-fvia-C` is consistent with `-fasm` with respect to FFI declarations:

> >
> > both bind to the ABI, not the API.

- foreign calls can now be inlined freely across module boundaries, since
  a header file is not required when compiling the call.

- bootstrapping via C will be more reliable, because this difference
  in behavour between the two backends has been removed.

>
> There is one disadvantage:

- we get no checking by the C compiler that the FFI declaration
  is correct.

## Prototypes