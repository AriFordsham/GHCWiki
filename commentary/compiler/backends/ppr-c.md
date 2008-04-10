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
  both bind to the ABI, not the API.

- foreign calls can now be inlined freely across module boundaries, since
  a header file is not required when compiling the call.

- bootstrapping via C will be more reliable, because this difference
  in behavour between the two backends has been removed.


  
There are some disadvantages:
  

- we get no checking by the C compiler that the FFI declaration
  is correct.

- we can't benefit from inline definitions in header files.

## Prototypes


When a label is referenced by an expression, the compiler needs to
know whether to declare the label first, and if so, at what type.

- all labels referenced as a result of an FFI declaration
  are declared as `extern StgWord[]`, including funciton labels.
  If the label is called, it is first cast to the correct
  function type.  This is because the same label might be
  referred to both as a function and an untyped data label in
  the same module (e.g. Foreign.Marsal.Alloc refers to "free"
  this way).

- all RTS symbols already have declarations (mostly with the correct
  type) in [includes/StgMiscClosures.h](/trac/ghc/browser/ghc/includes/StgMiscClosures.h), so no declarations are generated.

- certain labels are known to have been defined earlier in the same file,
  so a declaration can be omitted (e.g. SRT labels)

- certain math functions (`sin()`, `cos()` etc.) are already declared because
  we \#include math.h, so we don't emit declarations for these.  We need
  to \#include math.h because some of these fuctions have inline
  definitions, and we get terrible code otherwise.


When compiling the RTS cmm code, we have almost no information about
labels referenced in the code.  The only information we have is
whether the label is defined in the RTS or in another package: a label
that is declared with an import statement in the .cmm file is assumed
to be defined in another package (this is for dynamic linking, where
we need to emit special code to reference these labels).


For all other labels referenced by RTS .cmm code, we assume they are
RTS labels, and hence already declared in [includes/StgMiscClosures.h](/trac/ghc/browser/ghc/includes/StgMiscClosures.h).  This is
the only choice here: since we don't know the type of the label (info,
entry etc.), we can't generate a correct declaration.
