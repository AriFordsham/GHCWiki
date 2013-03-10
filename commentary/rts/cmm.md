# GHC Commentary: What the hell is a `.cmm` file?


A `.cmm` file is rather like C--.  The syntax is almost C-- (a few constructs are missing), and it is augmented with some macros that are expanded by GHC's code generator (eg. `INFO_TABLE()`).  A `.cmm` file is compiled by GHC itself: the syntax is parsed by [compiler/cmm/CmmParse.y](/trac/ghc/browser/ghc/compiler/cmm/CmmParse.y) and [compiler/cmm/CmmLex.x](/trac/ghc/browser/ghc/compiler/cmm/CmmLex.x) into the [Cmm](commentary/compiler/cmm-type) data type, where it is then passed through one of the [back-ends](commentary/compiler/backends).


We use the C preprocessor on `.cmm` files, making extensive use of macros to make writing this low-level code a bit less tedious and error-prone.  Most of our C-- macros are in [includes/Cmm.h](/trac/ghc/browser/ghc/includes/Cmm.h). One useful fact about the macros is `P_` is an alias for `gcptr`, and you should not use it for non-garbage-collected pointers.


For more information on Cmm (GHC's implementation of C--), see [the Cmm language](commentary/compiler/cmm-type) page.
