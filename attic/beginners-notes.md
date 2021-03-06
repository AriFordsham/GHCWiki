# Notes From a Beginning GHC Hacker

## General notes (some just for Windows)

1. [The Building Guide](building) is very good, read the appropriate parts.
1. Optimize your set up for build speed.  Having to wait 10 minutes to compile because you added an export to a critical module is no fun.

  1. Use MSYS, not cygwin.  MSYS is **much** faster.
  1. Do not use -O to compile GHC while you are making heavy changes.  Once things compile smoothly and you want to test it, compile with optimization (on my machine, adding -O triples the compile time for a fresh 'make').
1. Use [hasktags](https://github.com/MarcWeber/hasktags) or similar program to generate TAGS file, so that you can go right to the definition of a function (from any module) using `M-.` in Emacs (see [here](emacs#using-tags-to-quickly-locate-definitions-in-a-project) for details).
1. If you need to add or delete modules, use 'make boot' to fix up the dependencies.

## Notes on the type system (and its interactions)


The compiler is, of course, found in the fptools/compiler directory.  Important subdirectories for hacking on the type system include: 

- basicTypes (Name, Var, OccName)
- prelude (PrelNames, TysWiredIn)
- types
- typecheck
- iface (IfaceType, BinIface)
- parser (ParserCore)


The critical parts dealing with types are in the 'types' and 'typecheck' directories:

**types**

- TypeRep.lhs

  - The representation of types, the Type datatype, is defined here and exported concretely
  - A few crucial type constructors (like -\>) are defined here in order to avoid excessive module loops
- TyCon.lhs

  - The representation of type constructors, TyCon, is defined here and exported abstractly
  - Construction and manipulation functions for type constructors are here
- Type.lhs

  - Contains the bulk of the type system, imports the concrete representation from TypeRep, and re-exports Type abstractly


(more to come)

## Debugging the compiler

1. Make good use of `ASSERT ( booleanCondition )`, or better yet, `ASSERT2 ( booleanCondition, msg )`
1. You can add trace messages to the forcing of `expr` with `pprTrace str sDoc $ expr`
1. Read about the [GHC Debugging Options](http://www.haskell.org/ghc/docs/latest/html/users_guide/options-debugging.html); the following debugging options are particularly useful:

  - -dshow-passes : shows the compilations phases
  - -ddump-tc-trace : shows the typechecking status messages inserted by `traceTc`
  - -ddump-simpl : shows the Core code after simplification
  - -dppr-debug : makes the debugging dumps more noisy (and useful); among other things, shows kinds for type variables
1. such debugging options can be added to `make` by passing the argument `EXTRA_HC_OPTS='flags'`


 


## Running the testsuite


To run the testsuite:

1. darcs get --partial http://darcs.haskell.org/testsuite
1. make the compiler **and** libraries
1. cd testsuite/tests/ghc-regress; make
