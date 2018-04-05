
On this page we describe the principles behind the implementation of the linear types extension as described at \[[LinearTypes](linear-types)\].


The current implementation progress can be seen on [ here](https://github.com/tweag/ghc/tree/wip/linear-types)

## Principles


The main principle behind the implementation is to modify `FunTyCon` with an extra argument which indicates the \*multiplicity\* of an arrow. The data type for multiplicities is defined
in `compilerbasicTypes/Weight.hs` and is called `Rig`. There are two multiplicities, `One` which indicates that the function is linear and `Omega` which indicates that it is not.


Binders also have a weight attached to them. 


The rest of the implementation is essentially correctly propagating and calculating linearity information whenever a `FunTy` is created.

## Core Lint

TODO - this is described somewhat in the minicore document but it is not finished. 
