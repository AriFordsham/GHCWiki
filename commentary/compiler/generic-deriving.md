# The new Generic Deriving mechanism


GHC includes a new (in 2010) mechanism to let you write generic functions.  It is described in [ A generic deriving mechanism for Haskell](http://www.dreixel.net/research/pdf/gdmh_nocolor.pdf), by Magalhães, Dijkstra, Jeuring and Löh.  This page sketches the specifics of the implementation; we assume you have read the paper.


This mechanism replaces the [previous generic classes implementation](http://www.haskell.org/ghc/docs/6.12.2/html/users_guide/generic-classes.html).

## Main components

- `TcDeriv.tcDeriving` generates an `InstInfo` for each data type that **fill in**

- **Say which library modules, in which packages, contain which data types and classes**.
