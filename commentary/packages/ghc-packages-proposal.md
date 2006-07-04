
This page summarises our current proposal for packages in GHC.

## The problem


A vexed question in the current design of Haskell is the issue of whether a single program can contain two modules with the same name.  Currently that is absolutely ruled out, and as a result packages are fundamentally non-modular: every package must use a distinct space in the global namespace. 


There are two quite separate issues, addressed in "Question 1", "Question 2" below.  First we give assumptions.

## Assumptions


Before we start, note that we take for granted the following

- Each package has a globally-unique name, organised by some social process.  This assumption is deeply built into Cabal, and lots of things would need to change if it wasn't met.

- Module names describe *purpose* (what it's for, e.g. `Data.Bits`), whereas package names describe *provenance* (where it comes from, e.g. `"gtkhs"`).  We should not mix these two up, and that is a good reason for not combining package and module names into a single grand name.  One quite frequently wants to globally change provenance but not purpose (e.g. compile my program with a new version of package "foo"), without running through all the source files to change the import statements.

## Question 1: Can two different packages contain a module with the same module name?


We now think that's unreasonable to answer 'no', because that means that EVERY module in EVERY package written by ANYONE must have different module names. That's like saying that every function must have different local variables, and is a serious loss of modularity.  We suspect that this is something about which we can all agree.


The only sensible way to fix it is to relax the language design so that

- A module name must be unique within its package (only)


That means that module A.B.C could exist \*both\* in package P1 and in P2. And both packages could be linked into the same program. Suppose for the moment that A.B.C is not exposed by both P1 and P2.  Then you would say simply:

```wiki
  ghc --make Main -o app
```


The authors of packages P1 and P2 didn't need to know about each other, and don't need to choose globally unique module names.


For this to work, GHC must incorporate the package name (and version) into the names of entities the package defines.  That means that when compiling a module M you must say what package it is part of:

```wiki
  ghc -c -package package-name P1 C.hs
```


Then C.o will contain symbols like "`P1.A.B.C.f`" etc.  In effect, the "original name" of a function `f` in module `M` of package `P` is `<P,M,f>`.


But what if two packages **expose** the same module A.B.C?  That takes us to Question 2.

## Question 2.  When you say "import A.B.C", from what package does A.B.C come?


Here GHC already has a fairly elaborate scheme (perhaps too elaborate).

- For a start, you may or may not have a package installed.  
- Even if you do, the package may or may not be exposed by default (reasoning: you may want old versions of package X to be installed, but not in scope by default).  
- Then, you can use the `-hide-package` flag to hide an otherwise-exposed package, and the `-package` flag to expose an otherwise-hidden package.


When GHC incorporates package names in exported symbols, you will be able to expose package P1 when compiling module M (say), and expose P2 when compiling module N by manipulating these flags.  Then M and N could both import module A.B.C, which would come from P1 and P2 respectively. But:

- What if you wanted to import A.B.C from P1 and A.B.C from P2 into the *same* module?
- What if you want to only replace *parts* of P1 (e.g., you want to use an updated version of a module in `base`)?
- Compiling different modules with different flags in a way that affects the *semantics* (rather than, say, the optimisation level) seems undesirable.
- To support `--make` in this situation we'd need to allow `-package` flags in the per-module `OPTIONS` pragmas, which isn't currently supported.  (`ghc --make` already gathers those options together for the link step.)


The obvious solution is to allow the programmer to specify the source package in the import line, something like this:

```wiki
  import A.B.C from "base" ( map, filter )
```


That would presumably get the most recent installed incarnation of the `base` package. If you want a particular version of the package, we could allow

```wiki
  import A.B.C from "base-3.4" ( map, filter )
```


The exact syntax is unimportant. The important thing is that the programmer can specify the package in the source text.


An open question: if A.B.C is in the package being compiled, and in an exposed package, and you say `import A.B.C`, do you get an error ("ambiguous import"), or does the current package override.  And if the former, how can you say "import A.B.C from the current package"?
