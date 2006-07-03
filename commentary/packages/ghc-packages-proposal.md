
This page summarises our current proposal for packages in GHC.

## The problem


A vexed question in the current design of Haskell is the issue of whether a single program can contain two modules with the same name.  Currently that is absolutely ruled out, and as a result packages are fundamentally non-modular: every package must use a distinct space in the global namespace. 


There are two quite separate issues.

### Question 1: Can two different packages contain a module with the same module name?


I now think that's unreasonable to answer 'no', because that means that EVERY module in EVERY package written by ANYONE must have different module names. That's like saying that every function must have different local variables, and is a serious loss of modularity.  I suspect that this is something about which we can all agree.


To allow different packages to contain a module with the same name, the implementation must keep module names from different packages distinct.  If this was done, you could build a single program that used two packages that each used a *hidden* module M.


But what if two pacakges expose the same module M?  That takes us to Question 2.

### Question 2.  How are the (exposed) modules from an (installed) package brought into scope?


That is, when you say "import M", from what package does M come?


Here GHC already has a fairly elaborate scheme (perhaps too elaborate).

- For a start, you may or may not have a package installed.  
- Even if you do, the package may or may not be exposed by default (reasoning: you may want old versions of package X to be installed, but not in scope by default).  
- Then, you can use the `-hide-package` flag to hide an otherwise-exposed package, and the `-package` flag to expose an otherwise-hidden package.


By manipulating these flags, you can expose package P1 when compiling module A (say), and expose P2 when compiling module B.  Then A and B could both import module M, which would come from P1 and P2 respectively. But:

- What if you wanted to import M from P1 and M from P2 into the *same* module?
- Compiling different modules with different flags in a way that affects the *semantics* (rather than, say, the optimisation level) seems undesirable.

## Our proposed design


Simon Marlow and I have gradually become convinced that we have to fix this, and the only sensible way to fix it is to relax the language design so that

- a module name must be unique within its package (only)


That means that module A.B.C could exist \*both\* in package P1 and in P2. And both packages could be linked into the same program. Suppose for the moment that A.B.C is not exposed by both P1 and P2.  Then you would say simply:

```wiki
  ghc --make Main -o app
```


Note the late binding here.  The authors of packages P1 and P2 didn't need to know about each other, and don't need to choose globally unique names.


Things are a bit more complicated if both P1 and P2 expose A.B.C, because then "`import A.B.C`" is ambiguous. Then it's unlikely that both P1 and P2 are exposed packages, and you'll need to bring them into scope explicitly:

```wiki
  ghc -c -package P1 M1.hs
  ghc -c -package P2 M2.hs
  ...compile other modules...
  ghc -o app M1.o M2.o ... -package P1 -package P2
```


To support `--make` in this situation we'd need to allow `-package` flags in the per-module `OPTIONS` pragmas.  (`ghc --make` already gathers those options together for the link step.)


The fundamental thing GHC needs to do is to include the package name into the names of entities the package defines.  That means that when compiling a module M you must say what package it is part of:

```wiki
  ghc -c -package-name P1 C.hs
```


Then C.o will contain symbols like "`P1.A.B.C.f`" etc.  In effect, the "original name" of a function `f` in module `M` of package `P` is `<P,M,f>`.

## Optional extra: the Packages space


Perhaps every (exposed) module from every (installed) package should always be available via an import like

```wiki
   import Packages.Gtk-1_3_4.Widget.Button
```


That is, the module is name by a fully-qualified name involving its package name (already globally unique).  


(Tiresome side note: to make the package id look like a module name we may have to capitalise it, and change dots to underscores.  And that could conceivably make two package names collide.)

## Optional extra: grafting


Some kind of 'grafting' or 'mounting' scheme could be added, to allow late binding of where in the module tree the is brought into scope.  One might say

```wiki
	ghc -c Foo.hs -package gtk-2.3=Graphics.GTK
```


to mount the `gtk-2.3` package at `Graphics.GTK` in the module name space.  Outside
the package one would need to import `Graphics.GTK.M`, but within the package one just imports `M`.  That way the entire package can be mounted elsewhere in the namespace, if desired, without needing to change or recompile the package at all.


This would allow a single module to import modules from two different packages that happened to use the same name.  It's not strictly a necessary feaure.  If you want to

- import module A from package P, and 
- import module A from package Q into a single module M of a program, 


you can always do this:

- make a new module AP, that imports A and re-exports it all; 
- compile AP with package P visible and Q hidden
- ditto for AQ
- make M say "import AP; import AQ".


The exact details of the mounting scheme, and whether it is done at
build time, at install time, or at compilation time, or all of the
above, are open to debate.  We don't have a very fixed view.
