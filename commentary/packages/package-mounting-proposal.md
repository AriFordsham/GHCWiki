### Proposal for Package Mounting


It may help to refer to [Commentary/Packages/GhcPackagesProposal](commentary/packages/ghc-packages-proposal) for an introduction to some of the issues mentioned here.


A message by Frederik Eaton to the Haskell mailing list describing the present proposal is archived: [http://www.haskell.org/pipermail/libraries/2005-June/004009.html](http://www.haskell.org/pipermail/libraries/2005-June/004009.html). (Also, see note at the end of this document regarding an earlier proposal by Simon Marlow)


This document will go over Frederik's proposal again in brief. The proposal doesn't involve any changes to syntax, only an extra command line option to `ghc`, etc., and a small change to Cabal syntax.


In this proposal, during compilation of a module, every package would have a "mount point" with respect to which its particular module namespace would be resolved. Each package should have a default "mount point", but this default would be overridable with an option to `ghc`, etc.


For example, the `X11` library currently has module namespace:

```wiki
  Graphics.X11.Types
  Graphics.X11.Xlib
  Graphics.X11.Xlib.Atom
  Graphics.X11.Xlib.Event
  Graphics.X11.Xlib.Display
  ...
```


In this proposal, it might instead have default mount point `Graphics.X11` and (internal) module namespace:

```wiki
  Types
  Xlib
  Xlib.Atom
  Xlib.Event
  Xlib.Display
  ...
```


To most users of the X11 package, there would be no change - because of the mounting, modules in that package would still appear with the same names in places where the X11 package is imported: `Graphics.X11.Types`, etc. However, if someone wanted to specify a different the mount point, he could use a special compiler option, for instance `-package-base`:

```wiki
  ghc -package X11 -package-base Graphics.Unix.X11 ...
```


(so the imported namespace would appear as `Graphics.Unix.X11.Types`, `Graphics.Unix.X11.Xlib`, etc.) Note that the intention is for each `-package-base` option to refer to the package specified in the preceding `-package` option, so to give package `PACKAGE` a mount point of `BASE` we use the syntax

```wiki
  ghc ... -package PACKAGE -package-base BASE ...
```


Ideally one would also be able to link to two different versions of the same package, at different mount points:

```wiki
  ghc -package X11-1.2 -package-base NewX11 -package X11-1.0 -package-base OldX11 ...
```


(yielding `NewX11.Types`, `NewX11.Xlib`, ...; `OldX11.Types`, `OldX11.Xlib`, ...)


However, usually the default mount point would be sufficient, so most users wouldn't have to learn about `-package-base`.


Additionally, Cabal syntax should be extended to support mounting. I would suggest that the optional mount point should appear after a package in the Build-Depends clause of a Cabal file:

```wiki
  Build-Depends: X11(Graphics.Unix.X11.Xlib)
```


And in the package Cabal file, a new clause to specify the default mount point:

```wiki
  Default-Base: Graphics.X11
```

### Evaluation


This proposal has several advantages over the [Commentary/Packages/PackageImportsProposal](commentary/packages/package-imports-proposal) proposal.

- *No package names in code*. In this proposal, package names would be decoupled from code. This is very important. It should be possible to rename a package (or create a new version of a package with a new name), and use it in a project, without editing every single module of the project and/or package. Even if the edits could be done automatically, they would still cause revision control headaches. Any proposal which puts package names in Haskell source code should be considered unacceptable.

- *No syntax changes*. The [Commentary/Packages/PackageImportsProposal](commentary/packages/package-imports-proposal) proposal requires new syntax, but this proposal does not. Of course, in this proposal it would be slightly more difficult for the programmer to find out which package a module is coming from. He would have to look at the command line that compiles the code he's reading. However, I think that that is appropriate. Provenance should not be specified in code, since it changes all the time. (And there could be a simple debugging option to GHC which outputs a description of the namespace used when compiling each file)

- *Simpler module names*. This proposal would allow library authors to use simpler module names in their packages, which would in turn make library code more readable, and more portable between projects. For instance, imagine that I wanted to import some of the code from the `X11` library into my own project. Currently, I would have to delete every occurrence of `Graphics.X11` in those modules. Merging future changes after such an extensive modification would become difficult. This is a real problem, which I have encountered while using John Meacham's curses library. There are several different versions of that library being used by different people in different projects, and it is difficult to consolidate them because they all have different module names. The reason they have different module names is that package mounting hasn't been implemented yet. The [Commentary/Packages/PackageImportsProposal](commentary/packages/package-imports-proposal) proposal would not fix the problem.

- *Development decoupled from naming*. (there is a bit of overlap with previous points here) In the present proposal, programmers would be able to start writing a library before deciding on a name for the library. For instance, every module in the `Parsec` library contains the prefix `Text.ParserCombinators.Parsec`. This means that either the author of the library had to choose the name `Parsec` at the very beginning, or he had to make several changes to the text of each module after deciding on the name. Under the present proposal, he would simply call his modules `Char`, `Combinator`, `Error`, etc.; the `Text.ParserCombinators` prefix would be specified in the build system, for instance in the Cabal file.


Frederik's mailing list message discusses some other minor advantages, but the above points are the important ones. In summary, it is argued that the above proposal should be preferred to [Commentary/Packages/PackageImportsProposal](commentary/packages/package-imports-proposal) because it is both easier to implement (using command line options rather than syntax), and more advantageous for the programmer.

### Note on Package Grafting


A proposal by Simon Marlow for "package grafting" predates this one: [http://www.haskell.org/pipermail/libraries/2003-August/001310.html](http://www.haskell.org/pipermail/libraries/2003-August/001310.html). However, the "package grafting" proposal is different in that it suggests selecting a "mount point" at library installation time, where in the present proposal, the "mount point" is selected each time a module using the library in question is compiled. The difference is important, as one doesn't really want to have to install a new copy of a library just to use it with a different name. Also, Simon Marlow's proposal puts package versions in the module namespace and therefore source code, where we argue for decoupling source code from anything to do with provenance - be it package names or version numbers.
