# Package Reorg


In this page we collect proposals and design discussion for
reorganising the packages that come with compilers, and the contents
of those packages.


None of the ideas herein are claimed to belong to any particular
person, many of the ideas have been extracted from mailing list
discussions, eg.

> [ http://www.haskell.org/pipermail/libraries/2006-November/006396.html](http://www.haskell.org/pipermail/libraries/2006-November/006396.html)


Some of the points are GHC-specific.  Please feel free to insert
points specific to other compilers.

## Goals

- It would be good to have set of packages that is installed with
  every Haskell implementation.  This seems to be Bulat's main point
  in the thread above.
- It should be possible to upgrade any package, even if that package
  came with the compiler.

## Proposal


Here's a straw-man proposal
  

- There is a set of packages that come with every conforming Haskell
  implementation.  Let's call these the **Core Packages** to
  avoid confusion (Bulat called these the "base packages", but that's an 
  over-used term given that there is a package called `base`).
  The good thing about the Core Packages is that
  users know that they will be there, and they are consistent with
  each other.

- Any particular implementation may install more packages by default;
  for example GHC will install the `template-haskell` and `stm`
  packages.  Let's call these the **GHC Install Packages**, **Hugs
  Install Packages** etc; the Install Packages are a superset of the
  Core Packages.

### What is in the Core Packages?


The Core Packages are installed with every conforming Haskell implementation.  What should be in the Core?  There is a tension:

1. **As much as possible**; which means in practice widely-used and reasonably stable packages.  It is convenient for programmers to have as much as possible in a consistent, bundle that is (a) known to work together bundle, and (b) known to work on all implementations.  
1. **As little as possible**; which in practice means enough to run Cabal so that you can run the Setup files that come when downloading new packages.  As Ian puts it: the less we force the implementations to come with, the quicker compilation will be when developing, the smaller Debian packages (for example) can be, the lower the disk space requirements to build GHC, the lower the time wasted when a Debian package (for example) build fails and the fewer packages we are tangling up with compiler release schedules.


There's a real choice here: Bulat wants (1) and Ian wants (2).


Initial stab at (1):

- `base`
- `haskell98`
- `Cabal`
- `filepath`


Initial stab at (2):

- `base`
- `Cabal`
- `haskell98`
- Some `regex` packages (precisely which?)
- `unix` or `Win32`
- `parsec`
- `mtl`
- `time`
- `network`


Questionable:

- `QuickCheck`
- `HUnit`


Bulat: i think that all regex packages should be included and of course libs that helps testing. overall, it should be any general-purpose lib that porters accept (emlarging these sets makes users live easier, and porters live harder)

### The base package


The base package is a bit special

- Package `base` is rather big at the moment.  

- From a user's point of view it would be nicer to give it a
  compiler-independent API.  (A module like `GHC.Exts` would move to
  a new package `ghc-base`.)


Thinking of GHC alone for a moment, we could have a package `ghc-base`
(which is pretty much the current `base`) and a thin wrapper package
`base` that re-exposes some, but not all, of what `ghc-base` exposes.
To support this re-exposing, we need a small fix to both GHC and
Cabal, but one that is independently desirable.


Similarly, Hugs could build `hugs-base` from the same souce code, by
using CPP-ery, exactly as now.  The thin `base` wrapper package
would not change. 


To make `base` smaller, we could remove stuff, and put it into 
separate packages.  But be careful: packages cannot be cyclic, so
anything that is moved out can't be used in `base`.
Some chunks that would currently be easy to split off are:

- Data.ByteString.\* (plus future packed Char strings)
- Control.Applicative (?), Data.Foldable, Data.Monoid (?), Data.Traversable, Data.Graph, Data.IntMap, Data.IntSet, Data.Map, Data.Sequence, Data.Set, Data.Tree
- System.Console.GetOpt
- Text.PrettyPrint.\*
- Text.Printf


Some other things, such as arrays and concurrency, have nothing else depending on them, but are so closely coupled with GHC's internals that extracting them would require exposing these internals in the interface of `base`.


Bulat: my ArrayRef library contains portable implementation of arrays. there is only thin ghc/hugs-specific layer which should be provided by ghcbase/hugsbase libs. except for MPTC problem (IArray/MArray classes has multiple parameters), this library should be easily portable to any other haskell compiler

### Other packages


Other non-core packages would probably have their own existence.  That
is, they don't come with an implementation; instead you use
`cabal-get`, or some other mechanism, such as your OS's package
manager.  Some of these currently come with GHC, and would no longer do
so

- `GLUT`
- `ALUT`
- `OpenAL`
- `OpenGL`
- `HGL`
- `HUnit`
- `ObjectIO`
- `X11`
- `arrows`
- `cgi`
- `fgl`
- `html`
- `xhtml`


Bulat: i propose to unbundle only graphics/sound libs because these solves particular problems and tends to be large, non-portable (?) and just legacy ones - like ObjectIO. we should keep everything small & general purpose, including HUnit, arraows, fgl, html and xhtml, and include even more:


ByteString, regex-\*, Edison, Filepath, MissingH, NewBinary, QuickCheck, monads

## Testing


We should separate out package-specifc tests, which should be part of
the repository for each package.  Currently they are all squashed
together into the testsuite repository.

## Implementation-specific notes

### Notes about GHC


Currently GHC installs a set of packages by default: base, stm,
template-haskell, cabal, haskel98, readline, 3 of the 5 regex
packages.  These are exactly the libraries required to build GHC.
That shouldn't be the criterion.  This set of packages are currently
called GHC's "core packages", but should be renamed to **GHC Boot
Packages**.


One reason we do this is because it means that every GHC installation
can build GHC.  Less configure-script hacking.  (NB: even today if you
upgrade any of these packages, and then build GHC, the build might
fail because the CPP-ery in GHC's sources uses only the version number
of GHC, not the version number of the package.)


Still, for convenience we'd probably arrange that the GHC Install
Packages included all the GHC Boot Packages.


Every GHC installation must include packages: `base` and
`template-haskell`, else GHC itself will not work.  (In fact
`haskell98` is also required, but only because it is linked by
default.)


So GHC's Install Packages would be the Core Packages plus

- `template-haskell`
- `stm`
- `readline`


You can upgrade any package, including `base` after installing GHC.
However, you need to take care. You must not change a number of things
that GHC "knows about".  In particular, these things must not change

- Name
- Defining module


GHC knows even more about some things, where you must not change

- Type signature
- For data types, the names, types, and order of the constructors


The latter group are confined to packages base and template-haskell.


(Note: a few other packages are used by tests in GHC's test suite,
currently: `mtl`, `QuickCheck`.  We should probably eliminate the mtl
dependency; but `QuickCheck` is used as part of the test infrastructure
itself, so we'll make it a GHC Boot Package.)

### Notes about Hugs


Recent distributions of Hugs come in two sizes, jumbo and minimal.
Minimal distributions include only the packages `base`, `haskell98` and `Cabal`.
(Hugs includes another package `hugsbase` containing interfaces to Hugs primitives.)
The requirements for this set are to

- run Haskell 98 programs
- allow packages to be added and upgraded using Cabal


(Currently `cpphs` is a Haskell 98 program, so the latter implies the former.)


It should be possible to upgrade even the core packages using Cabal.
