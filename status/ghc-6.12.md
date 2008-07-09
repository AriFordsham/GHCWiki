# Plans for 6.8.3


Here are our [ release plans for 6.8.3](http://www.haskell.org/pipermail/glasgow-haskell-users/2008-March/014467.html)

# Plans for GHC 6.10


We expect to release GHC 6.10 around ICFP 2008.  Here are the big items that we hope to include, along with who is primarily responsible for delivering these promises:

- Several **language extensions** advertised in the [November 2007 status report](status/nov07):

  - Improvements to record syntax
  - View patterns
  - Quasiquotation
  - Generalised list comprehensions

>
> These are all in the HEAD already.

- **External Core** (output only) is working again, thanks to Tim Chevalier.

- **Haddock 2** (see also [\#1964](https://gitlab.haskell.org//ghc/ghc/issues/1964) (GHC.Prim), [\#2335](https://gitlab.haskell.org//ghc/ghc/issues/2335) (build problem)).  (**Ian Lynagh**: a few days.)

  - Build it with GHC (maybe ship it with GHC too)
  - Documentation for GHC API done via Haddock 2

- **Unicode support for text I/O**.  This means adding Unicode encoding/decoding for Text I/O handles.   (**Simon Marlow**: a few days work.)

  - Consensus was that Text I/O should always use the current locale encoding.  
  - You can elect to have no encoding by opening in binary mode, but that's all.

- **Extensible exceptions**, along the lines of Simon's paper [ An Extensible Dynamically-Typed Hierarchy of Exceptions](http://www.haskell.org/~simonmar/papers/ext-exceptions.pdf).  This is mainly a library change.  **Ian Lynagh** is running a discussion, but we expect it to reach consensus in plenty of time for 6.10.

- **Parallel garbage collection** (see [ Parallel generational-copying garbage collection with a block-structured heap](http://research.microsoft.com/%7Esimonpj/papers/parallel-gc/index.htm)).  *Simon Marlow*

- **Shared libraries**, as a result of Clemens Fruhwirth's Summer of Code project.  ([\#1876](https://gitlab.haskell.org//ghc/ghc/issues/1876)) *Simon Marlow*

  - Binaries get much smaller
  - Compile a package on Windows to a DLL; it just works
  - C program (or Excel) that calls multiple Haskell functions gets just one copy of the RTS, rather than one per DLL as now.
  - Performance penalty, but too small to measure

- **[Back-end revamp](commentary/compiler/new-code-gen)** (see also [\#1501](https://gitlab.haskell.org//ghc/ghc/issues/1501)).  **John Dias** is in charge.  For 6.10 we will make sure that the whole existing path still exists, so we can choose at a late date whether to rely on the new path or not.

- **[ Type families](http://haskell.org/haskellwiki/GHC/Indexed_types)**, fully working. *Manuel Chakravarty and Simon PJ*

- **[ Nested data parallelism](http://haskell.org/haskellwiki/GHC/Data_Parallel_Haskell)**, in some form. *Roman Leshchinskiy, Gabriele Keller, Manuel Chakravarty, Simon PJ*

- **Substantial changes to the ** are likely, now that John Dias is here as an intern.n  *John Dias, Simon PJ, Norman Ramsey*

- Further **library reorganisation**, but with more attention paid to backward compatibility. *Ian Lynagh*

- Better **versioning** to support separate compilation; perhaps checksums/fingerprints.

- Improvements to the **GHC API** (Thomas Schilling's SoC project)

- GHC now uses **libffi** to implement parts of the FFI, replacing some of the home-grown and very architecture-specific code we had to do this.  Amongst other benefits, this will ease the task of porting GHC in the future.

## Temporary list of 6.10 priorities

- backwards compat
- more library reorg ([\#1338](https://gitlab.haskell.org//ghc/ghc/issues/1338))
- binary package DB, or at least make the one-file-per package work ([\#593](https://gitlab.haskell.org//ghc/ghc/issues/593), [\#723](https://gitlab.haskell.org//ghc/ghc/issues/723), [\#2089](https://gitlab.haskell.org//ghc/ghc/issues/2089))
- `^C` should raise an exception by default (also SIGPIPE, see [\#1619](https://gitlab.haskell.org//ghc/ghc/issues/1619), [\#2301](https://gitlab.haskell.org//ghc/ghc/issues/2301))
- initial GHC API improvements: preserve comments and pragmas, generic traversals ([\#1467](https://gitlab.haskell.org//ghc/ghc/issues/1467), [\#1886](https://gitlab.haskell.org//ghc/ghc/issues/1886), [GhcApiStatus](ghc-api-status))
- include cabal-install in the release ([\#2385](https://gitlab.haskell.org//ghc/ghc/issues/2385))
- finish System.Process revamp ([\#2233](https://gitlab.haskell.org//ghc/ghc/issues/2233))

# Beyond 6.10


This is a list of things that are floating about in our minds for what to do beyond 6.10.  Nothing is decided, and these items vary wildly in their size.

- **Opaque interfaces** (optionally), so you can upgrade a library without recompiling.

- **Parallelism*: better profiling tools.
  ***

- **Visual Haskell**: a Visual Studio plugin.  There is one, but it has suffered bit-rot.

- **GHC as a platform** is the aspiration that it should be easy to plug extensions into GHC, and easy to use GHC to extend other software.

- **Static verification** along the lines of Dana Xu's work.
