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

- **Extensible exceptions**, along the lines of Simon's paper [ An Extensible Dynamically-Typed Hierarchy of Exceptions](http://www.haskell.org/~simonmar/papers/ext-exceptions.pdf).  This is mainly a library change.  *Simon Marlow*

- **Parallel garbage collection** (see [ Parallel generational-copying garbage collection with a block-structured heap](http://research.microsoft.com/%7Esimonpj/papers/parallel-gc/index.htm)).  *Simon Marlow*

- **Shared libraries**, as a result of Clemens Fruhwirth's Summer of Code project.  *Simon Marlow*

- **[ Type families](http://haskell.org/haskellwiki/GHC/Indexed_types)**, fully working. *Manuel Chakravarty and Simon PJ*

- **[ Nested data parallelism](http://haskell.org/haskellwiki/GHC/Data_Parallel_Haskell)**, in some form. *Roman Leshchinskiy, Gabriele Keller, Manuel Chakravarty, Simon PJ*

- **Substantial changes to the [back end](commentary/compiler/new-code-gen)** are likely, now that John Dias is here as an intern.n  *John Dias, Simon PJ, Norman Ramsey*

- Further **library reorganisation**, but with more attention paid to backward compatibility. *Ian Lynagh*

- Better **versioning** to support separate compilation; perhaps checksums/fingerprints.

- Improvements to the **GHC API** (Thomas Schilling's SoC project)

# Beyond 6.10


This is a list of things that are floating about in our minds for what to do beyond 6.10.  Nothing is decided, and these items vary wildly in their size.

- **Opaque interfaces** (optionally), so you can upgrade a library without recompiling.

- **Parallelism*: better profiling tools.
  ***

- **Visual Haskell**: a Visual Studio plugin.  There is one, but it has suffered bit-rot.

- **GHC as a platform** is the aspiration that it should be easy to plug extensions into GHC, and easy to use GHC to extend other software.

- **Static verification** along the lines of Dana Xu's work.
