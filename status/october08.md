# GHC Status October 2008


For the last six months we have been primarily focussed on the 6.10.1 release, which should be out by the time you read this. We are extremely grateful for the increasing support we get for the community in putting GHC releases together; more people than ever before are now helping maintain subcomponents, implementing features, fixing bugs, testing release candidates, and much more besides. We couldn't have made this release without your help!

## The GHC 6.10 branch


GHC 6.10.1 is the first release in the 6.10 branch, and features many improvements over the 6.8 branch; the highlights are:

- Some **new language features** have been implemented:

  - Record syntax: wild-card patterns, punning, and field disambiguation
  - Generalised quasi-quotes (Geoff Mainland), from the paper [ Why it's nice to be quoted: quasi-quoting in Haskell](http://www.eecs.harvard.edu/~mainland/ghc-quasiquoting/mainland07quasiquoting.pdf) (Haskell workshop 2007)
  - Generalised list comprehensions (Max Bolingbroke), from the paper [ Comprehensive comprehensions: comprehensions with "Order by" and "Group by"](http://research.microsoft.com/%7Esimonpj/papers/list-comp/index.htm) (Haskell workshop 2007).
  - View patterns (Dan Licata); see [view patterns wiki page](view-patterns).

- **Type families** have been completely re-implemented, by Manuel Chakravarty, along the lines of our ICFP 2008 paper [ Type checking with open type functions](http://research.microsoft.com/%7Esimonpj/papers/assoc-types/index.htm) --- only simpler.  As a result, we believe that type families work reliably in GHC 6.10.  There is one missing feature, however, namely the ability to have equalities in the superclass context of a class.   We'll add that to the HEAD in the next few months.

- GHC now comes with **Haddock 2**, which supports all GHC extensions

- **Parallel garbage collection** has been implemented by Simon Marlow.  This speeds up even purely-sequential programs, by using the extra processors during garbage collection.  Our ISMM'08 paper gives the details [ Parallel generational-copying garbage collection with a block-structured heap](http://research.microsoft.com/%7Esimonpj/papers/parallel-gc/index.htm). 

- The base library now uses extensible exceptions, as described in Simon Marlow's paper [ An Extensible Dynamically-Typed Hierarchy of Exceptions](http://www.haskell.org/~simonmar/papers/ext-exceptions.pdf) (Haskell workshop 2006).

- The GHC API now uses a Ghc Monad, making it easier to use.  Furthermore, the API now has Haddock documentation

- External core (output only) now works again

- Data Parallel Haskell (DPH), which supercedes NDP) is now an extralib


See the release notes for full details.

## The GHC 6.12 branch


Meanwhile, development goes on in the HEAD:

- John Dias is still working hard on rewriting GHC's backend, and his changes should be landing in the next few months

- the DPH team are still developing furiously

- We hope that Max Bolingbroke's "Dynamically Loaded Plugins" summer of code project will be merged in time for 6.12

- Likewise, Donnie Jones's project for profiling parallel programs should be merged in time for 6.12

- Finally, unicode text I/O and dynamic libraries were slated for 6.10 but weren't quite ready in time, so we certainly expect those to make it for in 6.12


From a development point of view, there are a couple of changes on the horizon:

- We plan to change how GHC's build system works, to decouple it from Cabal's internals

- We plan to change from darcs to git for the version control system used by GHC
