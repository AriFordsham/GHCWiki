# GHC Status Report, May 2014


GHC development has been steadily moving forward. While we originally planned to release 7.8.1 back in November, we unfortunately had aspects of scope creep - we began fixing bugs, and adding a minor thing, and fixing bugs, and fixing bugs.


However, 7.8.1 was released in early April this year. It turned out we had a disastrous bug slip in between the release candidates however, which required an immediate fix and the release of 7.8.2 \[GHC8978\]. Other than shuffling back our version numbers, this luckily didn't change much. We plan to release 7.8.3 later in the year.


Meanwhile, HEAD steams onward, with some preliminary work for the 7.10 milestone laid down. We've already got some plans as to what we'll be doing - and if you want something done, you should join in as well!

## Libraries, source language, type system

- **Applicative-Monad** - GHC 7.10 will (finally) make `Applicative` a superclass of `Monad`. This is an API-breaking change for `base`, and users are encouraged to begin fixing their code now. To that end, GHC 7.8 now emits warnings for code that would violate the Applicative-Monad proposal \[AMP\].

- **[ApplicativeDo](applicative-do)** - Now that `Applicative` is a superclass of `Monad`, Simon Marlow has plans to implement a new extension for GHC, which will allow `do` notation to be used in the context of `Applicative`, not just `Monad`.

- **Overloaded record fields** - In 2013, Adam Gundry implemented the new `-XOverloadedRecordFields` extension for GHC, described on the wiki \[ORF\]. This will finally be available in GHC 7.10.

- **Kinds without Data** - Trevor Elliott, Eric Mertens, and Iavor Diatchki have began implementing support for "data kind" declarations, described in more detail on the GHC wiki \[KD\]. The idea is to allow a new form of declaration that introduces a new kind, whose members are described by the (type) constructors in the declaration. This is similar to promoting data declarations, except that no new value-level-constructors are declared, and it also allows the constructors to mention other kinds that do not have corresponding type-level representation (e.g., \*). 

- **Explicit type application** - Stephanie Weirich, Richard Eisenberg and Hamidhasan Ahmed have been working on adding explicit type applications to GHC. This allows the programmer to specify the types that should be instantiated for arguments to a function application, where normally they would be inferred. While this capability already exists in GHC's internal language, System FC -- indeed, every FC-pro program has function application with explicitly applied types -- it has not been available in Haskell itself. While a lot of the syntax and design is not quite final, there are some details about the design available on the wiki \[TA\].

- **Using an SMT Solver in the type-checker** - Iavor Diatchki is working on utilizing an off-the-shelf SMT solver in GHC's constraint solver. Currently, the main focus for this is improved support for reasoning with type-level natural numbers, but it opens the doors to other interesting functionality, such as supported for lifted (i.e., type-level) `(&&)`, and `(||)`, type-level bit-vectors (perhaps this could be used to implement type-level sets of fixed size), and others.   This work is happening on branch `wip/ext-solver`.

- **Kind equality and kind coercions** - Richard Eisenberg (with support from Simon PJ and Stephanie Weirich, among others) is implementing a change to the Core language, as described in a recent paper \[FC\]. When this work is complete, *all* types will be promotable to kinds, and *all* data constructors will be promotable to types. This will include promoting type synonyms and type families. As the details come together, there may be other source language effects, such as the ability to make kind variables explicit. It is not expected for this to be a breaking change -- the change should allow strictly more programs to be accepted.

- **Partial type signatures** - Thomas Winant and Dominique Devriese (with support from Simon PJ) have been working on partial type signatures for GHC. A partial type signature is a type signature that can contain *wildcards*, written as underscores. These wildcards can be types unknown to the programmer or types he doesn't care to annotate. The type checker will use the annotated parts of the partial type signature to type check the program, and infer the types for the wildcards. A wildcard can also occur at the end of the constraints part of a type signature, which indicates that an arbitrary number of extra constraints may be inferred. Whereas TypedHoles allow holes in your terms, PartialTypeSignatures allow holes in your types. The design as well as a working implementation are currently being simplified \[PTS\].

- **TLS Support in AMQP library** - Alain O'Dea, Holger Reinhardt, Vincent Hanquez, and Michael Klishin collaborated to provide TLS support for the Advanced Message Queing Protocol (AMQP) library.  This involved replacing the existing GHC.IO.Handle transport with Network.Connection. Vincent provided a pure Haskell implementation of TLS in the connection library and addressed a deadlock issue that the AMQP library's multithreaded use of connections uncovered. Options were added to control whether TLS was desired and whether or not to perform certificate verification.

## Back-end and runtime system

- **CPU-specific optimizations** - Austin is currently investigating the implementation of CPU-specific optimisations for GHC, including new `-march` and `-mcpu` flags to adjust tuning for a particular processor. Right now, there is some preliminary work towards optimizing copies on later Intel machines. There's interest in expanding this further as well.

- **Changes to static closures for faster garbage collection** - Edward is working on an overhaul of how static closures represented at runtime to eliminate some expensive memory dereferences in the GC hotpath. The initial results are encouraging: these changes can result in an up to 8% in the runtime of some GC heavy benchmarks \[HEAPALLOCED\].

- **Coverity** - Austin & friends have began running the Coverity static analyzer over the GHC runtime system in an attempt to weed out bugs \[CoverityScan\]. This has luckily reported several very useful issues to us, and identified some possible cleanup. These fixes are also going into the 7.8 branch, and GHC and its associated code will be scanned by Coverity continuously in the future.

- **New, smaller array type** - Johan Tibell has recently added a new array type, `SmallArray#`, which uses less memory (2 words) than the `Array#` type, at the cost of being more expensive to garbage collect for array sizes large than 128 elements.

## Frontend, build-system, and miscellaneous changes

- **Repo reorganization** One big thing that Herbert Valerio Riedel has been tackling has been the problematic situation with GHC's current usage of git submodules and `./sync-all`. This is one of our most common complaints from newcomers and people attempting to help with development (with good reason), and we're hoping within the 7.10 timeframe, GHC will be far easier to clone and work on.

>
> To this end, we've already done some massive simplification - in HEAD, the repositories for `base`, `testsuite`, `template-haskell`, `ghc-prim`, `integer-gmp` and `integer-simple` are now part of GHC's repository itself. These repositories are commonly developed in lockstep with GHC, and it greatly helps in many workflows, including bisection of bugs.

- **Continuous integration improvements** - Work on new CI systems for GHC has been slow, but thanks to the work of **Joachim Breitner** and **Gábor Páli**, GHC is now built on [ http://travis-ci.org](http://travis-ci.org) \[TravisCI\] as well as nightly builders of a variety of flavors and machines \[Builders\]. We're also hoping to investigate using a Continuous Integration system to help build against a stable set of selected Hackage packages, to help find issues with the releases more easily.

- **Debian builds of GHC** - Thanks to **Joachim Breitner** and **Herbert Valerio Riedel**, GHC now has greatly improved support for Debian packaging - there is now an official Ubuntu PPA for GHC \[PPA\], as well as a dedicated Debian repository for GHC nightly builds \[DEB\].

## Future Plans

- **Dynamic space limits** - Edward has been working on dynamic space limits for Haskell, whereby you can run some code in a container with a maximum space limit associated with it.  There's working code \[RLIMITS\] but there are some barriers to getting it deployable in GHC (it requires a new compilation mode ala prof, and it doesn't yet work with GHCi or 32-bit).

# References


\[GHC8978\] [ https://ghc.haskell.org/trac/ghc/ticket/8978](https://ghc.haskell.org/trac/ghc/ticket/8978)

\[AMP\] [ https://github.com/quchen/articles/blob/master/applicative_monad.md](https://github.com/quchen/articles/blob/master/applicative_monad.md)
 
\[KD\] Kinds without Data - [ http://ghc.haskell.org/trac/ghc/wiki/GhcKinds/KindsWithoutData](http://ghc.haskell.org/trac/ghc/wiki/GhcKinds/KindsWithoutData)
 
\[ORF\] [ https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields](https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields)

\[TA\] Explicit type application - [ http://ghc.haskell.org/trac/ghc/wiki/ExplicitTypeApplication](http://ghc.haskell.org/trac/ghc/wiki/ExplicitTypeApplication)

\[FC\] "System FC with Explicit Kind Equality" - [ http://www.seas.upenn.edu/\~eir/papers/2013/fckinds/fckinds-extended.pdf](http://www.seas.upenn.edu/~eir/papers/2013/fckinds/fckinds-extended.pdf)

\[PTS\] [ https://ghc.haskell.org/trac/ghc/wiki/PartialTypeSignatures](https://ghc.haskell.org/trac/ghc/wiki/PartialTypeSignatures)

\[CoverityScan\] [ https://scan.coverity.com](https://scan.coverity.com)
 
\[PPA\] [ https://launchpad.net/\~hvr/+archive/ghc/](https://launchpad.net/~hvr/+archive/ghc/)
 
\[DEB\] [ http://deb.haskell.org](http://deb.haskell.org)
 
\[TravisCI\] [ https://github.com/nomeata/ghc-complete](https://github.com/nomeata/ghc-complete)
 
\[Builders\] [ https://ghc.haskell.org/trac/ghc/wiki/Builder](https://ghc.haskell.org/trac/ghc/wiki/Builder)

\[HEAPALLOCED\] [ https://ghc.haskell.org/trac/ghc/ticket/8199](https://ghc.haskell.org/trac/ghc/ticket/8199)
 
\[RLIMITS\] [ http://ezyang.com/rlimits.html](http://ezyang.com/rlimits.html)