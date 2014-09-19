# GHC plans for 7.10.1


There's still a lot planned for GHC 7.10. Our current release schedule looks like **we'd like an RC by Christmas**, and a **release close to Feburary 2015**


See [milestone:7.10.1](/trac/ghc/milestone/7.10.1) and [ Active tickets](https://ghc.haskell.org/trac/ghc/query?status=infoneeded&status=merge&status=new&status=patch&group=status&milestone=7.10.1)

## Libraries, source language, type system

- **Signature sections**.  Lennart Augustsson is implementing `(:: ty)` to work the same as `(\x->x::ty)`.  Needs a wiki design page.

- **Applicative-Monad** - GHC 7.10 will (finally) make `Applicative` a superclass of `Monad`. This is an API-breaking change for `base`, and users are encouraged to begin fixing their code now. To that end, GHC 7.8 now emits warnings for code that would violate the Applicative-Monad proposal [ AMP](https://github.com/quchen/articles/blob/master/applicative_monad.md).

- **[ApplicativeDo](applicative-do)** - Now that `Applicative` is a superclass of `Monad`, Simon Marlow has plans to implement a new extension for GHC, which will allow `do` notation to be used in the context of `Applicative`, not just `Monad`.

- **Strict language extension**.  Johan Tibell is working on a `-XStrict` language extension that will make GHC compile programs in a by-default strict way.  [Details here](language-strict).

- **Cloud Haskell statics**.  Mathieu Boespflug and Facundo Domínguez at TweagIO are working on support for Cloud Haskell's `static` feature.  [Details here](static-pointers).

- **Overloaded record fields** - In 2013, Adam Gundry implemented the new `-XOverloadedRecordFields` extension for GHC, described on the wiki [OverloadedRecordFields](overloaded-record-fields). This will finally be available in GHC 7.10.

- **Kinds without Data** - Trevor Elliott, Eric Mertens, and Iavor Diatchki have began implementing support for "data kind" declarations, described in more detail on the GHC wiki \[KD\]. The idea is to allow a new form of declaration that introduces a new kind, whose members are described by the (type) constructors in the declaration. This is similar to promoting data declarations, except that no new value-level-constructors are declared, and it also allows the constructors to mention other kinds that do not have corresponding type-level representation (e.g., \*). 

- **Explicit type application** - Stephanie Weirich, Richard Eisenberg and Hamidhasan Ahmed have been working on adding explicit type applications to GHC. This allows the programmer to specify the types that should be instantiated for arguments to a function application, where normally they would be inferred. While this capability already exists in GHC's internal language, System FC -- indeed, every FC-pro program has function application with explicitly applied types -- it has not been available in Haskell itself. While a lot of the syntax and design is not quite final, there are some details about the design available on the wiki [ExplicitTypeApplication](explicit-type-application).

- **Using an SMT Solver in the type-checker** - Iavor Diatchki is working on utilizing an off-the-shelf SMT solver in GHC's constraint solver. Currently, the main focus for this is improved support for reasoning with type-level natural numbers, but it opens the doors to other interesting functionality, such as supported for lifted (i.e., type-level) `(&&)`, and `(||)`, type-level bit-vectors (perhaps this could be used to implement type-level sets of fixed size), and others.   This work is happening on branch `wip/ext-solver`.

- **Kind equality and kind coercions** - Richard Eisenberg (with support from Simon PJ and Stephanie Weirich, among others) is implementing a change to the Core language, as described in a recent paper [ FC](http://www.seas.upenn.edu/~eir/papers/2013/fckinds/fckinds-extended.pdf). When this work is complete, *all* types will be promotable to kinds, and *all* data constructors will be promotable to types. This will include promoting type synonyms and type families. As the details come together, there may be other source language effects, such as the ability to make kind variables explicit. It is not expected for this to be a breaking change -- the change should allow strictly more programs to be accepted.

- **Partial type signatures** - Thomas Winant and Dominique Devriese are working on partial type signatures for GHC. A partial type signature is a type signature that can contain *wildcards*, written as underscores. These wildcards can be types unknown to the programmer or types he doesn't care to annotate. The type checker will use the annotated parts of the partial type signature to type check the program, and infer the types for the wildcards. A wildcard can also occur at the end of the constraints part of a type signature, which indicates that an arbitrary number of extra constraints may be inferred. Whereas `-XTypedHoles` allow holes in your terms, `-XPartialTypeSignatures` allow holes in your types. The design as well as a working implementation are currently being simplified [PartialTypeSignatures](partial-type-signatures).

## Back-end and runtime system

- **CPU-specific optimizations** - Austin is currently investigating the implementation of CPU-specific optimisations for GHC, including new `-march` and `-mcpu` flags to adjust tuning for a particular processor. Right now, there is some preliminary work towards optimizing copies on later Intel machines. There's interest in expanding this further as well.

- **Changes to static closures for faster garbage collection** - Edward is working on an overhaul of how static closures represented at runtime to eliminate some expensive memory dereferences in the GC hotpath. The initial results are encouraging: these changes can result in an up to 8% in the runtime of some GC heavy benchmarks, see [\#8199](https://gitlab.haskell.org//ghc/ghc/issues/8199).

- **New, smaller array type** - Johan Tibell has recently added a new array type, `SmallArray#`, which uses less memory (2 words) than the `Array#` type, at the cost of being more expensive to garbage collect for array sizes larger than 128 elements.

- **Faster small array allocation** - Johan Tibell has made array allocation of arrays of small, statically know size faster by making it inline with the normal heap check, instead of out-of-line in a separate nursery block.

- **DWARF-based stack tracing** - Peter Wortmann and Arash Rouhani (with support from the Simons) are working on enabling GHC to generate and use DWARF debugging information. This should allow us to obtain stack traces and do profiling without the need for instrumentation.

- **Reimplemented GMP-based `Integer` backend ([\#9281](https://gitlab.haskell.org//ghc/ghc/issues/9281))** - This provides a GMP-based `Integer` backend not relying on registering GHC-specific [ custom GMP memory allocators](https://gmplib.org/manual/Custom-Allocation.html) which cause problems when linking to other C-code also using GMP unaware of GHC's memory management.

# Tickets


See [milestone:7.10.1](/trac/ghc/milestone/7.10.1)