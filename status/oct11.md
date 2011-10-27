# GHC Status October 2011


GHC is still humming along, with the 7.2.1 release (more of a "technology preview" than a stable release) having been made in August, and attention now focused on the upcoming 7.4 branch. By the time you read this, the 7.4 branch will have been created, and will be in "feature freeze". We will then be trying to fix as many bugs as possible before releasing later in the year.


We advertised 7.2 as a technology preview, expecting 7.4 to merely consolidate the substantial new features in 7.2.  But as it turns out GHC 7.4 will have a further wave of new features, especially in the type system.
Significant changes planned for the 7.4 branch are:

- **Declarations at the GHCi prompt.** Simon Marlow has extended GHCi so that it is possible to give any declaration at the ghci prompt. For example,

  ```wiki
  Prelude> data D = D Int
  Prelude> case D 5 of D x -> print x
  5
  ```

  This has already been merged, so will definitely be in 7.4.

- **Data type promotion and kind polymorphism.**  As we do more and more type-level programming, the lack of a decent kind system (to make sure that your type-level programs make sense) has become an increasingly pressing issue.  If all goes well, GHC 7.4 will take a substantial step forward:

  - First, all simple data types (including lists and tuples) will automatically be "promoted" to be kinds as well, a design inspired by Conor McBride's Strachclyde Haskell Extension [ http://personal.cis.strath.ac.uk/\~conor/pub/she/ SHE](http://personal.cis.strath.ac.uk/~conor/pub/she/ SHE).  For example:

    ```wiki
    type family F :: Bool -> *
    type instance F True  = Int
    type instance F False = Char
    ```
  - Second, GHC will support full kind polymorphism.  For example, consider the following data type declaration

    ```wiki
    data T f a = MkT (f a)
    -- T :: forall k. (k -> *) -> k -> *
    ```

    GHC will now infer the polymorphic kind signature above, rather that "defaulting" to `T :: (*->*) -> * -> *` as Haskell98 does.

> >
> > These new kind-system developents are described in "Giving Haskell a promotion" [ http://research.microsoft.com/\~simonpj/papers/ext-f/ KindPolymorphism](http://research.microsoft.com/~simonpj/papers/ext-f/ KindPolymorphism).  Julien Cretin and Pedro Magalhães have done all the implementation.

- **Constraint kinds.** Max Bolingbroke has implemented another extension to GHC's kind system, by adding the kind `Constraint` that classifies type constraints.  This turns out to be a rather neat way to implement all the joy of Tom Schrijvers and Dominic Orchard's paper "Haskell type constraints unleashed" [ http://tomschrijvers.blogspot.com/2009/11/haskell-type-constraints-unleashed.html Unleashed](http://tomschrijvers.blogspot.com/2009/11/haskell-type-constraints-unleashed.html Unleashed).  For example, you can now say

  ```wiki
  type Stringy a = (Show a, Read a)
  f :: Stringy a => a -> a
  f = read . show
  ```

  Here, the constraint `(Stringy a)` is a synonym for `(Show a, Read a)`.  More importantly, by combining with assocaited types, we can write some fundamentally new kinds of programs:

  ```wiki
  class Coll c where
    type X a :: Constraint
    insert :: X a => a -> c a -> c a
  isntance Coll [] where
    type X a = Eq a
    insert x []  = [x]
    insert x (y:ys) | x==y      = ys 
                    | otherwise = y : insert x ys 
  ```

  Here `X` is an assocaited constraints synonmy of the class `Coll`. The key point is that differnet instances can give different definitions to `X`.  The GHC wiki page describese the design [ http://hackage.haskell.org/trac/ghc/wiki/KindFact WikiConstraint](http://hackage.haskell.org/trac/ghc/wiki/KindFact WikiConstraint), and Max's blog posts give more examples [ http://blog.omega-prime.co.uk/?p=61 ConstraintFamlies\], \[http://blog.omega-prime.co.uk/?p=127 ConstraintKind](http://blog.omega-prime.co.uk/?p=61 ConstraintFamlies], [http://blog.omega-prime.co.uk/?p=127 ConstraintKind).

- **Associated type synonym defaults.**  Haskell lets you give a *default method* for the operations of a class.
  Associated type synonym defaults let you declare a *default type instance* for the associated type synonyums of a class. This feature, implemented by Max Bolingbroke, nicely fills out missing design corner.  For example

  ```wiki
  class C a where
    type T a
    type T a = [a]  -- Default synonym
    f :: T a -> a
  instance C Int 
    f (x:xs) = x    -- No definition given for T
  ```

  Since we do not give a definition for `T` in the `instance` declaration, it filled in with the default given in the `class` declaration, just as if you had written `type T Int = [Int]`.

- **Monad comprehensions.**  After a long absence, monad comprehensions are back, thanks to George Giorgidze and his colleagues.  With `{-# LANGUAGE MonadComprehensions #-}` the comprehension `[ f x | x <- xs, x>4 ]` is intrepeted in an arbitrary monad, rather than being restricted to lists.  Not only that, but it generalises nicely for `then` and `group` forms too, and fully supports rebindable syntax.  See their paper "Bringing back monad comprehensions" [ http://www.pathfinder-xquery.org/files/giorgidze/haskell2011.pdf MonadComp](http://www.pathfinder-xquery.org/files/giorgidze/haskell2011.pdf MonadComp) at the 2012 Haskell Symposium.

- **Improvements to the implementation of type constraints.**  Over the last six months, Dimitrios and Simon PJ (with Stephanie Weirich and Brent Yorgey) have figured out several improvements to the GHC's type constraint solver and its strongly-typed Core language.  The changes to the constraint solver eliminate hundreds of lines of code, and make it more efficient as well.  The changes to the Core language make it treat equality constraints uniformly with other type constraints; this makes the internals vastly more uniform.  These changes are mostly invisible to programmers, but the changes to Core allow us to support equality superclasses for the first time.  Details in our paper "Practical aspects of evidence-based compilation in System FC" [ http://research.microsoft.com/\~simonpj/papers/ext-f/ NewFC](http://research.microsoft.com/~simonpj/papers/ext-f/ NewFC)

- **Profiling and hpc overhaul.** GHC currently has three different ways of tracking which pieces of code are executed: const-centre profiling, HPC coverage, and GHCi debugger breakpoints.  Each is implemented a different, anf somewhat *ad hoc* way.  Simon Marlow has overhauled the whole system, unifiying the three mechanisms into one.  On the way he has improved the semantics of cost centre stacks, which should lead to more useful time and space profiles.

- **Changes to the way Safe Haskell works** David Terei has improved the design of Safe Haskell since the 7.2.1 release. In particular, it will no longer cause build failures for users who do not explicitly enable it. *David to fill in*.

## Joining in


We continue to receive some fantastic help from a number of members from the Haskell community. Amongst those who have rolled up their sleeves recently are:

- Ben Gamari, Karel Gardas and Stephen Blackheath have been working towards getting a registerised Arm port working
- Many people, including Sergei Trofimovich, Erik de Castro Lopo, Joachim Breitner, Thorkil Naur, David M Peixotto and Ben Lippmeier, have contributed platform specific fixes for other platforms
- Reiner Pope added Template Haskell support for unresolved infix expressions and patterns
- Jose Pedro Magalhaes has replaced the old generics support with a new design
- Peter Wortmann taught GHC how to compile Objective-C++ files
- Sam Anklesaria added support for additional .ghci files
- Mikolaj Konarski and Duncan Coutts have improved GHC's event logging
- Geoffrey Mainland improved error messages for unterminated quasiquotations
- Johan Tibell implemented a "population count" primitive, and some other optimisations
- Ross Paterson has fixed some problems with Arrows
- Edward Z. Yang has been improving the RTS
- George Roldugin improved the sync-all tool used by GHC developers
- Austin Seipp has been improving some of the compiler documentation
- Miscellaneous fixes and improvements from Daniel Fischer, Michal Terepeta and Lennart Kolmodin


As ever, there is a lot still to do, and if you wait for us to do something then you may have to wait a long time. So don't wait; join in!

## Other developments


Work continues on improving GHC in various directions. Active projects we know about include:

### Cloud Haskell

**Jeff**

### Liquid types

**Ranjit**

### Parallel project

**Duncan**

### DPH

**Manuel**

## The glorious future


Also on the go, but not yet fully baked, are:

- Work on adding contracts to Hsakell, along the lines of Dana Xu's these, but using a first order logic theorem prover to check contract satisfaction (with Koen Claessen, Dimitrios Vytiniotis, Charles-Pierre Astolfi, and Nathan Collins).

- Extending the code generator to exploit vector instructions (with Peter Braam, Paul Monday, Duncan Coutts) [ http://hackage.haskell.org/trac/ghc/wiki/SimdLlvm VectorInstructions](http://hackage.haskell.org/trac/ghc/wiki/SimdLlvm VectorInstructions).

- A modular package language for Haskell (with Derek Dreyer and Scott Kilpatrick) [ http://hackage.haskell.org/trac/ghc/wiki/PackageLanguage Packages](http://hackage.haskell.org/trac/ghc/wiki/PackageLanguage Packages).

## Bibliography

- \[ConstraintFamilies\] *Constraint families*, Max Bolingbroke blog post, [ http://blog.omega-prime.co.uk/?p=61](http://blog.omega-prime.co.uk/?p=61)

- \[ConstraintKind\] "Constraint kinds for GHC", Max Bolingbroke blog post, [ http://blog.omega-prime.co.uk/?p=127](http://blog.omega-prime.co.uk/?p=127)

- \[WikiConstraint\] *Adding kind Constraint*, GHC wiki page, [ http://hackage.haskell.org/trac/ghc/wiki/KindFact](http://hackage.haskell.org/trac/ghc/wiki/KindFact)

- \[Unleashed\] "Haskell type constraints unleashed", Tom Schrijvers and Dominic Orchard, FLOPS 2010, [ http://tomschrijvers.blogspot.com/2009/11/haskell-type-constraints-unleashed.html](http://tomschrijvers.blogspot.com/2009/11/haskell-type-constraints-unleashed.html)

- \[KindPolymorphism\] *Giving Hasell a promotion*, Brent Yorgey, Stephanie Weirich, Julien Cretin, Dimitrios Vytiniotis, and Simon Peyton Jones, submitted to TLDI'12, [ http://research.microsoft.com/\~simonpj/papers/ext-f/](http://research.microsoft.com/~simonpj/papers/ext-f/)

- \[Cloud\] *Haskell for the cloud*, Epstein, Black, Peyton Jones, ICFP 2011, [ http://research.microsoft.com/\~simonpj/papers/parallel/](http://research.microsoft.com/~simonpj/papers/parallel/)

- \[MonadComp\] *Bringing back monad comprehenions*, George Giorgidze, Torsten Grust, Nils Schweinsberg, and Jeroen Weijers, Haskell Symposium 2011, [ http://www.pathfinder-xquery.org/files/giorgidze/haskell2011.pdf](http://www.pathfinder-xquery.org/files/giorgidze/haskell2011.pdf)

- \[!NewFC\] *Practical aspects of evidence-based compilation in System FC*, Vytiniotis and Peyton Jones, rejected by to ICFP 2011, [ http://research.microsoft.com/\~simonpj/papers/ext-f/](http://research.microsoft.com/~simonpj/papers/ext-f/)

- \[Packages\] *A package language for Haskell*, GHC wiki page, [ http://hackage.haskell.org/trac/ghc/wiki/PackageLanguage](http://hackage.haskell.org/trac/ghc/wiki/PackageLanguage)

- \[ParallelGhcProject\] *The Parallel GHC Project home page*, [ http://www.haskell.org/haskellwiki/Parallel_GHC_Project](http://www.haskell.org/haskellwiki/Parallel_GHC_Project)

- \[SHE\] *The Strathclyde Haskell Enhancement*, Conor McBride, 2010, [ http://personal.cis.strath.ac.uk/\~conor/pub/she/](http://personal.cis.strath.ac.uk/~conor/pub/she/)

- \[VectorInstructions\] *Using SIMD instructions via the LLVM back end*, GHC wiki page, [ http://hackage.haskell.org/trac/ghc/wiki/SimdLlvm](http://hackage.haskell.org/trac/ghc/wiki/SimdLlvm)