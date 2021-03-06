# GHC Status November 2007


Lots has happened on the GHC front over the last few months.  We
released GHC 6.8.1 on 3 November 2007.  GHC now has so many users, and
such a large feature "surface area", that simply getting to the point
where we can make a release is becoming quite a challenge.  Indeed, a
good deal of our effort in the last six months has been in the form of
consolidation: fixing bugs and solidifying what we have.  


These graphs show "tickets" which include bugs, feature requests, and tasks.  Of the "open tickets", about half are bugs.  Notice the big spike in "closed tickets" just before the 6.8.1 release!

![](nov07/rolling_average.png)

![](nov07/totals.png)


The major new features of 6.8.1 were described in the last issue
of the Haskell Communities Newsletter, so we won't repeat them here.
Instead, here are some of the highlights of what we are working on now.

## Syntactic and front-end enhancements


Several people have developed syntactic innovations, which are (or will shortly be) in the HEAD:

- **Three improvements to records**

  - *Wild-card patterns for records*.  If you have

    ```wiki
    	data T = MkT {x,y::Int, z::Bool}
    ```

    then you can say

    ```wiki
    	f :: T -> Int
    	f (MkT {..}) = x+y

    	g :: Int -> Int -> T
    	g x y = MkT {..}
    	      where
    		z = x>y
    ```

    The ".." in a pattern brings into scope all the fields of the
    record; while in a record construction it uses variables with
    those names to initialise the record fields. Here's the [user manual entry](http://www.haskell.org/ghc/dist/current/docs/users_guide/syntax-extns.html#record-wildcards)
  - *Record puns* is a slightly less abbreviated approach. You can write 'f' like this:

    ```wiki
     	f (MkT {x,y}) = x+y
    ```

    whereas Haskell 98 requires you to write "x=x,y=y" in the pattern. Similarly 
    in record construction.
  - *Record field disambiguation* is useful when there are several types in
    scope, all with the same field name.  For example, suppose another data type S
    had an 'x' field. Then if you write

    ```wiki
     	h (MkT {x=p,y=q}) = ...
    ```

    there is no doubt which 'x' you mean, but Haskell 98 will complain that there
    are two 'x's in scope.  Record field disambiguation just uses the constructor
    to decide which 'x' you mus mean.

>
> >
> > >
> > >
> > > None of these changes tackle the deeper issue of whether or not
> > > Haskell's current approach to records is the Right Way; rather the
> > > changes just make the current approach work a bit better. 
> > > Furthermore, they are all somewhat controversial, because they make it
> > > harder to see where something comes into scope.  Let's see what you think!
> > >
> > >
> >
>

- **View patterns** are implemented, by Dan Licata. Here's a simple example:

  ```wiki
  	polar :: Complex -> (Float, Float)
  	polar = ...
  	
   	f :: Complex -> Bool
  	f (polar -> (r,theta)) = r <= 1
  ```

  Here 'polar' is an ordinary function, used to transform the Complex to polar form.  The view pattern is the argument pattern for 'f'.  Many details [here](http://hackage.haskell.org/trac/ghc/wiki/ViewPatterns).

- **Generalised list comprehensions** (see [Comprehensive comprehensions: comprehensions with "Order by" and "Group by", Phil Wadler and Simon Peyton Jones, Haskell Workshop 2007](http://research.microsoft.com/%7Esimonpj/papers/list-comp/index.htm)) have been implemented by Max Bolinbroke.  Example:

  ```wiki
  	[(the dept, sum salary)
  	| (name, dept, salary) <- employees
  	, then sortWith by salary
  	, then takeWhile by salary < 50
  	, then take 5 ]
  ```

>
> >
> >
> > More details [here](http://hackage.haskell.org/trac/ghc/wiki/SQLLikeComprehensions).
> >
> >
>


        


- We are keen to get Geoff Mainland's **quasi-quoting mechanism** into
  GHC (see "Why It's Nice to be Quoted: Quasiquoting for Haskell",
  Geoffrey Mainland. Haskell Workshop 2007).  Geoff is working on 
  polishing it up.

## Type system stuff


The big innovation in GHC's type system has been the gradual
introduction of indexed type families in the surface syntax, and of
type equalities in the internal machinery.  


Indexed data families (called "associated data types" when
declared in type classes) are fairly simple, and they work fine
in GHC 6.8.1.  Indexed type families (aka "associated type synonyms")
are a different kettle of fish, especially when combined with
the ability to mention type equalities in overloaded types, thus:

```wiki
  f :: forall a b. (a ~ [b]) => ...
```


Tom Schrijvers spent three months at Cambridge, working on
the theory and implementation of a type inference algorithm. As 
a result we have a partially-working implementation, and we 
understand the problem much better, but there is still much to
do, both on the theoretical and practical front.  It's trickier
than we thought!  We have a short paper [Towards open type functions for Haskell](http://research.microsoft.com/%7Esimonpj/papers/assoc-types/index.htm) which
describes some of the issues, and an [wiki page](http://hackage.haskell.org/trac/ghc/wiki/TypeFunctions) that we keep up to date; it has a link to details of implementation status.  This is all joint work with
Martin Sulzmann, Manuel Chakravarty, and Tom Schrijvers.


## Parallel GC


Since 6.6 GHC has had support for running parallel Haskell on a multi-processor out of the box.  However, the main drawback has been that the garbage collector is still single-threaded and stop-the-world.  Since GC can commonly account for 30% of runtime (depending on the GC settings), this can seriously put a crimp in your parallel speedup.


Roshan James did an internship at MSR in 2006 during which he and Simon M worked on parallelising the major collections in GHC's generational garbage collector.  We had a working algorithm, but didn't observe much speedup on a multi-processor.  Since then, Simon rewrote the implementation and spent a large amount of time with various profiling tools, which uncovered some cache-unfriendly behaviour.  We are now seeing some speedup, but there is more tweaking and measuring still to be done.


This parallel GC is likely to be in GHC 6.10.  Note that parallel GC is independent of whether the Haskell program itself is parallel - so even single-threaded Haskell programs (e.g. GHC itself) should benefit from it.


The other side of the coin is to parallelise the *minor* collections.  These are normally too small and quick to apply the full-scale parallel GC to, and yet the whole system still has to stop to perform a minor GC.  The solution is almost certainly to allow each CPU to GC its own nursery independently.  There is existing research describing how to do this, and we plan to try applying it in context of GHC.

## Data parallel Haskell


After many month of designing, re-designing, and finally implementing a vectorisation pass operating on GHC's Core intermediate language, we finally have a complete path from nested data parallel array programs to the low-level, multi-threaded array library in package ndp.  We are very excited about having reached this milestone, but the path is currently very thin, complete unoptimised, and requires a special Prelude mockup.   More work is required before vectorisation is ready for end-users, but now that the core infrastructure is in place, we expect more rapid progress on user-visible features.


Besides working on optimisations and completing the backend library, we still need to implement [Partial Vectorisation of Haskell Programs](http://www.cse.unsw.edu.au/~chak/papers/CLPK07.html) and the treatment of unboxed types, which is crucial to vectorise the standard Prelude.  Most of the code was written by Roman Leshchinskiy.

## Back end stuff


GHC's back end code generator has long been known to generate poor code, particularly 
for tight loops of the kind that are cropping up more and more in highly optimised
Haskell code.  So in typical GHC style, rather than patch the immediate problem, we're redesigning
the entire back end.


What we want to do:

- Split the STG-to-C-- code generator (`codeGen`) into two: one pass
  generating C-- with functions and calls, and a second pass ("CPS") to 
  manifest the stack and calling/return conventions.

- Redesign the calling and return conventions, so that we can use more
  registers for parameter passing (this will entail decommissioning the
  via-C code generator, but the native code generator will outperform it).

- Give the back end more opportunity to do low-level transformation and
  optimisation, e.g. by exposing loops at the C-- level.

- Implement more optimisations over C--.

- Plug in a better register allocator.


What we've done so far:

- Michael Adams came for an internship and built a CPS converter
  for GHC's internal C-- data type.  

- He had barely left when Norman Ramsey arrived for a short sabbatical.  Based on his experience of building back ends for the Quick C-- compiler, he worked on a new zipper-based data structure to represent C-- code, and a sophisticated dataflow framework so that you can write new dataflow analyses in 30 mins.  

- Ben Lippmeir spent his internship building a graph-colouring,
  coalescing register allocator for GHC's native code generator.


As a result, we now have *lots* of new code.  Some of it is working;
much of it is as yet un-integrated and un-tested.  However, once we
have it all glued back together, GHC will become a place where you can
do Real Work on low-level optimisations, and code generation.  Indeed
John Dias (one of Norman's graduate students) will spend six months here
in 2008 to do work on code generation.


In short, GHC's back end, which has long been a poor relation, is
getting a lot of very sophisticated attention. Expect good things.

## Libraries


GHC ships with a big bunch of libraries.  That is good for users, but
it has two bad consequences, both of which are getting worse with
time.  First, it make it much harder to get a release together,
because we have to test more and more libraries too.  Second, it's
harder (or perhaps impossible) to upgrade the libraries independently
from GHC.  There's a meta-issue too: it forces us into a gate-keeper
role in which a library gets a big boost by being in the "blessed set"
shipped with GHC.


Increasingly, therefore, we are trying to decouple GHC from big
libraries.  We ship GHC with a set of "boot" libraries, without which
GHC will not function at all, and "extra" libraries, which just happen
to come with some binary distributions of GHC, and which can be upgraded
separately at any time.
To further that end, we've split the "base" package into a bunch of
smaller packages, and expect to further split it up for GHC 6.10.
This has led to lots of pain, because old programs
that depended on 'base' now need to depend on other packages too;
see [upgrading packages](http://www.haskell.org/haskellwiki/Upgrading_packages) for details.  But
it's good pain, and matters should improve too as Cabal matures.  We have been exploring possibilities for [lessening the pain](commentary/packages/package-compatibility-proposal) in 6.10.  We
have also devised a
[package versioning policy](http://www.haskell.org/haskellwiki/Package_versioning_policy)
which will help future library upgrades.  
