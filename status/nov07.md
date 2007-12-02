# GHC Status November 2007


Lots has happened on the GHC front over the last few months.  We
released GHC 6.8.1 on 3 November 2007.  GHC now has so many users, and
such a large feature "surface area" that simply getting to the point
where we can make a release is becoming quite a challenge.  Indeed, a
good deal of our effort in the last six months has been in the form of
consolidation: fixing bugs and solidifying what we have.  

[](/trac/ghc/attachment/wiki/Status/Nov07/rolling_average.png)

[](/trac/ghc/attachment/wiki/Status/Nov07/totals.png)


The major new features of 6.8.1 were described in the last issue
of the Haskell Communities Newsletter, so we won't repeat them here.
Instead, here are some of the highlights of what we are working on now.

## Syntactic and front-end enhancements


Several people have developed superficial but perhaps-very-useful
syntactic innovations, which now form part of the HEAD:

- Three improvements to records

  - **Wild-card patterns for records**.  If you have

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
  - **Record puns** is a slightly less abbreviated approach. You can write 'f' like this:

    ```wiki
     	f (MkT {x,y}) = x+y
    ```

    whereas Haskell 98 requires you to write "x=x,y=y" in the pattern. Similarly 
    in record construction.
  - **Record field disambiguation** is useful when there are several types in
    scope, all with the same field name.  For example, suppose another data type S
    had an 'x' field. Then if you write

    ```wiki
     	h (MkT {x=p,y=q}) = ...
    ```

    there is no doubt which 'x' you mean, but Haskell 98 will complain that there
    are two 'x's in scope.  Record field disambiguation just uses the constructor
    to decide which 'x' you mus mean.

> > >
> > > None of these changes tackle the deeper issue of whether or not
> > > Haskell's current approach to records is the Right Way; rather the
> > > changes just make the current approach work a bit better. 
> > > Furthermore, they are all somewhat controversial, because they make it
> > > harder to see where something comes into scope.  Let's see what you think!

- **View patterns** are implemented, by Dan Licata. Here's a simple example:

  ```wiki
  	polar :: Complex -> (Float, Float)
  	polar = ...
  	
   	f :: Complex -> Bool
  	f (polar -> (r,theta)) = r <= 1
  ```

  Here 'polar' is an ordinary function, used to transform the Complex to polar form.  The view pattern is the argument pattern for 'f'.  Many details [ here](http://hackage.haskell.org/trac/ghc/wiki/ViewPatterns).

- **Generalised list comprehensions** (see [ Comprehensive comprehensions: comprehensions with "Order by" and "Group by", Phil Wadler and Simon Peyton Jones, Haskell Workshop 2007](http://research.microsoft.com/%7Esimonpj/papers/list-comp/index.htm)) have been implemented by Max Bolinbroke.  Example:

  ```wiki
  	[ (the dept, sum salary)
  	| (name, dept, salary) <- employees
  	, then sortWith by salary
  	, then takeWhile by salary < 50
  	, then take 5 ]
  ```

> >
> > More details [ here](http://hackage.haskell.org/trac/ghc/wiki/SQLLikeComprehensions).

- We are keen to get Geoff Mainland's **quasi-quoting mechanism** into
  GHC (see "Why It's Nice to be Quoted: Quasiquoting for Haskell",
  Geoffrey Mainland. Haskell Workshop 2007).  Geoff is working on 
  polishing it up.

## Type system stuff

- I'll write more about type equalities

## Back end stuff

- Ben Lippmeir spent his internship building a graph-colouring,
  coalescing register allocator for GHC's native code generator.

> >
> > SIMON SAY MORE

- Michael Adams came for an internship and built a CPS converter
  for GHC's internal C-- data type.  He had barely left when Norman
  Ramsey arrived for a short sabbatical.  Based on his experience of
  building back ends for the Quick C-- compiler, he worked on a new
  zipper-based data structure to represent C-- code, and a sophisticated
  dataflow framework so that you can write new dataflow analyses in 
  30 mins.  


As a result, we now have \*lots\* of new code.  Some of it is working;
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


Increasingly, therefore, we are trying to un-couple GHC from big
libraries.  We ship GHC with a set of "core" libraries, without which
GHC will not function at all, and "extra" libraries, which just happen
to come with GHC, and which can be upgraded separately at any time.
To further that end, we've split the "base" package into a bunch of
smaller packages.  This has led to lots of pain, because old programs
that depended on 'base' now need to depend on other packages too.  But
it's good pain, and matters should improve too as Cabal matures.


More detail here: XXX
