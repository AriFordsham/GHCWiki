CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary/Packages/PackageReorg"
  queryString          = "?version=3"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:59:22 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","262"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary/Packages/PackageReorg\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= Package Reorg =

In this page we collect proposals and design discussion for
reorganising the packages that come with compilers, and the contents
of those packages.

None of the ideas herein are claimed to belong to any particular
person, many of the ideas have been extracted from mailing list
discussions, eg.

 [http://www.haskell.org/pipermail/libraries/2006-November/006396.html]

Some of the points are GHC-specific.  Please feel free to insert
points specific to other compilers.

== Goals ==

 * It would be good to have set of packages that is installed with
   every Haskell implementation.  This seems to be Bulat's main point
   in the thread above.  Let's call these the '''Core Packages''' to
   avoid confusion (Bulat called these the "base packages").
   The good thing about the Core Packages is that
   users know that they will be there, and they are consistent with
   each other.

 * Any particular implementation may install more packages by default;
   for example GHC will install the `template-haskell` and `stm`
   packages.  Let's call these the '''GHC Install Packages''', '''Hugs
   Install Packages''' etc; the Install Packages are a superset of the
   Core Packages.

 * It should be possible to upgrade any package, even if that package
   came with the compiler.

== What is in the Core Packages? ==

The Core Packages are installed with every conforming Haskell implementation.

Here's an initial stab

  * `base`
  * `Cabal`
  * `haskell98`
  * Some `regex` packages (precisely which?)
  * `unix` or `Win32`
  * `parsec`
  * `mtl`
  * `time`
  * `network`

Questionable:
  * `QuickCheck`
  * `HUnit`

== The base package ==

The base package is a bit special

 * Package `base` is rather big at the moment.  

 * From a user's point of view it would be nicer to give it a
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
 * Data.!ByteString.* (plus future packed Char strings)
 * Control.Applicative (?), Data.Foldable, Data.Monoid (?), Data.Traversable, Data.Graph, Data.!IntMap, Data.!IntSet, Data.Map, Data.Sequence, Data.Set, Data.Tree
 * System.Console.!GetOpt
 * Text.!PrettyPrint.*
 * Text.Printf
Some other things, such as arrays and concurrency, have nothing else depending on them, but are so closely coupled with GHC's internals that extracting them would require exposing these internals in the interface of `base`.

== Other packages ==

Other non-core packages would probably have their own existence.  That
is, they don't come with an implementation; instead you use
`cabal-get`, or some other mechanism, such as your OS's package
manager.  Some of these currently come with GHC, and would no longer do
so

  * `GLUT`
  * `ALUT`
  * `OpenAL`
  * `OpenGL`
  * `HGL`
  * `HUnit`
  * `ObjectIO`
  * `X11`
  * `arrows`
  * `cgi`
  * `fgl`
  * `html`
  * `xhtml`

== Testing ==

We should separate out package-specifc tests, which should be part of
the repository for each package.  Currently they are all squashed
together into the testsuite repository.

== Notes about GHC ==

Currently GHC installs a set of packages by default: base, stm,
template-haskell, cabal, haskel98, readline, 3 of the 5 regex
packages.  These are exactly the libraries required to build GHC.
That shouldn't be the criterion.  This set of packages are currently
called GHC's "core packages", but should be renamed to '''GHC Boot
Packages'''.

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
 * `template-haskell`
 * `stm`
 * `readline`

You can upgrade any package, including `base` after installing GHC.
However, you need to take care. You must not change a number of things
that GHC "knows about".  In particular, these things must not change
 * Name
 * Defining module
GHC knows even more about some things, where you must not change
 * Type signature
 * For data types, the names, types, and order of the constructors
The latter group are confined to packages base and template-haskell.

(Note: a few other packages are used by tests in GHC's test suite,
currently: `mtl`, `QuickCheck`.  We should probably eliminate the mtl
depedency; but `QuickCheck` is used as part of the test infrastructure
itself, so we'll make it a GHC Boot Package.)

```
