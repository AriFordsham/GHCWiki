CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/HackathonProjects"
  queryString          = "?version=17"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:56:47 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","256"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/HackathonProjects\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= Project suggestions for the 2006 GHC hackathon =

See also FridayIdeas - someone should combine these lists :)

Add your suggestions for the hackathon below...

== GHC Projects ==

 * Hack to allow RTS to integrate with an external event loop (eg to give us ideal threading when using Gtk+)

 * Add a {{{ghc --clean}}} that just executes {{{find -name '*.o' -o -name '*.hi' -exec rm {} \;}}}, perhaps in a more portable fashion

 * Further work on debugging in GHCi

 * Improve the native code generator: see BackEndNotes

 * Get the front panel working again (#599)

 * WARNING pragma (#657)

 * Warning suppression (#602)

 * Accept more encodings for source code (#690)

 * Use gcc's libffi to implement Adjustor.c & ByteCodeFFI

 * Add :edit support to GHCi (#95)

 * Option to allow compiling from within GHCi (#276)

 * GHC plugins: allow passes to be loaded at runtime from plugins

 * Get shared libraries or DLLs working

 * Build a Windows-native version of GHC (using MS tools instead of gcc).

 * GHC API clients: hstags, code metrics (eg. feature use)

 * Implemene the static argument transformation (#888)

 * Whole-program dead-code detection (with {{{--make}}}).

 * Whole-program overloading elimination (with {{{--make}}}).
 
 * Work on Haddock on top of the GHC API (see [http://www.haskell.org/pipermail/haskell/2006-August/018415.html this message]).

 * {{{-Ofile}}}: take the list of optimisation passes to run from a file instead of current hard-wired sequence.

 * {{{-fmonad-comprehension}}}: Reuse existing list comprehension code to restore it for arbitrary monads.

 * Improve the profiler (longer stacks reported while heap profiling, for example)
 
 * Add type parameter to [wiki:Commentary/Compiler/HsSynType HsSyn] for the post-typechecking type to ensure it does not exist until after typechecking

 * Improve pretty-printer for Core to have better layout -- Geoff Washburn may look at this. 

 * Improve instance deriving: allow deriving Enum for more types, allow deriving Eq, Ord, Show for GADTs. -- Bjorn Bringert

 * Hacking the build system (for example)
 * Setting up GHC to build the base library using Cabal (since you have the GHC team, the Cabal team and the hmake team in one place!)
   * Isaac Jones - I can be available for general Cabal questions as well. 
 * Hacking on Cabal-Get (not really a GHC thing!)
   * Isaac Jones 
 * Fix C-- (cmm) output?

== Library Projects ==

 * Work on the Streams library

 * Replace GMP with OpenSSL mp library (#601)

 * A binding for your favourite C/C++ library...  (eg. GStreamer?)

== Cabal Projects ==

 * Cabalisation of the GHC library (#654)

 * Put two solid days of work into getting cabal-get finished and stable.  Additionally, put as many packages into the DB as possible.

== More Project Suggestions ==

 * Generate Windows installers automatically from Cabal packages (or
   other OS-specific package format, eg. RPM)

 * Any of the [query:?status=new&status=assigned&status=reopened&type=bug&order=priority bugs], [query:?status=new&status=assigned&status=reopened&type=task&order=priority&group=difficulty tasks], or [query:?status=new&status=assigned&status=reopened&type=feature+request&order=priority feature requests].

  * Guarantee that .hi files and .o files are consistent, possibly by means of some sort of digital signature that would cause a link-time failure

  * A visualizer that would allow one to step forward and backward through the transformations that the compiler is doing (instead of crawling through endless outputs of {{{-ddump-*}}}).

```
