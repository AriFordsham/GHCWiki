CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/GhcPlanning"
  queryString          = "?version=12"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:59:18 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","252"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/GhcPlanning\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= Planning notes for GHC =

This page is an internal planning document, for Ian L, Simon M, and Simon PJ. It tracks the various things we'd like to get done, apart from the usual bug triage and release cycle.

== Current active mini-projects ==


Ian's projects

 * Add merging policy to WorkingConventions.

 * '''BuildBot''': Install on darcs.haskell.org and set up slaves
   * Set up Windows nightly build
   * Set up unregisterised nightly build?

 * '''Building libraries using Cabal''': Tidy up patches, do haddocking, test, then push.
   Don't worry about unreg way: we'll probably drop it anyway, and for nightly testing we
   can just do a full unreg build. Can't build the GHC package with --make due to
   a bug where GHC gets confused when as it learns more about a type as it compiles
   recursive modules: [ticket:930].
   * Look into whether we can do `SplitLibraries` with Cabal or not.

 * '''Mailing lists''':
   * Put procmail in front of mailman so we can allow big darcs patches through.
   * get sudo access on haskell.org
   * install spam-filtering technology
   * reoganise mailing lists: remove cvs-all, resubscribe everyone to the other lists
     (announce beforehand).

 * '''Broken tests'''
   Change `fail` to `broken(123)` and try to get the HEAD to a state where all
   test failures are broken and have a bug annotated. Update building guide
   to know about this change.

 * '''GHC API'''.  Respond to Norman Ramsey, Brian Smith, Clemens Fruhwirth.  Propose improvements to the GHC API.  

 * '''Ghc Performance Index''' (#1009).

 * '''Download statistics'''
 
 * '''Dynamic linking and shared libraries'''
   * GHCi seems to be working unregisterised
   * Make -fPIC work with the NCG on various arches
   * DLL/SO for RTS+Base libs.  Then lots of DLLs/SOs can share one RTS.
   * nofib -fPIC vs normal code on the common arches
   * nofib DLL vs static on the common arches
   * Doc updates
   * Write Wiki page describing GHCi linker
     * GHCi’s linker (.o files) vs system linker (.so and .dll only)
     * GHCi’s linker only works on 5-ish platforms.  

Simon PJ's projects
 * '''Implication constraints''' doc/tidying up
 * '''Demand analysis''' with Kirsten Chevalier
 * '''Associated data types and type synonyms''', with Manuel: [wiki:TypeFunctions]
 * '''Data parallel Haksell''', with Manuel, Gabi, Roman; see [http://haskell.org/haskellwiki/GHC/Data_Parallel_Haskell]

Simon M's projects
 * '''darcs''' Get darcs.h.o:~igloo/darcs/ installed
 * '''Parallel garbage collection'''

== Awaiting attention ==

This list intended to be in priority order (but of course the prorities might not be right!

 * '''Windows installers'''. Want to help Neil get going (see also #604).


```
