CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/DarcsRepositories"
  queryString          = "?version=1"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:57:53 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","255"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/DarcsRepositories\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
[[PageOutline]]

= GHC Darcs Repositories =

This page lists the active darcs repositories relating to GHC.  For instructions on actually getting a GHC source tree, see [wiki:Building/GettingTheSources].

== The HEAD ==

These darcs repositories are the HEAD (main trunk) of GHC development:

||[http://darcs.haskell.org/ghc]||The main GHC repository||
||[http://darcs.haskell.org/testsuite]||The test suite (requires python 2.4+) ||
||[http://darcs.haskell.org/nofib]||The benchmark suite||

The following repositories are the "core" packages, that populate the libraries directory of a GHC tree:

||http://darcs.haskell.org/packages/base||
||http://darcs.haskell.org/packages/Cabal||
||http://darcs.haskell.org/packages/haskell98||
||http://darcs.haskell.org/packages/readline||
||http://darcs.haskell.org/packages/regex-base||
||http://darcs.haskell.org/packages/regex-posix||
||http://darcs.haskell.org/packages/regex-compat||
||http://darcs.haskell.org/packages/stm||
||http://darcs.haskell.org/packages/template-haskell||
||http://darcs.haskell.org/packages/unix||
||http://darcs.haskell.org/packages/Win32||

And the following repositories are the "extra" packages:

||http://darcs.haskell.org/packages/ALUT||
||http://darcs.haskell.org/packages/GLUT||
||http://darcs.haskell.org/packages/HGL||
||http://darcs.haskell.org/packages/HUnit||
||http://darcs.haskell.org/packages/HaXml||
||http://darcs.haskell.org/packages/Japi||
||http://darcs.haskell.org/packages/ObjectIO||
||http://darcs.haskell.org/packages/OpenAL||
||http://darcs.haskell.org/packages/OpenGL||
||http://darcs.haskell.org/packages/QuickCheck||
||http://darcs.haskell.org/packages/X11||
||http://darcs.haskell.org/packages/arrows||
||http://darcs.haskell.org/packages/fgl||
||http://darcs.haskell.org/packages/haskell-src||
||http://darcs.haskell.org/packages/html||
||http://darcs.haskell.org/packages/monads||
||http://darcs.haskell.org/packages/mtl||
||http://darcs.haskell.org/packages/network||
||http://darcs.haskell.org/packages/parsec||
||http://darcs.haskell.org/packages/time||
||http://darcs.haskell.org/packages/xhtml||

== Branches ==

The following branches are active:

 '''6.6 Branch'''::
  Prepend `ghc-6.6` to the name of the repository to get the 6.6 branch.  For example,
  the 6.6 ghc repository is at [http://darcs.haskell.org/ghc-6.6/ghc].
  [[br]][[br]]
  Note: only the main repositories and the core libraries were branched for 6.6, the
  extralibs packages are not found under `ghc-6.6`.  The `darcs-all` script knows
  where to get everything, so you don't have to worry about this, just follow the
  instructions in [wiki:Building/GettingTheSources].


```
