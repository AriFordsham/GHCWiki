CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/FridayIdeas"
  queryString          = "?version=23"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:56:36 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","251"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/FridayIdeas\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= How Should We Spend Friday? =

== Projects ==
Add projects ideas you have, and add your name if you're interested in working on them.

 * Hacking the build system (for example)
 * Setting up GHC to build the base library using Cabal (since you have the GHC team, the Cabal team and the hmake team in one place!)
   * Isaac Jones - I can be available for general Cabal questions as well.
 * Hacking on Cabal-Get (not really a GHC thing!)
   * Isaac Jones
 * Fix C-- (cmm) output?
 * Discussing proposed changes to backend and code generation
 * Discussing dynamic linking
 * Strategies in debugging GHC and RTS.
 * Improve instance deriving: allow deriving Enum for more types, allow deriving Eq, Ord, Show for GADTs. -- Bjorn Bringert
 * A walkthrough of GHC's intermediate outputs and how they are related to the various stages charted in http://cvs.haskell.org/trac/ghc/wiki/Commentary/Compiler/HscMain

== General Organization ==
The above are specific projects or subgroups that might form.  What about more generally?  How should we organise the day?

 * just let everyone hack on individual projects, with floating Simons to help. --- I'd like that.
 * form subgroups to work together on shared projects
   * Project ideas here?
   * Resuscitate ExternalCore
   * Low hanging optimizations?
   * STGLint
   * Rewrite the Evil Mangler in Haskell?
   * GHC RTS in Cyclone?
   * Work on GHC API, I would like to use it to provide feedback and interactivity to programmers
   * Hook a multithreaded memory manager into the RTS
 * have more explanatory talk sessions led from the front
 * form subgroups for explanatory sessions on different topics (but not to work on a shared project)
 * work on fleshing out the wiki/commentary?
 * ???

== Seeking Enlightenment ==

 * Space leaks
   * short example posted to cafe by Rodrigues ([http://haskell.org/pipermail/haskell-cafe/2006-June/016171.html], [http://haskell.org/pipermail/haskell-cafe/2006-July/016709.html])
```
