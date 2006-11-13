CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Building"
  queryString          = "?version=14"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:59:03 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","250"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Building\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac


= Building and Porting GHC =

This Guide is primarily aimed at those who want to build and/or
hack on GHC.  It describes how to get started with building GHC on your
machine, and how to tweak the settings to get the kind of build you
want.  It also describes the inner workings of the build system, so you
can extend it, modify it, and use it to build your code.

The documentation here will eventually replace the
[http://www.haskell.org/ghc/docs/latest/html/building/index.html Building Guide].  Text
from that manual is gradually being incorporated here.

== Contents ==

 * [wiki:Building/GettingTheSources Getting the sources]
 * [wiki:Platforms Platforms that GHC currently supports]
 * [wiki:Building/Prerequisites What tools you need]
 * [wiki:Building/QuickStart Quick start: just building and installing GHC]
 * [wiki:Building/Hacking Quick start for developers]
 * [wiki:Building/Unregisterised Unregisterised builds]
 * [wiki:Building/Rebuilding How do I re-build after updating or changing GHC?]
 * [wiki:Building/Using Using the build system]
 * [wiki:Building/BuildSystem Architecture of the build system]
 * [wiki:Building/Docs Building the documentation]
 * [wiki:Building/Porting Porting GHC]
 * [wiki:Building/KnownProblems Known pitfalls in building GHC]
 * [wiki:Building/RunningTests Running the GHC test framework]
 * [wiki:Building/RunningNoFib The NoFib benchmark suite]
 * Platform-specific build issues
   * [wiki:Building/PlatformsScriptsFileNames Platforms, scripts, and file names]
   * [wiki:Building/Windows Building under Windows]
   * [wiki:Building/MacOSX Building under MacOS X]

== Contributed documentation ==

Please feel free to add pages here.  In due course, information can be incorporated into the main documentation above.

 * [wiki:ProblemsCompilingGhc]: Hints about building GHC on Windows platforms  

== OLD documentation ==

  * [http://www.haskell.org/ghc/docs/latest/html/building/index.html Building Guide]
```
