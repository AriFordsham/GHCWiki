CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary/Compiler/API"
  queryString          = "?version=4"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:55:11 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","259"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary/Compiler/API\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac


= GHC Commentary: The GHC API =

This section of the commentary describes everything between [wiki:Commentary/Compiler/HscMain HscMain] and the front-end; that is, the parts of GHC that coordinate the compilation of multiple modules.

== Orgainsation of the top of GHC ==

[[Image(ghc-top.png)]]

The GHC API is the interface exported by [[GhcFile(compiler/main/GHC.hs)]].  To compile a Haskell module that uses the GHC API, use the flag {{{-package ghc}}} (in GHC 6.6 and later).  GHC itself contains a few front-ends:

 * The "one-shot" mode, where GHC compiles each file on the command line separately (eg. {{{ghc -c Foo.hs}}}).  This mode
   is implemented directly on top of [wiki:Commentary/Compiler/HscMain HscMain], since it compiles only one file at a
   time.  In fact, this is all that GHC consisted of prior to version 5.00 when GHCi and {{{--make}}} were introduced.

 * GHCi, the interactive environment, is implemented in [[GhcFile(compiler/ghci/InteractiveUI.hs)]] and sits squarely on top
   of the GHC API.

 * {{{--make}}} is almost a trivial client of the GHC API, and is implemented in [[GhcFile(compiler/main/Main.hs)]].

 * {{{-M}}}, the Makefile dependency generator, is also a client of the GHC API and is implemented in 
   [[GhcFile(compiler/main/DriverMkDepend.hs)]].

Note that since GHC is packaged as a single binary, all of these front-ends are present, and there is a single command-line API.  Everything goes via the {{{main}}} function in [[GhcFile(compiler/main/Main.hs)]].


```
