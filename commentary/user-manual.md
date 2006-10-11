CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary/UserManual"
  queryString          = "?version=1"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:57:34 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","259"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary/UserManual\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac


= The user manual =

GHC's user manual contains documentation intended for users of GHC.  They are not interested in how GHC works; they just want to use it.

The user manual is held in [[GhcFile(docs/user_guide)]], and is written in !DocBook format (.xml files).  This allows us to typeset it as HTML pages, or as Latex.

See also the [wiki:Building/Docs notes on building the documentation].

Notes on formatting:
 * Cross-links to Haddock library documtation for module `A.B` in package `foo` should be wrtten thus:
{{{
 <ulink url="../libraries/foo/A-B.html"> visible link text </ulink>
}}}
 This relative path will link correctly when the user manual is installed locally, or on GHC's home page. Unfortunately, it won't link correctly in an in-place tree, but that's too bad.

```
