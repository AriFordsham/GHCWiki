CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary/Organisation"
  queryString          = "?version=1"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:54:29 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","259"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary/Organisation\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= Overall organisation of GHC =

Start at the [http://haskell.org/ghc GHC home page].  The most important links are
in the left-hand column:

* [http://haskell.org/haskellwiki/GHC Documentation].  This is the ''user'' documentation, aimed at people who use GHC, but don't care how it works.  It's on the Haskell Wiki (powered by MediaWiki), and we strongly encourage people to edit and improve it.

* [http://hackage.haskell.org/trac/ghc Developers].  This link takes you to the home page for ''developers''; that is, people interested in hacking on GHC itself (i.e. you).  It's a Wiki too, but powered by Trac, and includes bug-tracking etc.

* [http://www.haskell.org/ghc/download.html Download].  At any moment, GHC has a '''STABLE branch''' and the '''HEAD'''.
  * The HEAD is simply the latest, greatest version that we are working on; it may be broken on any given day, although youa ar
```
