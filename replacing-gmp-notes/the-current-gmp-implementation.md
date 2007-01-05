CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/ReplacingGMPNotes/TheCurrentGMPImplementation"
  queryString          = "?version=1"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 07:00:27 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","275"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/ReplacingGMPNotes/TheCurrentGMPImplementation\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac


= The Current GMP Implementation =

Esa Ilari Vuokko, who at one time attempted to replace GMP with [http://math.libtomcrypt.com/ LibTomMath], posted several messages with good notes on the current implementation.  Much of what is on this page is derived from those notes.  See, [http://www.haskell.org/pipermail/glasgow-haskell-users/2006-August/010669.html Replacement for GMP(3)] and [http://www.haskell.org/pipermail/glasgow-haskell-users/2006-August/010674.html Replacement for GMP(4)].


```
