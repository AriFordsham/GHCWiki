CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary/Rts/Storage"
  queryString          = "?version=9"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:58:00 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","260"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary/Rts/Storage\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac


= GHC Commentary: The Storage Manager =

GHC's storage manager is designed to be quite flexible: there are a large number of tunable parameters in the garbage collector, and partly the reason for this was because we wanted to experiment with tweaking these settings in the context of Haskell.

[[Image(sm-top.png)]]

 * [wiki:Commentary/Rts/Storage/HeapObjects Layout of Heap Objects]
 * [wiki:Commentary/Rts/Storage/Stack Layout of the Stack]
 * [wiki:Commentary/Rts/Storage/Slop Slop]
 * [wiki:Commentary/Rts/Storage/BlockAlloc The Block Allocator]
 * [wiki:Commentary/Rts/Storage/GC The Garbage Collector]
 * [wiki:Commentary/Rts/Storage/CAFs Garbage Collecting CAFs]

```
