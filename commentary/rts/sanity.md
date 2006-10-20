CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary/Rts/Sanity"
  queryString          = "?version=1"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:58:02 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","259"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary/Rts/Sanity\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= Sanity Checking =

Source code: [[GhcFile(rts/Sanity.c)]], [[GhcFile(rts/Sanity.h)]].

The purpose of sanity checking is to catch bugs in the RTS as early as possible; if the program is going to crash, we want it to crash as soon as possible after the error occurred.  The problem with debugging the RTS is that heap corruption can go unnoticed through several GC cycles, making it particularly difficult to trace back to the erroneous code.

Sanity checking is turned on by the `+RTS -DS` option.  We treat it like an expensive assertion: normal assertions are allowed to take a few extra percent of runtime, so we don't mind having them on all the time in a `DEBUG` runtime, but sanity checking may double the run time of the program or worse.  So the rule of thumb is that expensive assertions go into sanity checking, cheap assertions are on in `DEBUG`, or possibly even on all the time.

Sanity checking does a complete traversal of the heap after each GC to look for dangling pointers (see `checkHeap` in [[GhcFile(rts/Sanity.c)]]).  For this it needs to ensure that there is no [wiki:Commentary/Rts/Storage/Slop slop], which is why we can only do this in a `DEBUG` runtime: the slop-avoiding machinery is only on with `DEBUG`.

Sanity checking also turns on some other expensive checks: for example in the generic apply code we check that the arguments point to valid closures.
```
