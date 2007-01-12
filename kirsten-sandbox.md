CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/KirstenSandbox"
  queryString          = "?version=35"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 07:00:49 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","254"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/KirstenSandbox\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= Kirsten's to-do list =

== Demand analysis ==

 1. Get code ready to check in:
   1. cleanup
   1. better comments (i.e., examples for things that may not work right)
   1. testing

 1. Update Commentary 

 1. Experiments:
   1. comparison with old strictness analyzer
   1. see whether new optimizations are helping
   1. remove special cases for {{{build}}} and see what happens

 1. Paper

 1. Modify worker/wrapper split
   1. so as to exploit the new analysis information

= Old =

KirstenSandbox/GhcOldMac - building GHC on Mac OS 10.2.1 (the summary: don't do it.)]
```
