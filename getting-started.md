CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/GettingStarted"
  queryString          = "?version=2"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:59:01 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","253"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/GettingStarted\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= Getting Started =

How to get started with hacking on GHC:

 * [wiki:Building/GettingTheSources Grab the latest sources]
 * [wiki:Building/Hacking Set up your build tree]
 * Pick an easy [query:?status=new&status=assigned&status=reopened&type=bug&order=priority&group=difficulty  bug report] or [query:?status=new&status=assigned&status=reopened&type=task&order=priority&group=difficulty  task] to work on
 * Useful information about GHC's architecture is in the [wiki:Commentary]
 * Hack away... ask questions on [http://haskell.org/mailman/listinfo/cvs-ghc the cvs-ghc mailing list]
 * [wiki:WorkingConventions#Submittingpatches Submit patches back], using {{{darcs send}}} is the recommended way.

```
