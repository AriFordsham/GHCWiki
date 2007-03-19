CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/DataParallel/WorkPlan"
  queryString          = "?version=1"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 07:01:58 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","258"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/DataParallel/WorkPlan\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
== Work plan for implementing Data Parallel Haskell ==

Major milestones:

 '''Milestone 1:''' basic SMP library (End of Feb 2007)::
   Implementation of the central parts of an array library of flat and segmented arrays that uses distributed types to partition work on an SMP thread gang and uses fusion to eliminate superflous join points and intermediate arrays.
 '''Milestone 2:''' basic vectorisation (End of Aug 2007)::
  Implementation of the vectorisation transformation and basic interaction of vectorised and non-vectorised code to provide (including the results from Milestone 1) a complete path from source programs to parallel executable for simple examples.
 '''Milestone 3:''' larger examples (End of Feb 2008)::
  Optimisations and added functionality to handle larger example programs.

=== Work items for vectorisation ===

 * We need closure conversion to handle the full higher-order case.
 * Can we get some cheap'n'cheesy vectorisation by disallowing any "difficult" form of higher-order functions?  (Eg, higher-order functions without free variables and without partial applications are easier to vectorise.)

=== Work items for the library ===

 * ??
```
