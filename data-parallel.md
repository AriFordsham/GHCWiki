CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/DataParallel"
  queryString          = "?version=13"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 07:02:59 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","251"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/DataParallel\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= Data Parallel Haskell =

This page documents the integration of nested data parallelism into GHC at the developer level, including notes about where we are and what needs doing.  See also the [http://haskell.org/haskellwiki/GHC/Data_Parallel_Haskell user-level wiki page], which includes examples and tutorial-style instructions.

Most of the material describing our approach is partitioned into a set of subpages:

 * [wiki:DataParallel/Example Nested data parallelism by example]
 * [wiki:DataParallel/SMP Data parallelism on shared-memory machines]
 * [wiki:DataParallel/Design High-level design of adding NDP to GHC]
 * [wiki:DataParallel/ClosureConversion Details of the implementation of closure conversion]
 * [wiki:DataParallel/VectorisationSpec Requirements of the vectorisation transformation]
 * [wiki:DataParallel/Desugaring Desugaring of array comprehensions]
 * [wiki:DataParallel/Related Other nested data parallel work]

== Status and work plan ==

Detailed information on how to use the current implementation is at the [http://haskell.org/haskellwiki/GHC/Data_Parallel_Haskell user-level wiki page].  Here is information on the implementation status and outstanding work items:

 * DPH [wiki:DataParallel/Repositories repositories]
 * Our [wiki:DataParallel/WorkPlan work plan]
 * Some [wiki:DataParallel/Benchmarks benchmarks]

== Old material ==

Pages that have fallen out of use (and contain out dated information):

 * [wiki:DataParallel/Optimisation Optimisation, and problems therewith]
```
