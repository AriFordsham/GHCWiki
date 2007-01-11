CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/PAPI"
  queryString          = "?version=3"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 07:00:42 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","247"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/PAPI\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= Measuring program performance using CPU events =

The GHC runtime has been extended to support the use of the PAPI library to count occurrences of CPU events such as cache misses and branch mispredictions. The PAPI extension separates the events occurring in the garbage collector and mutator code for more accurate pinpointing of performance problems.

This page describes how to compile the RTS with PAPI enabled and explains the RTS options for CPU event selection. This page also contains patches to collect CPU event information in nofib runs and to allow their comparison using nofib-analyse. This is especially useful to measure the effects of optimisations accross a whole range of programs systematically.

= Compiling and running programs with PAPI =

to do

= Using PAPI with the nofib benchmarking suite =

to do

= Resources =

 * [http://icl.cs.utk.edu/papi/] PAPI home page.
 * [http://developer.amd.com/article_print.jsp?id=90] An article introducing the business of using CPU counters for performance measurement.

```
