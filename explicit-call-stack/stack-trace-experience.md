CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/ExplicitCallStack/StackTraceExperience"
  queryString          = "?version=3"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 07:01:33 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","267"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/ExplicitCallStack/StackTraceExperience\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= Experience with existing stack tracing tools =

Before we decide what kind of stack trace we want, it is very useful to see what the other existing tools already provide. This will give us some ideas about what is workable in practice.

The tools we will be testing are:
  * Hat, particularly hat-tack and hat-trail (hat stack is just a specialisation of hat-trail).
  * Cost Centre Stacks (CCS) with +RTS -xc -RTS
  * Andy Gill's HpcT tracer
  * The ghci debugger extended with a simple stack passing transformation. We will try both a full transformation and some kind of partial transformation (where only part of the code is transformed).

The test suite is a selection of programs from the nofig-buggy suite provided by the group at Technical University of Valencia. It is a modified version of the usual nofib benchmark suite. The suite can be obtained like so:
{{{
   darcs get --partial http://einstein.dsic.upv.es/darcs/nofib-buggy/
}}}

Since we are interested in stack traces we will limit ourselves to programs with crash with an uncaught exception, such as divide by zero, head of empty list, calls to error, pattern match failure and so forth.

The actual test programs are all from the real category:
  * anna: Julien's strictness analyzer. It has been modified to divide by zero.
  * whatever

== Test results ==

=== Test 1, anna, divide by zero error ===

  


```
