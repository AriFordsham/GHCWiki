CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "GMPMemoryManagement"
  queryString          = "?version=2"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 07:02:09 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","258"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access GMPMemoryManagement\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= Interaction between GMP and the Storage Manager =

The Glasgow Haskell Compiler uses GMP to implement Integer.
GMP expects a storage model where pointers (newtyped as mpz_t) are explicitly allocated and freed.
However, Haskell requires Integers to be garbage collected.
This could be done with !ForeignPtrs, however (presumably for performance reasons?) GHC uses !ByteArray#s from the heap instead.
rts/sm/Storage.c `initStorage` sets three override functions to do this: `stgAllocForGMP`, `stgReallocForGMP`, and `stgDeallocForGMP`.

This requires a fairly subtle interaction in order to work safely,
because heap !ByteArray#s are normal objects and can be moved by garbage collection.
The important thing to note is that the GHC garbage collector is of the stop-the-world variety.
This means that all threads must reach a synchronization point before garbage collection can actually begin.
However, there are no such synchronization points inside the GMP-based primops.
Therefore, garbage collection, and consequent moving of Integers, can only occur when no GMP operations are executing.

```
