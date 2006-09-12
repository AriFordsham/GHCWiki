CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary/Compiler/Backends"
  queryString          = "?version=1"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:55:35 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","262"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary/Compiler/Backends\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac


= GHC Commentary: Backends =

After [wiki:Commentary/Compiler/Cmm Cmm] has been generated, we have a choice of targets to compile to:

 * [wiki:Commentary/Compiler/Backends/PprC The C code generator]
 * [wiki:Commentary/Compiler/Backends/NCG  The native code generator]
 * Future: a C-- code generator

These backends are completely interchangeable.  Our preferred route is the native code generator, because the C code generator relies on some serious hackery, namely the [wiki:Commentary/EvilMangler Evil Mangler], to get fast tail-calls and other performance tricks.  The Evil Mangler is slated for removal as soon as possible, which would leave us with just the native code generator for optimised compilation, and the C code generator for portable, non-optimised, or unregisterised compilation.

It is likely that only the native code generator will be able to generate position independent code (PIC) which is necessary for dynamic libraries, so once we're doing this the C code generator will be even more deprecated.

```
