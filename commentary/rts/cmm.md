CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary/Rts/Cmm"
  queryString          = "?version=2"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:55:36 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","257"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary/Rts/Cmm\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac


= GHC Commentary: What the hell is a `.cmm` file? =

A `.cmm` file is rather like C--.  The syntax is almost C-- (a few constructs are missing), and it is augmented with some macros that are expanded by GHC's code generator (eg. `INFO_TABLE()`).  A `.cmm` file is compiled by GHC itself: the syntax is parsed by [[GhcFile(compiler/cmm/CmmParse.y)]] and [[GhcFile(compiler/cmm/CmmLex.x)]] into the [wiki:Commentary/Compiler/CmmType Cmm] data type, where it is then passed through one of the [wiki:Commentary/Compiler/Backends back-ends].

We use the C preprocessor on `.cmm` files, making extensive use of macros to make writing this low-level code a bit less tedious and error-prone.  Most of our C-- macros are in [[GhcFile(includes/Cmm.h)]].
```
