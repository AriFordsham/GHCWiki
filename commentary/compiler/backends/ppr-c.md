CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary/Compiler/Backends/PprC"
  queryString          = "?version=1"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:55:40 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","265"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary/Compiler/Backends/PprC\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac


= GHC Commentary: The C code generator =

Source: [[GhcFile(compiler/cmm/PprC.hs)]]

This phase takes [wiki:Commentary/Compiler/CmmType Cmm] and generates plain C code.  The C code generator is very simple these days, in fact it can almost be considered pretty-printing.

There are some slight subtleties:

 * [wiki:Commentary/Rts/HeapObjects#InfoTables info tables], which are expressed in Cmm as being laid out before the entry code for a
   closure, are compiled into separate top-level structures in the generated C, because C has no support for laying out data
   next to functions.  The desired layout is reconstructed in the assembly file by the [wiki:Commentary/EvilMangler Evil Mangler],
   or not if we're compiling unregisterised (see [wiki:Commentary/Rts/HeapObjects#TABLES_NEXT_TO_CODE TABLES_NEXT_TO_CODE]).
```
