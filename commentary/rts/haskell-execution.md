CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary/Rts/HaskellExecution"
  queryString          = "?version=14"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 07:04:31 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","263"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary/Rts/HaskellExecution\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac


= The Haskell Execution Model =

The [wiki:Commentary/Compiler/StgSynType STG language] has a clear ''operational'' model, as well as having a declarative lambda-calculus reading.  The business of the [wiki:Commentary/Compiler/CodeGen code generator] is to translate the STG program into `C--`, and thence to machine code, but that is mere detail. From the STG program you should be able to understand:
  * What functions are in the compiled program, and what their entry and return conventions are
  * What heap objects are allocated, when, and what their layout is

GHC uses an eval/apply execution model, described in the paper [http://research.microsoft.com/%7Esimonpj/papers/eval-apply How to make a fast curry: push/enter vs eval/apply].  This paper is well worth reading if you are interested in this section.

Contents:
 
 * [wiki:Commentary/Rts/HaskellExecution/Registers Registers]
 * [wiki:Commentary/Rts/HaskellExecution/FunctionCalls Function Calls]
 * [wiki:Commentary/Rts/HaskellExecution/CallingConvention Call and Return Conventions]
 * [wiki:Commentary/Rts/HaskellExecution/HeapChecks Heap and Stack checks]
 * [wiki:Commentary/Rts/HaskellExecution/Updates Updates]

```
