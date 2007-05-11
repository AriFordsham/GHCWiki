CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary/Compiler/CPS"
  queryString          = "?version=2"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 07:04:00 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","259"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary/Compiler/CPS\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= CPS Conversion =

This part of the compiler is still under construction and it not presently in ghc-HEAD.
These notes are to document it for when it does get merged in.

== Overview ==
This pass takes Cmm with native proceedure calls and an implicit stack and produces Cmm with only tail calls implemented as jumps and an explicit stack.  In a word, it does CPS conversion.  (All right, so that's two words.)

== Design Aspects ==
 * Proc-Point Analysis
 * Calling Conventions
 * Live Value Analysis
 * Stack Layout

== Simple Design ==
 * Split blocks into multiple blocks at function calls
 * Do liveness analysis
 * Split every block into a separate function
 * Pass all live values as parameters (probably slow)
   * Must arrange for both the caller and callee to know argument order
     * Simple design: callee just chooses some order and all callers must comply
   * Eventually could be passed implicitly but keeping things explicit makes things easier
   * Evantually could use a custom calling convention
 * Save live values before a call in the continuation
   * Must arrange for bot the caller and callee to know field order
     * Simple design: callee just chooses some order and all callers must comply
   * Eventually needs to be optimized to reduce continuation shuffling
     * Can register allocation algorithms be unified with this into one framework?

== Pipeline ==
 * CPS
   * Make closures and stacks manifest
   * Makes all calls are tail calls
 * Parameter Elimination
   * Makes calling convention explicit
   * For externally visible functions calling conventions is machine specific, but not backend specific because functions compiled from different backends must be be able to call eachother
   * Lor local functions calling convention can be left up to the backend because, it can take advantage of register allocation.
     * However, the first first draft will specify the standard calling convention for all functions even local ones because:
       * It's simpler
       * The C code generator can't handle function parameters because of the Evil Mangler
       * The NCG doesn't yet understand parameters

```
