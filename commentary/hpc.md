CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary/Hpc"
  queryString          = "?version=1"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:59:30 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","253"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary/Hpc\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= Haskell Program Coverage =

This page describes the Haskell Program Coverage implementation inside GHC.

The basic idea is this
 * For each (sub)expression in the Haskell Syntax, write the (sub)expression in a HsTick
 * Each HsTick has a module local index number.
 * There is a table (The Mix datastructure) that maps this index number to original source location.
 * Each HsTick is mapped in the Desugar pass with: 
{{{
  dsExpr (HsTick n e) = case tick<modname,n> of DEFAULT -> e
}}}
 * This tick is a special type of Id, a TickOpId which takes no core-level argument, but has two pre-applied arguments; the module name and the module-local tick number.
   * We store both module name and tick number to allow this Id to be passed (inlined) inside other modules.
   * This Id has type '''State# World#'''
 * The core simplifier must not remove this case, but it can move it.
   * The do-not-remove is inforced via the ... function in ....
   * The semantics are tick if-and-when-and-as you enter the DEFAULT case. But a chain of consecative ticks can be executed in any order.
 * The CoreToStg Pass translates the ticks into StgTick
{{{
  .. (case tick<m,n> of DEFAULT -> e) = .. StgTick m n (... e)
}}}
 * The Cmm code generate translates StgTick to a 64 bit increment.

TO BE CONTINUED.

=== Binary Tick Boxes ===

The reason we do not translate tick boxes using.
{{{
 if e then (tick a e1) else (tick b e2)
}}}
is tick<a> e1 is a CAF, and gets lifted to top level. This maintain the coverage information, but does not allow for entry counting. If the if/then/else is called 100 times, and no exceptions were thrown, then you would expect the binary tick count to add up to 100. We hope to use Hpc to do path optimization in the future, so real numbers are important.
 
Also, we translate the tick late to allow case-of-case to work, allowing unboxed compares to work without generating boolean intermeduates. We still need to push one optimzation into the simpifier for this to work.

```
