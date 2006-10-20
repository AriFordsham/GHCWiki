CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary/Rts"
  queryString          = "?version=12"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:57:59 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","253"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary/Rts\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac


= GHC Commentary: The Runtime System =

GHC's runtime system is a slightly scary beast: 50,000 lines of C and C-- code, much of which seems at first glance to be completely obscure.  What on earth does the RTS ''do''?  Here are the highlights:

 * It includes all the bits required to execute Haskell code that aren't compiled into the code itself.
   For example, the RTS contains the code that knows how to raise an exception when you call {{{error}}},
   and code to implement the multi-precision {{{Integer}}} operations (via the GMP library).

 * It includes a sophisticated storage manager, including a multi-generational garbage collector with copying
   and compacting strategies.

 * It includes a user-space scheduler for Haskell threads, together with support for scheduling Haskell threads
   across multiple CPUs, and allowing Haskell threads to call foreign functions in separate OS threads.

 * There's a byte-code interpreter for GHCi, and a dynamic linker for loading up object code into a GHCi session.

 * Heap-profiling (of various kinds) and time-profiling of Haskell code are included.

 * Support for Software Transactional Memory is now included...

Next, we try to make sense of how it all fits together.

== Block Diagram ==

[[Image(rts-overview.png)]]

== RTS: Contents ==

 * [wiki:Commentary/Rts/Config RTS Configurations]
 * [wiki:Commentary/Rts/Word The Word]
 * [wiki:Commentary/Rts/Cmm What the hell is a .cmm file?]
 * [wiki:Commentary/Rts/Storage The Storage Manager]
 * [wiki:Commentary/Rts/Stack Layout of the stack]
 * [wiki:Commentary/Rts/Slop Slop]
 * [wiki:Commentary/Rts/Sanity Sanity Checking]
 * [wiki:Commentary/Rts/HaskellExecution The Haskell Execution model]
 * [wiki:Commentary/Rts/Scheduler The Scheduler]
 * [wiki:Commentary/Rts/FFI So how does foreign import "wrapper" work?]
 * [wiki:Commentary/Rts/Interpreter GHCi support: the byte-code interpreter and dynamic linker]
 * [wiki:Commentary/Rts/AsyncExceptions Asynchronous exceptions]
 * [wiki:Commentary/Rts/STM Software Transactional Memory (STM)]
 * [wiki:Commentary/Rts/CAFs Garbage Collecting CAFs]
 * [wiki:Commentary/Rts/Weak Weak Pointers and Finalizers]
 * [wiki:Commentary/Rts/Conventions Coding conventions in the RTS]

```
