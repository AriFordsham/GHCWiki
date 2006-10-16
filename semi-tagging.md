CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/SemiTagging"
  queryString          = "?version=8"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:57:40 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","251"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/SemiTagging\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= The semi-tagging optimisation =

Here I describe the design of the semi-tagging optimisation. Currently most of the text comes from http://hackage.haskell.org/trac/summer-of-code/ticket/48

This page reflects my current understanding on the compiler and the RTS, so if there is something wrong, just yell!

== The starting point ==

Currently when evaluating an expression that is the scrutinee of a case:
{{{
case x of { ... }
}}}
GHC jumps to the code for (i.e. "enters") the x closure, which returns when x is evaluated. Commonly, x is already evaluated, and the code for an evaluated constructor just (vector) returns immediately.

''Alexey: add some example HC code here''

== Testing before jumping ==

The simplest optimisation is this.  Instead of entering the closure, grab its info pointer, and follow the info pointer to get the tag.  Now test the tag; if it's evaluated, don't enter the closure.  

The benefit is that processors are typically faster at "test-and-jump to known location" than they are at "jump to this pointer".

''Alexey: add some example HC code here''

== Tagging the LSB of an evaluated closure ==

 The idea is to encode the fact that a pointer points to an evaluated object by setting the LSB of the pointer. If the case expression detects that the closure is evaluated, it can avoid the jump and return, which are expensive on modern processors (indirect jumps).

||  || bits 31..2 || bits 1 0 ||
|| unevaluated closure || ptr || 00 ||
|| evaluated constructor closure || ptr || 01 ||

This would require modifying
 * the code generation so that when allocating a constructor, the pointer to it has the appropriate bits set (just a matter of adjusting the offset from Hp)
 * the GC to set the LSB bit of constructor closure pointers,
 * the GC and the RTS code to mask out the LSB pointer when dereferencing it,
 * the code generation to test the LSB bit and case expressions and avoid the indirect jump.

== Using more than one bit ==

We can go a bit further than this, too. Since there are 2 spare bits (4 on a 64-bit machine), we can encode 4 (16) states. Taking 0 to mean "unevaluted", that leaves 3 (15) states to encode the values for the "tag" of the constructor. eg. an evaluated Bool would use 1 to indicate False and 2 to indicate True. An evaluated list cell would use 1 to indicate [] and 2 to indicate (:).

||  || bits 31..2 || bits 1 0 ||
|| unevaluated closure || ptr || 00 ||
|| cons. no. 1    || ptr || 01 ||
|| cons. no. 2    || ptr || 10 ||
|| cons. no. 3    || ptr || 11 ||

The nice thing about the current approach is that code size is small; implementing the test and jump will certainly add extra code to compiled case expressions. But the gains might be worth it. Complexity-wise this means masking out these bits when following any pointer to a heap object, which means carefully checking most of the runtime.

This would require modifying all of the above plus modifying
 * the code generator so that it checks whether the number of constructors is smaller or equal than 3/15.

== Using a tag directly in the pointer ==

Constructors without children (such as {{{False}}} and {{{True}}}) only need their tag to be represented. Hence we can drop the pointer altogether as follows:

||  || bits 31..2 || bits 1 0 ||
|| unevaluated closure || ptr || 00 ||
|| cons. w/no children || tag || 01 ||
|| cons. w/children no. 1    || ptr || 10 ||
|| cons. w/children no. 2    || ptr || 11 ||

This still limits us to data types that have no more than two constructors with children. We can improve on this by noting that pointers will not point to low addresses. So we can make a simple test to distinguish between tags and pointers:

||  || bits 31..2 || bits 1 0 || ||
|| unevaluated closure || ptr || 00 || ||
|| cons. w/no children || tag || 01 || tag < no. of  constructors||
|| cons. w/ children || ptr || 01 || ptr >= no. of constructors ||
|| cons. w/children no. 1    || ptr || 10 || ||
|| cons. w/children no. 2    || ptr || 11 || ||

Of course this assumes that we don't have data types with too many thousands of constructors.

It might be possible that the case code for the alternatives above is becoming too complex. We might settle for the following "simple" option:

||  || bits 31..2 || bits 1 0 || ||
|| unevaluated closure || ptr || 00 || ||
|| cons. w/no children || tag || 01 || tag < no. of  constructors||
|| cons. w/ children || ptr || 01 || ptr >= no. of constructors ||

```
