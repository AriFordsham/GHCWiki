CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary/Rts/HeapObjects"
  queryString          = "?version=2"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:54:38 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","262"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary/Rts/HeapObjects\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
[ Up: [wiki:Commentary/Rts] ]

= GHC Commentary: The Layout of Heap Objects =

== Terminology ==

 * A ''lifted'' type is one that contains bottom (_|_), conversely an ''unlifted'' type does not contain _|_.
   For example, {{{Array}}} is lifted, but {{{ByteArray#}}} is unlifted.

 * A ''boxed'' type is represented by a pointer to an object in the heap, an ''unboxed'' object is represented by a value.
   For example, {{{Int}}} is boxed, but {{{Int#}}} is unboxed.

The representation of _|_ must be a pointer: it is an object that when evaluated throws an exception or enters an infinite loop.  Therefore, only boxed types may be lifted.

There are boxed unlifted types: eg. {{{ByteArray#}}}.  If you have a value of type {{{ByteArray#}}}, it definitely points to a heap object with type {{{ARR_WORDS}}} (see below), rather than an unevaluated thunk.

Unboxed tuples {{{(#...#)}}} are both unlifted and unboxed.  They are represented by multiple values passed in registers or on the stack, according to the [wiki:Commentary/Rts/HaskellExecution return convention].

== Heap Objects ==

All heap objects have the same basic layout, embodied by the type {{{StgClosure}}} in [http://darcs.haskell.org/ghc/includes/Closures.h Closures.h].  The diagram below shows the layout of a heap object:


== Info Tables ==


== Types of Heap Object ==
```
