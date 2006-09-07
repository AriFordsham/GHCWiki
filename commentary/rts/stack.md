CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary/Rts/Stack"
  queryString          = "?version=3"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:54:43 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","259"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary/Rts/Stack\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
[ Up: [wiki:Commentary/Rts] ]

= Layout of the stack =

Every [wiki:Commentary/Rts/HeapObjects#ThreadStateObjects TSO object] contains a stack.  The stack of a TSO grows downwards, with the topmost (most recently pushed) word pointed to by {{{tso->sp}}}, and the bottom of the stack given by {{{tso->stack + tso->stack_size}}}.

The stack consists of a sequence of ''stack frames'' (also sometimes called ''activation records'') where each frame has the same layout as a heap object:

|| Header || Payload... ||

There are several kinds of [wiki:Commentary/Rts/Stack#StackFrames stack frame], but the most common types are those pushed when evaluating a {{{case}}} expression:
{{{
  case e0 of p1 -> e1; ...; pn -> en 
}}}
The code for evaluating a {{{case}}} pushes a new stack frame representing the alternatives of the case, and continues by evaluating {{{e0}}}.  When {{{e0}}} completes, it returns to the stack frame pushed earlier, which inspects the value and selects the appropriate branch of the case.  The stack frame for a {{{case}}} includes the values of all the free variables in the case alternatives.

== Info tables for stack frames ==

The info table for a stack frame has a couple of extra fields in addition to the [wiki:Commentary/Rts/HeapObjects#InfoTables basic info table layout]:

[[Image(ret-itbl.png)]]

The ''SRT'' field points to the SRT table for this stack frame (see [wiki:Commentary/Rts/CAFs] for details of SRTs).  The return vector gives a vector of return addresses in the case of the {{{RET_VEC_SMALL}}} and {{{RET_VEC_BIG}}} types of return addresses; see [wiki:Commentary/Rts/HaskellExecution#VectoredReturns vectored returns] for more details.

== Layout of the payload ==

Unlike heap objects which mainly have "pointers first" layout, in a stack frame the pointers and non-pointers are intermingled.  This is so that we can support "stack stubbing" whereby a live variable stored on the stack can be later marked as dead simply by pushing a new stack frame that identifies that slot as containing a non-pointer, so the GC will not follow it.

Stack frames therefore have [wiki:Commentary/Rts/HeapObjects#Bitmaplayout bitmap layout].

== Kinds of Stack Frame ==

{{{RET_BCO}}},
{{{RET_SMALL}}},
{{{RET_VEC_SMALL}}},
{{{RET_BIG}}},
{{{RET_VEC_BIG}}},
{{{RET_DYN}}},
{{{RET_FUN}}},
{{{UPDATE_FRAME}}},
{{{CATCH_FRAME}}},
{{{STOP_FRAME}}},
{{{ATOMICALLY_FRAME}}},
{{{CATCH_RETRY_FRAME}}},
{{{CATCH_STM_FRAME}}}

```
