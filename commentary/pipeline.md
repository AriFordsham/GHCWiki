CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary/Pipeline"
  queryString          = "?version=12"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:56:04 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","257"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary/Pipeline\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
[ Up: [wiki:Commentary] ]

= The compilation pipeline =

When GHC compiles a module, it calls other programs, and generates a series of intermediate files.  Here's a summary of the process.
(source reference: {{{ghc/compiler/main/DriverPipeline.hs}}})

We start with {{{Foo.hs}}} or {{{Foo.lhs}}}, the "l" specifing whether literate style is being used.

 * Run the unlit pre-processor, {{{unlit}}}, to remove the literate markup, generating {{{Foo.lpp}}}.  The {{{unlit}}} processor is a C program kept in {{{utils/unlit}}}.

 * Run CPP (if {{{-cpp}}} is specified), generating {{{Foo.cpp}}} or {{{Foo.hspp}}} respectively.

 * Run the compiler itself. This does not start a separate process; it's just a call to a Haskell function.  This step always generates an '''interface file''' {{{Foo.hi}}}, and depending on what flags you give, it also generates a compiled file:
   * Assembly code: flag {{{-S}}}, file {{{Foo.s}}}
   * C code: flag {{{-fviaC}}}, file {{{Foo.hc}}}
   * C-- mode: flag {{{-fcmm}}}, file {{{Foo.cmm}}}, believed not to work

  * Run the C compiler [followed by the evil mangler] or assembler, as appropriate, generating {{{Foo.o}}}

== Interface files ==

An '''interface file''' supports separate compilation by recording the information gained by compiling {{{M.hs}}} in its interface file {{{M.hi}}}.  Morally speaking, the interface file {{{M.hi}}} is part of the object file {{{M.o}}}; it's like a super symbol-table for {{{M.o}}}.

Interface files are kept in binary, GHC-specific format.  The format of these files changes with each GHC release, but not with patch-level releases.  You can see what's in an interface file (often very useful) thus:
{{{
  ghc --show-iface M.hi
}}}

Here are some of the things stored in an interface file {{{M.hi}}}
 * A list of what {{{M}}} exports.
 * The types of exported functions, definition of exported types, and so on.
 * Version information, used to drive the smart recompilation checker.
 * The strictness, arity, and unfolding of exported functions.  This is crucial for cross-module optimisation; but it is only included when you compile with {{{-O}}}.

== HC files ==

GHC uses {{{gcc}}} as a code generator, in a very stylised way:
 * Generate {{{Foo.hc}}}
 * Compile it with {{{gcc}}}, using {{{register}}} declarations to nail a bunch of things into registers (e.g. the allocation pointer)
 * Post-process the generated assembler code with the Evil Mangler

```
