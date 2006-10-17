CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary/Pipeline"
  queryString          = "?version=21"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:57:51 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","257"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary/Pipeline\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac


Video: [http://video.google.com/videoplay?docid=-4326420154219711812 Compilation Pipeline] and interface files (17'30")

= The compilation pipeline =

When GHC compiles a module, it calls other programs, and generates a series of intermediate files.  Here's a summary of the process.
(source reference: {{{ghc/compiler/main/DriverPipeline.hs}}})

We start with {{{Foo.hs}}} or {{{Foo.lhs}}}, the "l" specifing whether literate style is being used.

 * Run the '''unlit pre-processor''', {{{unlit}}}, to remove the literate markup, generating {{{Foo.lpp}}}.  The {{{unlit}}} processor is a C program kept in [[GhcFile(utils/unlit)]].

 * Run the '''C preprocessor''', `cpp`, (if {{{-cpp}}} is specified), generating {{{Foo.hspp}}}.

 * Run '''the compiler itself'''. This does not start a separate process; it's just a call to a Haskell function.  This step always generates an [wiki:Commentary/Compiler/IfaceFiles '''interface file'''] {{{Foo.hi}}}, and depending on what flags you give, it also generates a compiled file:
   * Assembly code: flag {{{-S}}}, file {{{Foo.s}}}
   * C code: flag {{{-fvia-C}}}, file {{{Foo.hc}}}
   * C-- mode: flag {{{-fcmm}}}, file {{{Foo.cmm}}}, believed not to work

  * In the {{{-fvia-C}}} case:
    * Run the '''C compiler''' to generate `Foo.raw_s`.
    * Run the [wiki:Commentary/EvilMangler Evil Mangler], generating {{{Foo.s}}}

  * If `-split-objs` is in force, run the '''splitter''' on `Foo.s`.  This splits `Foo.s` into lots of small files.  The idea is that the static linker will thereby avoid linking dead code.

  * Run the assembler on `Foo.s` or, if `-split-objs` in in force, on each individual assembly file.

```
