CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary/PrimOps"
  queryString          = "?version=2"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:55:01 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","257"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary/PrimOps\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
[ Up: [wiki:Commentary] ]

= Primitive Operations (!PrimOps) =

A !PrimOp is a function that cannot be implemented in Haskell, and are provided natively by GHC.  For example, adding two {{{Int#}}} values is provided as the !PrimOp {{{+#}}}, and allocating a new mutable array is the !PrimOp {{{newArray#}}}.

PrimOps are made available to Haskell code through the virtual module {{{GHC.Prim}}}.  This module has no implementation, and its interface never resides on disk: if {{{GHC.Prim}}} is imported, we use a built-in {{{ModIface}}} value - see {{{ghcPrimIface}}} in [[GhcFile(compiler/iface/LoadIface.lhs)]].

== Implementation of !PrimOps: !MachOps ==

== The primops.txt.pp file ==

== Adding a new !PrimOp ==

```
