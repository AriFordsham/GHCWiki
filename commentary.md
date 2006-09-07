CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary"
  queryString          = "?version=8"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:54:46 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","251"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
== 0. Overview (SPJ) ==

 * [wiki:Commentary/Organisation The overall organisation of GHC]
 * [wiki:Commentary/SourceTree Source Tree Roadmap]
 * [wiki:Commentary/Pipeline The compilation pipeline]
 * [wiki:Commentary/CodingStyle Coding guidelines]

== 1. Building (SM) ==
 * Getting the code (do a live build...)
 * Setting up the build
 * Building
 * tweaking & recompiling, etc.
 * how libraries/packages are built
 * ghc-inplace vs. installed ghc, how does it run inplace (find its bits)
 * how to "use" the build system and common tasks, pointer to docs
 * using/extending the testsuite

== 2. The Compiler ==

 * Start in the middle [wiki:Commentary/Compiler/HscMain compiling a single module]
 * SPJ the main datatypes (RdrName, Name, Id, Var, TyVar, Type, Core,  HsSyn, Cmm).
 * SPJ renamer, typechecker, desugarer, core->core
 * SPJ ModIface, ModDetails, ModGuts
 * SPJ Core->CorePrep->Stg->Cmm
 * SM PrimOPs: primops.txt.pp - what is generated from it?
 * SM GHC API
 * SM HscMain upwards: ModSummary, Finder, upsweep, downsweep,

== 3. Back end (SM) ==

 *  SM execution model STG + eval/apply
 *  SM Cmm->NCG
 *  SM Cmm->BCO  (simple compilation scheme, no primops: GHC.PrimopWrappers)
 *  SM Cmm->C

== 4. [wiki:Commentary/Rts The Runtime System] ==

```
