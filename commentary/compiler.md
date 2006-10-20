CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary/Compiler"
  queryString          = "?version=7"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:58:06 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","256"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary/Compiler\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac


= GHC Commentary: The Compiler =

The compiler itself is written entirely in Haskell, and lives in the many sub-directories of the [[GhcFile(compiler)]] directory.  

 * [wiki:ModuleDependencies Compiler Module Dependencies] (deals with the arcane mutual recursions among GHC's many data types)
 * [wiki:Commentary/CodingStyle Coding guidelines]

 * [wiki:Commentary/Compiler/KeyDataTypes Key data types]
 * [wiki:Commentary/Compiler/HscMain Compiling one module: HscMain]
   * [wiki:Commentary/Compiler/Renamer Renamer]
   * Typechecker
   * Desugarer
   * Core->core
     * [wiki:Commentary/Compiler/StrictnessAnalysis Strictness analysis]
   * Core->CorePrep
   * [wiki:Commentary/Compiler/Core2Stg CorePrep->Stg]
   * [wiki:Commentary/Compiler/CodeGen The code generator]: Stg->Cmm

 * [wiki:Commentary/Compiler/API The GHC API]
 * [wiki:Commentary/Compiler/SymbolNames Symbol names and the Z-encoding]
 * [wiki:Commentary/Compiler/TemplateHaskell Template Haskell]
 * [wiki:Commentary/Compiler/WiredIn Wired-in and known-key things]
 * [wiki:Commentary/Compiler/Packages Packages]
 * [wiki:Commentary/Compiler/Finder The Finder]
 * [wiki:Commentary/Compiler/Backends Backends]:
   * [wiki:Commentary/Compiler/Backends/PprC C code generator]
   * [wiki:Commentary/Compiler/Backends/NCG Native code generator]

== Overall Structure ==

Here is a block diagram of its top-level structure:

[[Image(ghc-top.png)]]

The part called '''!HscMain''' deals with compiling a single module.  On top of this is built the '''compilation manager''' (in blue) that manages the compilation of multiple modules.  It exports an interface called the '''GHC API'''.  On top of this API are four small front ends:

 * GHCi, the interactive environment, is implemented in [[GhcFile(compiler/ghci/InteractiveUI.hs)]] and sits squarely on top of the GHC
   API.
 
 * {{{--make}}} is almost a trivial client of the GHC API, and is implemented in [[GhcFile(compiler/main/Main.hs)]]. 

 * {{{-M}}}, the Makefile dependency generator, is also a client of the GHC API and is implemented in
   [[GhcFile(compiler/main/DriverMkDepend.hs)]]. 

 * The "one-shot" mode, where GHC compiles each file on the command line separately (eg. {{{ghc -c Foo.hs}}}). This mode bypasses teh GHC API, and is implemented
   directly on top of [wiki:Commentary/Compiler/HscMain HscMain], since it compiles only one file at a time. In fact, this is all that   
   GHC consisted of prior to version 5.00 when GHCi and `--make` were introduced.

GHC is packaged as a single binary in which all of these front-ends are present, selected by the command-line flags indicated above.  There is a single command-line interface implemented in [[GhcFile(compiler/main/Main.hs)]].

In adition, GHC is compiled, without its front ends, as a ''library'' which can be imported by any Haskell program; see [[wiki:Commentary/Compiler/API the GHC API]].

 


```
