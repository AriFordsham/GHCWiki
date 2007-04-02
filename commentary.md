CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary"
  queryString          = "?version=77"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 07:02:08 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","251"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= The GHC Commentary =

This tree of wiki pages is a "commentary" on the GHC source code.  It contains all the explanatory material that doesn't belong in comments in the source code itself, because the material is wide-ranging, usually covers multiple source files, and is more architectural in nature.  The commentary can also be considered a design document for GHC.

Please feel free to add material to this commentary: don't worry too much about accuracy, in due course someone will edit your contribution.  Try to link to source files as much as possible by using this macro: {{{[[GhcFile(compiler/Makefile)]]}}} (the usual Trac {{{source:}}} macro doesn't work here because the GHC darcs repository isn't integrated into this Trac).  Also try to add appropriate links to other parts of the commentary.


== Contents ==

 * [wiki:Commentary/SourceTree Source Tree Roadmap]
 * [wiki:Commentary/Pipeline The compilation pipeline]

 * [wiki:Commentary/Compiler The Compiler]
  
 * [wiki:Commentary/Rts The Runtime System]
 
 * Cross-cutting concerns: topics which span both the compiler and the runtime system
    * [wiki:Commentary/Profiling Profiling]
    * [wiki:Commentary/PrimOps Primitive Operations (PrimOps)]; see also [wiki:Commentary/Compiler/WiredIn Wired-in and known-key things]

 * [wiki:Commentary/UserManual The User Manual] (formatting guidelines etc)

 * [wiki:Commentary/EvilMangler The Evil Mangler]

== Contributed Documentation ==

Please feel free to add new pages here.  In due course information will migrate from here to the main commentary above.

 * The Compiler
   * [wiki:TypeFunctions]: Notes concerning the implementation of type functions and associated types, which was merged in the HEAD during ICFP'06.
   * [wiki:IntermediateTypes]: Notes about the type system of GHC's new intermediate language (in the HEAD since ICFP'06)
   * [wiki:DataParallel]: Notes about the implementation of Data Parallel Haskell
   * [wiki:RewriteRules]: Notes about the implementation of RULEs in GHC
   * [wiki:BackEndNotes]: Some ideas and notes about the back end.
   * [wiki:Commentary/CmmExceptions Cmm: Implementing Exception Handling]: Implementing exception handling for primitive operations in Cmm
   * [wiki:GhciDebugger]: Some notes about the implementation of the GHCi debugger. Probably uninteresting unless you want to work on the debugger.
    * AddingNewPrimitiveOperations: How to add new primitive operations to GHC Haskell.
   * [wiki:ReplacingGMPNotes Replacing GMP]: Notes from an effort to replace GMP with another Bignum library.
   * [wiki:ExternalCore]: Describes the process of bringing External Core up to speed. Once finished, this will simply describe what External Core is, and how it works. 
   * [wiki:HaddockComments]: Some notes about how the Haddock comment support is implemented.
   * [wiki:ExplicitCallStack]: Notes about maintaining an explicit call stack, to support error attribution and profiling.
   * [wiki:Commentary/PositionIndependentCode Position Independent Code and Dynamic Linking]

 * The Runtime System
   * [wiki:GarbageCollectorNotes] Notes about GHC's existing single threaded garbage collector and development of a parallel GC.
   * [wiki:GMPMemoryManagement] Describes how the garbage collector cooperates with GMP for Integer.
   * [wiki:SemiTagging]: Describes how the semi-tagging optimisation will be implemented.
   * [wiki:PAPI]: Measurement of program performance usign CPU events (cache misses, branch mispredictions).

 * Getting to grips with the code base
   * BeginnersNotes: Some notes about getting started hacking GHC and the structure of the compiler (especially types and typecheck)
   * DebuggingGhcCrashes: how to use gdb to debug a crash in GHC-compiled code.

 * [wiki:AboutVideos Hackathon presentations] (video)

 
== Old but useful ==

Finally, here are some generally-useful, but now somewhat-out-of-date resources:
 * [http://www.cse.unsw.edu.au/~chak/haskell/ghc/comm/ The old GHC Commentary]: Information on the internals of GHC, in various states of up-to-dateness.  We are keen to move this stuff out of its current location and onto this Wiki.  If anyone is willing to help do that, even for just a part in which you are interested, we would be delighted.
 * GhcPapers: papers and pointers to other documents that relate to the inner workings of GHC.


Convert type diagram to SVG? Upload graphviz source?
```
