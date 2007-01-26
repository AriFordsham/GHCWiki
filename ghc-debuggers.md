CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/GhcDebuggers"
  queryString          = "?version=7"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 07:01:07 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","252"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/GhcDebuggers\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
== Debugging Support Efforts in GHC ==

There are various efforts to add Haskell-level debugging support to GHC. This page
summarizes them.

=== The Ghci Debugger ===

The [wiki:GhciDebugger Ghci Debugger] was developed under the Google SoC initiative. The implementation is an extension to Ghci, called at debug time, and the main contributions are

 * A closure viewer, capable of showing intermediate computations without forcing them, and without depending on types (and of course that excludes dependency on Show instances)
 * Adding a basic breakpoint primitive to use in a system of dynamic breakpoints for ghci, which calls ghci in the correct scope.

|| Status: || Implemented, ??? ||
|| Video: || ??? || 

== The Hpc Tracer (HpcT) ==

The Hpc Tracer is a dynamic tracer for Haskell Program Coverage information. HpcT allows you to single step
through your program, seeing exactly what part of the code was executed when. It also support viewing exception and thread change events, as well as providing a 'Step Back' button which allow you to rewind your program to see what path you took to get to a problematic piece of code (for example head []).

The primary weakness is the inability to see live data structures; this debugger/tracer is silent, you can see where you are but not any of your values.

|| Status: || Implemented, ??? ||
|| Video: || http://movies.unsafeperformio.com/hpctpreview2.mov ||

== Merging the two designs ==

The plan is to take the best ideas from both debuggers and combine them into an even better debugger. Currently GhciD has support for displaying local variables, but HpcT does not. However, HpcT has more efficient breakpoints.

Let E be the expression we want to break at.

GhciD transforms E like so:

   breakpointJump <id> <vals> <srcloc> E

where <id> is a pointer into a data structure in Ghci which contains indentifer infos, <vals> is a list of values of local variables, and <srcloc> is a source location of the breakpoint. The problem with this is that the data structures associated with <id> <vals> <srcloc> are turned into let allocations by the compiler. This is costly. To keep the costs down GhciD only annotates a limited class of expressions, such as function bodies and case alternatives. 

HpcT transforms E like so:

   case tick_n E of () -> E

where n is some kind of unique identifer for the annotation which refers (via a table) to the source location of the breakpoint. 

This is cheaper than breakpointJump because it doesn't involve any let allocations. Therefore ticks can be added to a larger class of sub expressions. Obviously the downside is that tick does not have access to local variable information.

The question is, how can we get the performance of tick combined with the variable infos of breakpointJump?

One idea would be to extend Byte Code Objects (BCOs) with additional fields which record the neccessary source information. 

```
