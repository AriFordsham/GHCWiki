CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/NewGhciDebugger"
  queryString          = "?version=18"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 07:02:15 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","254"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/NewGhciDebugger\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac

[[PageOutline]]

= Documentation for the new GHCi debugger =

These notes detail the breakpoint debugger which is being incorportated into GHCi. Note that there was/is a previous prototype debugger, and we share some of its code (specifically the term printer) (see: [wiki:GhciDebugger]).

== User's Manual ==

=== Setting break points ===

The general rule of thumb for breakpoints is that you can set a breakpoint on any thing which is not a value (though there are some exceptions). For example, a literal character is a value, but a case expression is not. 

We call the places where you can set breakpoints as '''breakable expressions''' (even if some of them aren't strictly expressions).

You '''can''' set breakpoints on the following things: (XXX) Check this list carefully!
 1. Function applications. We allow breakpoints on partial applications, even though they are technically values. Also, if there is an application with more than one argument, we only allow breaks on the whole expression, not on the sub-applications within: e.g. for the expression `map f list`, we allow a break on the whole expression, but not on the sub-application of `map f`.
 2. Case expressions.
 3. Function declarations (all the equations of a function).
 4. Case alternatives.
 5. Do statements.
 6. Guards.
 7. Bodies of functions, pattern bindings, lambdas, guarded equations.

Conversely, you '''cannot''' set breakpoints on the following things, except if they occur as the outermost expression in the body of a declaration:
 1. Literals.
 2. Variables.
 3. Do blocks. XXX check this one
 4. List comprehensions. XXX check this one

You can set a breakpoint in three ways:
 1. By line number.
 2. By line and column number.
 3. By function name (not implemented yet).

In each case you can specify in which module you want to set the breakpoint, however, if that is omitted, the debugger will choose a suitable default module for you (XXX give a better explanation of what module is chosen by default).

The syntax for setting breakpoints by line number is:

{{{
   :break OptionalModuleName 12
}}}

This will activate the breakpoint which corresponds to the leftmost outermost breakable expression which ''begins'' and ''ends'' on line 12 in the module called `OptionalModuleName`, if such an expression exists. XXX If no such expression exists then what happens? Currently the debugger will report an error message, but perhaps it is nicer for it to probe a few lines ahead until it finds a breakable expression, or give up after some threshold number of lines?

The syntax for setting breakpoints by line and column is:

{{{
   :break OptionalModuleName 12 7
}}}

This will activate the breakpoint which corresponds to the ''smallest'' breakable expression which encloses the source location: line 12, column 7, if such an expression exists. If no such expression exists the debugger will report an error message and no breakpoints will be set.

The syntax for setting breakpoints by function name is: (XXX not yet implemented)

{{{
   :break OptionalModuleName functionName
}}}

This will activate the outermost breakpoint associated with the definition of the function called `functionName`. The breakpoint will cover all the equations of a multi-equation function. XXX What about local functions? XXX What about functions defined in type classes (default methods) and instance declarations?

=== Listing the active breakpoints ===

You can list the set of active breakpoints with the following command:

{{{
   :show breaks
}}}

Each breakpoint is given a unique number, which can be used to identify the breakpoint should you wish to delete it (see the `:delete` command). Here is an example list of breakpoints:

{{{
   0) Main (12,4)-(12,8)
   1) Foo (13,9)-(13,13)
   2) Bar (14,4)-(14,47)
}}}

=== Inspecting values ===

=== Single stepping ===

=== Continuing execution after a breakpoint ===

=== Known problems in the debugger ===

=== Wishlist of features (please add your's here) ===

== Todo ==

=== Pending ===

 * Replace Loc with a proper source span type

 * Look at slow behaviour of :print command on long list of chars (I've asked Pepe about this).

 * Investigate whether the compiler is eta contracting this def: "bar xs = print xs", this could be a problem if we want to print out "xs".

 * Implement show command (to list currently set breakpoints)

 * Fix the ghci help command

 * Implement the delete command (to delete one or more breakpoints)

 * Save/restore the link environment at break points. At a breakpoint we modify both the hsc_env of the current Session, and
also the persistent linker state. Both of these are held under IORefs, so we have to be careful about what we do here. The "obvious" option is to save both of these states on the resume stack when we enter a break point and then restore them when we continue execution. I have to check with Simon if there are any difficult issues that need to be resolved here, like gracefully handling exceptions etc.

 * Remove dependency on -fhpc flag, put debugging on by default and have a flag to turn it off

 * Allow break points to be set by function name. Some questions: what about local functions? What about functions inside
  type class instances, and default methods of classes?

 * Support Unicode in data constructor names inside info tables

 * Fix the slow search of the ticktree for larger modules, perhaps by keeping the ticktree in the module info, rather than re-generating it each time.

 * Use a primop for inspecting the STACK_AP, rather than a foreign C call

=== Done ===

=== Tentative ===


== Implementation notes ==



```
