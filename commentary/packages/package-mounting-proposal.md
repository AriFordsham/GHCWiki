CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary/Packages/PackageMountingProposal"
  queryString          = "?version=1"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:57:00 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","270"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary/Packages/PackageMountingProposal\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
A message by Frederik Eaton to the Haskell mailing list describing this proposal is archived here: [http://www.haskell.org/pipermail/libraries/2005-June/004009.html]. (Note: A proposal by Simon Marlow for "package grafting" predates it: [http://www.haskell.org/pipermail/libraries/2003-August/001310.html]. However, the "package grafting" proposal is different in that it suggests selecting a "mount point" at library installation time, where in the present proposal, the "mount point" is selected each time a module using the library in question is compiled. The difference is important, as one doesn't really want to have to install a new copy of a library just to use it with a different name. Also, Simon Marlow's proposal puts package versions in the module namespace and therefore source code, where we argue for decoupling source code from anything to do with provenance - be it package names or version numbers.)

This document will go over Frederik's proposal again in brief. The proposal doesn't involve any changes to syntax, only an extra command line option to {{{ghc}}}.

During compilation of a module, every package would have a "mount point" with respect to which its module namespace would be resolved. Each package should have a default "mount point", but this default would be overridable with an option to {{{ghc}}}.

For example, the {{{X11}}} library currently has module namespace:

{{{
  Graphics.X11.Types
  Graphics.X11.Xlib
  Graphics.X11.Xlib.Atom
  Graphics.X11.Xlib.Event
  Graphics.X11.Xlib.Display
  ...
}}}

In this proposal, it would have default mount point {{{Graphics.X11}}} and module namespace:

{{{
  Types
  Xlib
  Xlib.Atom
  Xlib.Event
  Xlib.Display
  ...
}}}

If someone wanted to specify a different the mount point, he could use the {{{-package-base}}} option, for example:

{{{
  ghc -package X11 -package-base Graphics.Unix.X11 ...
}}}

or

{{{
  ghc -package X11-1.2 -package-base NewX11 -package X11-1.0 -package-base OldX11 ...
}}}

However, usually the default would be used.

=== Evaluation ===

This proposal has several advantages over the GhcPackages proposal.

 * ''No package names in code''. In this proposal, package names would be decoupled from code. This is very important. It should be possible to rename a package (or create a new version of a package with a new name), and use it in a project, without editing every single module of the project and/or package. Even if the edits could be done automatically, they would still cause revision control headaches. Any proposal which puts package names in Haskell source code should be considered unacceptable.

 * ''No syntax changes''. The GhcPackages proposal requires new syntax, but this proposal does not. Of course, in this proposal it would be slightly more difficult for the programmer to find out which package a module is coming from. He would have to look at the command line that compiles the code he's reading. However, I think that that is appropriate. Provenance should not be specified in code, since it changes all the time. (And there could be a simple debugging option to GHC which outputs a description of the namespace used when compiling each file)

 * ''Simpler module names''. This proposal would allow library authors to use simpler module names in their packages, which would in turn make library code more readable, and more portable between projects. For instance, imagine that I wanted to import some of the code from the {{{X11}}} library into my own project. Currently, I would have to delete every occurrence of {{{Graphics.X11}}} in those modules. Merging future changes after such an extensive modification would become difficult. This is a real problem, which I have encountered while using John Meacham's curses library. There are several different versions of that library being used by different people in different projects, and it is difficult to consolidate them because they all have different module names. The reason they have different module names is that package mounting hasn't been implemented yet. The GhcPackages proposal would not fix the problem.

 * ''Development decoupled from naming''. In the present proposal, programmers would be able to start writing a library before deciding on a name for the library. For instance, every module in the {{{Parsec}}} library contains the prefix {{{Text.ParserCombinators.Parsec}}}. This means that either the author of the library had to choose the name {{{Parsec}}} at the very beginning, or he had to make several changes to the text of each module after deciding on the name. Under the present proposal, he would simply call his modules {{{Char}}}, {{{Combinator}}}, {{{Error}}}, etc.; the {{{Text.ParserCombinators}}} prefix would be specified in the build system, for instance in the Cabal file.

Frederik's mailing list message discusses some other minor advantages, but the above points are the important ones. In summary, it is argued that the above proposal should be preferred to GhcPackages because it is both easier to implement (using command line options rather than syntax), and more advantageous for the programmer.

```
