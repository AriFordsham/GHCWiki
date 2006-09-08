CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary/Compiler/Renamer"
  queryString          = "?version=2"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:54:55 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","261"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary/Compiler/Renamer\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
[ Up: [wiki:Commentary/Compiler/HscMain] ]

= The renamer =


The renamer's Number One taks is to replace [wiki:Commentary/Compiler/RdrNameType RdrNames] with [wiki:Commentary/Compiler/NameType Names].  For example, consider
{{{
module K where
  f x = True

module N where
  import K

module M where
  import N( f ) as Q
  f = (f, M.f, Q.f, \f -> f)
}}}
(where all the variables are {{{RdrName}}}s).  The result of renaming module M is:
{{{
M.f = (M.f, M.f, K.f, \f_22 -> f_22)
}}}
where all these names are now {{{Name}}}s.
  * The top-level unqualifed {{{RdrName}}} "{{{f}}}" has become the {{{External}}} {{{Name}}} {{{M.f}}}.  
  * The occurrences "{{{f}}}" and "{{{M.f}}}" are both bound to this {{{Name}}}.  
  * The qualified {{{RdrName}}} "{{{Q.f}}}" becomes the {{{Name}}} {{{K.f}}}, because the function is defined in module K.  
  * The lambda-bound "{{{f}}}" becomes an {{{Internal}}} name, here written {{{f_22}}}.  (All the {{{External}}} names have uniques too, but we often do not print them.)

In addition, the renamer does the following things:

 * Sort out fixities. The parser parses all infix applications as '''right-associative''', regardless of fixity.  For example "{{{a * b + c}}}" is parsed as "{{{a * (b + c)}}}".  The renamer re-associates such nested operator applications, using the fixities declared in the module.

 * Dependency analysis for mutually-recursive groups of declarations.  This divides the declarations into strongly-connected components.

 * Lots of lexical error checking: variables out of scope, unused bindings, unused imports, patterns that use the same binder many times, etc.

```
