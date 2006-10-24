CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary/Compiler/StrictnessAnalysis/KirstenNotes"
  queryString          = "?version=1"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:58:17 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","277"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary/Compiler/StrictnessAnalysis/KirstenNotes\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= Linearity =

Instead of:
{{{
type DmdEnv = VarEnv Demand
}}}
we want:
{{{
type DmdEnv = Env CoreExpr Demand
}}}

We keep track of demands on partial applications.

After calling dmd_anal on the body of a let, which results in demand type {{{dmd_ty}}} with DmdEnv {{{dmd_env}}}, we do the following for each let-bound variable {{{f}}}:
1. Iterate through all the keys in {{{dmd_env}}}, finding all applications of {{{f}}} to ''n'' arguments.
1. For each ''i'' from 1 through ''n'' (where ''n'' is {{{f}}}'s arity), if each of the applications of {{{f}}} to ''i'' arguments has usage demand {{{OneOrZero}}}, then it's safe to mark the corresponding lambda-expression as a one-shot lambda. 

This might work, but is kind of kludgy.
```
