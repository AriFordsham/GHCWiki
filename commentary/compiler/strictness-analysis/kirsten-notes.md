CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary/Compiler/StrictnessAnalysis/KirstenNotes"
  queryString          = "?version=6"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:58:18 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","277"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary/Compiler/StrictnessAnalysis/KirstenNotes\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

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

After calling dmd_anal on the body of a let, which results in demand type {{{dmd_ty}}} with demand env {{{dmd_env}}}, we do the following for each let-bound variable {{{f}}}:
 1. Iterate through all the keys in {{{dmd_env}}}, finding all applications of {{{f}}} to ''n'' arguments.
 1. For each ''i'' from 1 through ''n'' (where ''n'' is {{{f}}}'s arity), if each of the applications of {{{f}}} to ''i'' arguments has usage demand {{{OneOrZero}}}, then it's safe to mark the corresponding lambda-expression as a one-shot lambda. 

This might work, but is kind of kludgy.

This may be ''way'' too unnecessarily complicated. Can't we just get the same information from the demand on f in the free-var environment of the let body as it is, without changing the environment?

suppose the demand on f is
{{{
S1K(S1K(LMX))
}}}
if f = 
{{{
(\ x. (\ y. ...))
}}}
then we can mark the outer two lambdas as being one-shot. Right?

Not exactly. Suppose:
{{{
let f = \ x. \ y. ... in
  ...(f 1 2)...(f 3 4)...
}}}
f will have demand on it:
{{{
SMK(SMK(LMX))
}}}
because it's called more than once. We really want the demand to reflect that (f 1) is called only once and (f 3) is called only once, but it doesn't. So it doesn't seem like we can figure out what we need to know just by looking at the demand on f.
```
