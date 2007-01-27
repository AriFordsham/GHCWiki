CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/TypeFunctionsSynTC"
  queryString          = "?version=28"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 07:01:09 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","257"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/TypeFunctionsSynTC\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= Type Checking with Indexed Type Synonyms =

GHC has now FC as its typed intermediate language.
In a next step, we wish to add type functions to
GHC's source language.  Type functions in combination
with type annotations and GADTs allow us to type check
some interesting programs.

{{{
data Zero
data Succ n
data List a n where
  Nil  :: List a Zero
  Cons :: a -> List a m -> List a (Succ m)

type family Add :: * -> * -> *
type instance Add Zero     y = y
type instance Add (Succ x) y = Succ (Add x y)

append :: List a l -> List a m -> List a (Add l m)
append Nil xs = xs
append (Cons x xs) ys = Cons x (append xs ys)
}}}

However, type checking with type functions is challenging.

 * [wiki:TypeFunctionsSynTC/Challenge The challenge]
 * [wiki:TypeFunctionsSynTC/Naive A first (naive) attempt]
 * [wiki:TypeFunctionsSynTC/Second A second attempt]
 * [wiki:TypeFunctionsSynTC/GHC Type equations in GHC]
 * [wiki:TypeFunctionsSynTC/PlanMS Plan MS]
 * [wiki:TypeFunctionsSynTC/PlanMSRevised Plan MS revised]
 * [wiki:TypeFunctionsSynTC/Comparison Brief comparison]
 * [wiki:TypeFunctionsSynTC/GhcChr CHR-style simplification for GHC]
 * [wiki:TypeFunctionsSynTC/GhcChrExamples Examples]
```
