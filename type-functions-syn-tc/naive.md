CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/TypeFunctionsSynTC/Naive"
  queryString          = "?version=1"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:59:54 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","260"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/TypeFunctionsSynTC/Naive\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
== A first (naive) attempt ==


To solve  {{{(C implies t1=t2)}}} with respect to Ax

 1. We interpret Ax /\ C as a rewrite system (from left to right).
 2. We exhaustively apply rewrite rules on t1 and t2, written t1 -->* t1' and t2 -->* t2' and check that t1' and t2' are syntactically equivalent.


Immediately, we find a problem with this solving strategy.
Consider our running example.

Rewrite rules
{{{
(forall a. S [a] = [S a]) /\      (R1) -- axioms
(T Int = Int)                     (R2)

 /\

(T [Int] = S [Int]) /\            (R3) -- local assumptions
(T Int = S Int)                   (R4)
 }}}

applied to {{{(T [Int] = [Int])}}}

yields
{{{
T [Int] -->* [S Int]       (R3,R1)

[Int] -->* [Int]
}}}

Hence, our (naive) solver fails, but
clearly the (local) property  (T [Int] = [Int])
holds.

The trouble here is that

 * the axiom system Ax is confluent, but
 * if we include the local assumptions C, the combined system Ax /\ C is non-confluent (interpreted as a rewrite system)


Possible solutions:

Enforce syntactic conditions such that Ax /\ C is confluent.
It's pretty straightforward to enforce that Ax and
constraints appearing in type annotations and data types
are confluent. The tricky point is that if we combine
these constraints they may become non-confluent.
For example, imagine

{{{
Ax : T Int = Int

   a= T Int      -- from f :: a=T Int => ...

      implies 

        (a = S Int -- from a GADT pattern

            implies ...)
}}}

The point is that only during type checking we may
encounter that Ax /\ C is non-confluent!
So, we clearly need a better type checking method.



```
