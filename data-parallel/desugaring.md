CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/DataParallel/Desugaring"
  queryString          = "?version=2"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 07:02:06 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","258"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/DataParallel/Desugaring\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= Desugaring of array comprehensions =

Wadler's desugaring for list comprehensions is not suitable for arrays, as we need to use collective operations to get good parallel code.  The build/foldr desugaring, although using collective operations, isn't a good match for how the array operations are implemented.  In fact, the ''naive'' desugaring from the H98 report is a much better fit:
{{{
(1) [: e | :] 	         = [:e:]
(2) [: e | b, qs :]      = if b then [: e | qs :] else [::]
(3) [: e | p <- a, qs :] = let ok p = [: e | qs :]
		               ok _ = [::]
		           in concatMapP ok a
(4) [: e | let ds, qs :] = let ds in [: e | qs :]
(5) [: e | qs | qss   :] = 
(6) [: e | (XS, XSS) <- zip [: XS | qs :] [: XSS | qss :] :]
    where XS & XSS are the bound variables in qs & qss
}}}
In particular, `concatMapP f a` essentially implies to apply the lifted version of `f` directly to `a` and then the concat strips of one level of segment descriptors; i.e., both the `concatP` and the `mapP` vanish due to vectorisation.

== Problem with the naive rules ==

Nevertheless, these rules are not entirely satisfactory.  For example, `[:e | x <- a, b:]` turns into
{{{
concatMap (\x -> if b then [:e:] else [::]) a
}}}
which is a fairly complicated way to perform 
{{{
mapP (\x -> e) . filterP (\x -> b) $ a
}}}
even when taking vectorisation into account.  Under vectorisation, the conditional implies `filterP (\x -> b)`, but adds an expensive, and here useless, merge operation.  Maybe these overheads can be optimised away.  However, for the moment, we use a desugaring that is based on the above rules, but generates code that should be better suited to array processing.

== Modified rules ==


```
