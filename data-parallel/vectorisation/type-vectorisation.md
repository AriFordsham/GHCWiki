CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/DataParallel/Vectorisation/TypeVectorisation"
  queryString          = "?version=2"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 07:04:17 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","267"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/DataParallel/Vectorisation/TypeVectorisation\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
== Type Vectorisation ==

The transformation of types includes both closure conversion and the pairing of scalar with lifted computations.

=== Transformation rules ===

The type transformation rules achieve two goals: (1) they replace original type constructors and variables by their vectorised variants, where those are available, and (2) they alter the representation of functions:
{{{
T*            = T_V , if T_V exists
              = T    , otherwise
a*            = a_v
(t1 -> t2)*   = (  t1*  -> t2*,   , if kindOf t1 == #
                 [:t1*  -> t2*:])   or kindOf t2 == #
              = (  t1* :-> t2*,   , otherwise
                 [:t1* :-> t2*:])
(t1 t2)*      = t1* t2*
(forall a.t)* = forall a_v.t*
}}}
The transformation of function types includes both the change from `(->)` to `(:->)` as well as 
```
