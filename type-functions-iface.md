CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/TypeFunctionsIface"
  queryString          = "?version=3"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:55:03 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","257"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/TypeFunctionsIface\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= Type Functions: Interface =

== Representation of family instances ==

The `IfaceSyn.IfaceData` variant of `IfaceDecl` contains a new `ifFamInst :: Maybe IfaceFamInst` field that is distinguishes ordinary data/newtype declarations from family instances.  In the latter case, a `IfaceFamInst` value gives the instances head of the family instance.  Moreover, all family instance heads of a module are collected in the new `mi_fam_insts :: [(IfaceFamInst, IfaceDecl)]` field of `HscTypes.ModIface`.  We don't include the `IfaceDecl` component into `IfaceFamInst`, as we otherwise would get a cyclic dependency - the `IfaceDecl` must include `IfaceFamInst`, so that we can generate the correct wrapper signature for family data constructors during type checking the interface declarations.
```
