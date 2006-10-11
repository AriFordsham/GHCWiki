CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/TypeFunctionsIface"
  queryString          = "?version=8"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:57:31 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","257"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/TypeFunctionsIface\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= Type Functions: Interface =

== Representation of family instances ==

The `IfaceSyn.IfaceData` variant of `IfaceDecl` contains a new `ifFamInst :: Maybe (IfaceTyCon, [IfaceType])` field that distinguishes ordinary data/newtype declarations from family instances.  In the latter case, the `(IfaceTyCon, [IfaceType])` value gives the family instance type.  In addition, each family instance is represented by a value of type `IfaceFamInst` that includes the instances rough match (i.e., name of the family type and a `[Maybe IfaceTyCon]` value that gives the outermost type constructor of each index argument for that instance) and refers to the type declaration for full details (as a class instance does with its dfun).

Moreover, much like class instances, family instance heads of a module are collected in the new `mi_fam_insts :: [IfaceFamInst]` field of `HscTypes.ModIface`.  This allows to enter the rough matches into the `ModGuts`, when reading the interface, without having to parse the full tycon that represents the instance yet.  This in turn avoids pulling in everything that hangs of that tycon.
```
