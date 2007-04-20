CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/DataParallel/ClosureConversion/ClassLess"
  queryString          = "?version=2"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 07:03:02 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","268"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/DataParallel/ClosureConversion/ClassLess\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
== Closure conversion without a conversion class ==

The following scheme - if Roman doesn't find any problems with it (he is notorious for that) - should be simpler than what we had in mind so far for mixing converted and unconverted code.

=== Type declarations ===

If a type declaration for constructor `T` occurs in a converted module, we
(1) generate a converted type declaration `T_CC` together two conversion functions `fr_T` and `to_T`, and
(2) store these three names in the representation of `T`.
Concerning Point (2), more precisely the alternatives of `TyCon.TyCon` get a new field `tyConCC :: Maybe (TyCon, Id, Id)`.  This field is `Nothing` for data constructors for which we have no conversion and `Just (T_CC, fr_T, to_T)` if we have a conversion.

Incidentally, if during conversion we come across a type declaration that we don't know how to convert (as it uses fancy extensions), we just don't generate a conversion.

=== Class declarations ===

If we come across a class declaration for a class `C` during conversion, we convert it generating `C_CC`.  Like with type constructors, `Class.Class` gets a `classCC :: Maybe Class` field that is `Just C_CC` for classes that have a conversion.  We also ensure that the `classTyCon` of `C`, let's call it `T_C`, refers to `T_C_CC` and `fr_T_C` and `to_T_C` in its `tyConCC` field, and that the `classTyCon` of `C_CC` is `T_C_CC`.

=== Instance declarations ===

If we encounter an instance declaration for `C tau` during conversion, there are two alternatives: we have a conversion for `C` or not:
 * if we do not have a conversion, we generate an instance (and hence dfun) for `C tau^`, where `tau^` is the closure converted `tau`;
 * if we have a conversion, we generate an instance for `C_CC tau^`.
In any case, we add a field `is_CC :: Just Instance` to `InstEnv.Instance` that contains the additionally generated instance.  And in both cases, we should be able to derive the required code for the dfun from the definition of `C tau`.

=== Type terms ===

We determine the converted type `t^` of `t` as follows:
{{{
T^            = T_CC , if available
                T    , otherwise
a^            = a
(t1 t2)^      = t1^ t2^
(t1 -> t2)^   = Clo t1 t2
(forall a.t)^ = forall a.t^
(C t1 => t2)^ = C_CC t1^ => t2^ , if available
                C t1^ => t2^    , otherwise
}}}

=== Value bindings ===

When converting a toplevel binding for `f :: t`, we generate `f_CC :: t^`.  The alternatives `GlobalId` and `LocalId` of `Var.Var` get a new field `idCC :: Maybe Id` and the `Id` for `f` contains `Just f_CC` in that field.
```
