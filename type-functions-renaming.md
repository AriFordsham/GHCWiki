CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/TypeFunctionsRenaming"
  queryString          = "?version=4"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:54:25 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","258"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/TypeFunctionsRenaming\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= Type Functions: Renaming =

== Phasing ==

GHC is organised such that class and type declarations are processed (during renaming and type checking) before any instance declarations are considered.  In the presence of associated types, instance declarations may contain type definitions.  In particular, the ''data constructors'' introduced by associated data declarations need to be brought into scope before we can rename any expressions.  Otherwise, the intution wrt. to phasing is that kind signatures are handled in conjunction with vanilla algebraic data type declarations and instances of indexed types are handled together with instance ''heads''.  (NB: GHC splits the treatment of instances into two phases, where instances heads are processed in the first and member function declarations in the second phase.)

== Renaming of indexed types ==

=== Kind signatures ===

Kind signatures are renamed by `RnSource.rnTySig`, which is parametrised by a function that handles the binders (i.e., index variables) of the declaration.  This is so that we can use the same code for toplevel signatures and those in classes.  In the former case, the variables are in a defining position, whereas in classes they are in a usage position (as all index variables must be class parameters).

=== Definitions of indexed types ===

There is little extra that needs to be done for indexed types.  The main difference between vanilla synonyms and data/newtype declarations and the indexed variants is that the `tcdTyPats` field is not `Nothing`.  We simply call `rnTyPats` on these fields, which traverses them in the usual way.

=== Renaming of associated types ===

Associated '''data''' definitions are particularly interesting, as they not only introduce types, but also value level entities, namely the data constructors.  During renaming, we enter the names of all data constructors that an associated data type defines into the global `RdrName` environment by extending the function `RnNames.getLocalDeclBinders` such that it traverses instance declarations, too.  We are careful not to add the data type constructor multiple times by ignoring them in instance declarations.  The global `RdrName` environment only ever contains the type constructor introduced in the class declaration (i.e, the `RdrName` of an associated data type maps to the `Name` of the AT declaration in the class).

----
'''Remaining problem:''' The function `getLocalDeclBinders` must still supply the parent `Name` to the name generation for the data constructors. That parent name should be the one produced for the associated data declaration in the corresponding class declaration, which is hard to get hold of at this moment. So, we supply the Name of the data type constructor instead. That should probably be replaced by the class name in a later phase.
----

Otherwise, `RnSource.rnSrcInstDecl` invokes `RnSource.rnTyClDecl` on all associated types of an instance to rename them.


```
