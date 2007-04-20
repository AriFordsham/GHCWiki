CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/DataParallel/ClosureConversion"
  queryString          = "?version=7"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 07:03:00 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","262"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/DataParallel/ClosureConversion\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
[[wiki:DataParallel Up]]
== Closure conversion as part of vectorisation ==

'''TODO:''' Describe the treatment of higher-order functions and closure conversion here. The relevant paper is [http://www.cse.unsw.edu.au/~chak/papers/LCK06.html]. The approach is described in more detail in [http://opus.kobv.de/tuberlin/volltexte/2006/1286/].

=== Closure-converted types as indexed-types ===

One option for implementing closure-conversion is to represent closure-converted types as an indexed type whose type index is the original type and to combine that indexed type in a type class with methods for converting between closure-converted and vanilla terms.  The details are under [wiki:DataParallel/ClosureConversion/Indexed indexed closure conversion].  There are two potential benefits for this approach: (1) we will probably have to do something similar for vectorisation anyway - see the [wiki:DataParallel/VectorisationSpec requirements of vectorisation] - and (2) it seems that we need less bookkeeping (e.g., the name of a closure converted data type is just the indexed type with the original data type as its index).  However, there are problems, too; in particular, as we currently don't have class contexts and polytypes as type indexes.

=== Requirements of closure conversion ===

==== The straight forward part ====

The actual closure-conversion transformation on lambda terms, which we intend to perform on Core.

==== Type declarations, classes, and instances ====

''Type declarations.''
If the program contains a type declaration including an arrow type, we need a closure-converted version of that declaration (for use in the closure converted code); e.g., for
{{{
data T = MkT (Int -> Int)
}}}
we need
{{{
data T_CC = MkT_CC (Clo Int Int)
}}}

''Classes.''
Moreover, if the closure-converted code uses a type class, we need a closure-converted type class.  It might appear that this is a non-issue on Core as classes have already been desugared to plain System F.  The trouble is that GHC does not use plain System F.  It extends it by a whole array of extra type features.  In particular, classes induce datatype declarations., and yhey almost always contain arrow types.  Hence, as we just discussed, they need to be transformed.  Matters are complicated by GHC not actually generating standalone datatypes declarations that are emitted into interface files, a class declaration siliently implies a data declaration - GHC calls this an ''iface declaration sub-binder''.  So, it appears best to create for every class a closure-converted class that - as all other class declarations - implies the closure-converted class representation datatype.

''Instances.''
When we call overloaded closure-converted functions, we need to supply closure-converted dictionaries; i.e., closure-converted dfuns.  

==== Interface files ====

How much information do we want to put into interface files?  For example, for type declarations, we can add the closure-converted declaration to the interface file or we can only add the vanilla one and have the importing modules derive the converted form on the fly during iface type checking - similar to how iface declaration sub-binders are generated on the fly, instead of being put into the interface files.

==== Mixing converted and vanilla code ====

==== An alternative: one-the-fly closure conversion ====

----
'''** OLD STUFF **'''


=== From the Skype discussion, 16 Mar 07 ===

For each function `f::ty`, create a closure-converted function `fc::ty'`, where `ty'` is the closure-converted verion of `ty`.
Optimisation: make `fc=f`, if the code for `fc` will be identical to that of `f`.  

For each data type `T`, create a closure-converted data type `Tc`, whose constructors use `Clo` instead of `->`.  Optimisation: if `Tc` is identical to `T`, don't create a new data type.
```
