CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "TypeFunctionsExamples"
  queryString          = "?version=3"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:54:16 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","258"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access TypeFunctionsExamples\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= Examples of Type Functions =

The map example from ''Associated Types with Class'' in the new form:
{{{
data family Map k :: * -> *

data instance Map ()     v = MapUnit (Maybe v)
data instance Map (a, b) v = MapPair (Map a (Map b v))
}}}
We can define operations on indexed maps using a type class whose instances corresponds to the type indexes.  Note that a declaration, such as
{{{
data instance Map Int Char = Nonsense
}}}
is not acceptable, as it constraints the second argument of `Map`, which is not a type index, but a parametric argument.



Generic finite maps:
  
{{{
  class GMapKey k where
    data GMap k :: * -> *
    empty       :: GMap k v
    lookup      :: k -> GMap k v -> Maybe v
    insert      :: k -> v -> GMap k v -> GMap k v
  
  instance GMapKey Int where
    data GMap Int v        = GMapInt (Map.Map Int v)
    empty                  = GMapInt Map.empty
    lookup k (GMapInt m)   = Map.lookup k m
    insert k v (GMapInt m) = GMapInt (Map.insert k v m)
  
  instance GMapKey Char where
    data GMap Char v        = GMapChar (GMap Int v)
    empty                   = GMapChar empty
    lookup k (GMapChar m)   = lookup (ord k) m
    insert k v (GMapChar m) = GMapChar (insert (ord k) v m)
  
  instance GMapKey () where
    data GMap () v           = GMapUnit (Maybe v)
    empty                    = GMapUnit Nothing
    lookup () (GMapUnit v)   = v
    insert () v (GMapUnit _) = GMapUnit $ Just v
  
  instance (GMapKey a, GMapKey b) => GMapKey (a, b) where
    data GMap (a, b) v            = GMapPair (GMap a (GMap b v))
    empty                               = GMapPair empty
    lookup (a, b) (GMapPair gm)   = lookup a gm >>= lookup b 
    insert (a, b) v (GMapPair gm) = GMapPair $ case lookup a gm of
                                    Nothing  -> insert a (insert b v empty) gm
                                    Just gm2 -> insert a (insert b v gm2  ) gm
  
  instance (GMapKey a, GMapKey b) => GMapKey (Either a b) where
    data GMap (Either a b) v                = GMapEither (GMap a v) (GMap b v)
    empty                                   = GMapEither empty empty
    lookup (Left  a) (GMapEither gm1  _gm2) = lookup a gm1
    lookup (Right b) (GMapEither _gm1 gm2 ) = lookup b gm2
    insert (Left  a) v (GMapEither gm1 gm2) = GMapEither (insert a v gm1) gm2
    insert (Right a) v (GMapEither gm1 gm2) = GMapEither gm1 (insert a v gm2)
}}}
  
```
