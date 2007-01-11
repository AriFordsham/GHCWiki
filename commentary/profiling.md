CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary/Profiling"
  queryString          = "?version=5"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 07:00:42 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","259"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary/Profiling\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= Profiling =

GHC includes at least two types of profiling: cost-centre profiling and ticky-ticky profiling. Ticky-ticky profiling is currently ''not working'' in the HEAD, but hopefully should be working again soon.

Cost-centre profiling operates at something close to the source level, and ticky-ticky profiling operates at something much closer to the machine level. This means that the two types of profiling are useful for different tasks. Ticky-ticky profiling is mainly meant for compiler implementors, and cost-centre profiling for mortals. However, because cost-centre profiling operates at a high level, it can be difficult (if not impossible) to use it to profile optimized code. Personally, I (Kirsten) have had a lot of success using cost-centre profiling to find problems that were due to my own bad algorithms, but less success once I was fairly sure that I wasn't doing anything obviously stupid and was trying to figure out why my code didn't get optimized as well as it could have been.

== Cost-centre profiling ==

(add more details)

== Ticky-ticky profiling == 

(The following are my notes as I try to get this working again. Once it is working, I'll turn it into something more coherent. -krc)

Macros for bumping ticky counters are now defined in [[GhcFile(includes/Cmm.h)]]. Currently, code compiled with the {{{-fticky-ticky}}} flag fails to link because the macros rely on counter variables (things with names like {{{ENT_DYN_IND_ctr}}} being declared, but there are actually no declarations for them. I'll add those declarations to [[GhcFile(includes/RtsExternal.h)]] so I can get something working. Really, there should be something that automatically generates both the macros that are in [[GhcFile(includes/Cmm.h)]] and the declarations for the corresponding variables, so that they stay in sync.

Actually, maybe it would make more sense to add a new file, {{{RtsTicky.h}}} or something, which contains only ticky counter declarations (the same declarations that still exist in [[GhcFile(includes/StgTicky.h)]], which isn't used anymore), and that include that from [[GhcFile(includes/RtsExternal.h)]].
```
