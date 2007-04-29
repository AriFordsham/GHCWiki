CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Status"
  queryString          = "?version=6"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 07:03:14 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","248"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Status\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= GHC Status =

== Biannual status reports ==

Here are biannual GHC status reports, published in the [http://haskell.org/communities/ Haskell Communities and Activities Report]
  * [wiki:Status/April07 GHC status April 2007] (draft)
  * [wiki:Status/October06 GHC status October 2006]

== Performance ==

Here are the [http://www.cse.unsw.edu.au/~dons/nobench/i686/results.html results of the nobench suite], showing how GHC performs relative to various versions of itself, and other Haskell compilers.

== Planning notes ==

Here is our [wiki:GhcPlanning current planning document], intended mainly to help Simon, Simon, and Ian coordinate with each other.

== Design notes ==

Here are notes about aspects of GHC's design that are up for discussion.  (They often relate to Haskell generally, rather than just GHC, but it's a convenient place to keep them.)
  * [wiki:PackageReorg Haskell library organisation]
  * [wiki:GhcPackages The design for the package system itself]
  * [wiki:ExplicitCallStack Support for better error reporting]; the "head []" problem.
 

```
