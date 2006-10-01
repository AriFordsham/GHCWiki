CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Building/Unregisterised"
  queryString          = "?version=2"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:57:03 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","259"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Building/Unregisterised\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac


= Unregisterised builds =

Normally GHC will try to do a so-called registerised build, where it uses various architecture and OS specific knowledge to get more efficient code. However, on an architecture where this information has not been created, or has not been kept up-to-date, it is necessary to do an unregisterised build, which uses just plain old portable C.

To do an unregisterised build, add the following to your `mk/build.mk` file:

{{{
GhcUnregisterised=YES                                                     
GhcWithNativeCodeGen=NO                                                   
GhcWithInterpreter=NO                                                     
SplitObjs=NO
}}}

Currently the native code generator requires a registerised build. GHCi seems close to working, but anything non-trivial will cause it to go wrong ([ticket: #631]). Object splitting only works when building registerised.

```
