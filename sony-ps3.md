CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "SonyPS3"
  queryString          = "?version=1"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 07:03:07 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","249"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access SonyPS3\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
Assuming a Yellow Dog Linux install on the Sony PS3, you can get working ghc binaries (in rpm form) for ppc from 
[http://download.fedora.redhat.com/pub/fedora/linux/extras/5/ppc/repoview/G.group.html Fedora Extras Repository]

To build from source, (assuming you have installed a ghc binary package already) you may find the default {{{./configure}}} detects the system as {{{powerpc64-unknown-linux}}}.  This blew up for me when compiling {{{StgCRun.c}}} with assembler {{{junk at end of line, first unrecognized character is `@'}}} errors.  The fix is to pass an argument to configure to force a normal powerpc build {{{./configure --build=powerpc-unknown-linux}}}.

Build time was about 8 hours (with extralibs), including a couple of hours on the penultimate source file to be compiled.
```
