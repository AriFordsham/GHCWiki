CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/KirstenSandbox"
  queryString          = "?version=3"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 07:00:03 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","254"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/KirstenSandbox\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= Building GHC on Mac OS X 10.2, by Kirsten aged 26 1/52 =

 1. grab the HEAD off darcs, per [Building/GettingTheSources] (no problems here)
 1.  run autoreconf (this went ok, because I had just upgraded my autoconf in order to build darcs)
 1.  run ./configure
 1.  oops, I only have happy 1.13, it wants happy 1.15
 1.  go to the happy download page. what, no Mac OS X binary?
 1.  grab the happy sources
 1.  ./configure, make, make install. so far so good
 1.  ./configure GHC again
 1.  LOL, I need alex 2.0
 1.  why isn't this integrated into the GHC build process? also, googling for just "alex" is un-useful. so is googling for "alex lexer" and "alex lexer haskell"
 1.  LOL, *still* no Mac OS X binary.
 1.  ./configure; make in alex
 1.  have disgusting IM conversation with friend while waiting
 1.  alex: "you lose at life":
{{{
Creating a symbolic link from alex-2.0.1 to alex in /usr/local/bin failed: `/usr/local/bin/alex' already exists
Perhaps remove `/usr/local/bin/alex' manually?
make[2]: *** [install] Error 1
make[1]: *** [install] Error 1
make: *** [install] Error 1
}}}
 1.  consider a career change
 1.  rm /usr/local/bin/alex
 1.  sudo rm /usr/local/bin/alex
 1.  sudo make me a sandwich
 1.  sudo make install
 1.  okay, I have alex. yippee.
 1.  ./configure in GHC again

```
