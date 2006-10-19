CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Building/MacOSX"
  queryString          = "?version=2"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:57:56 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","255"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Building/MacOSX\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac


= Building on MacOS X =

== Getting Readline to work ==

Thanks to Paul R Brown for the following [http://mult.ifario.us/articles/2006/10/17/ghc-6-6-and-mac-os-x-readline-quick-fix instructions].

Building GHC 6.6 out of the box on MacOS X will leave you with a GHCi binary that has no readline support.  This is because MacOS X comes with a cut-down readline library that doesn't support all the things that GHC requires, so the GHC configure script decides not to use it.

To get readline working, you first need to install GNU readline:

{{{
cd ~/work
mkdir gnu-readline
cd !$
wget ftp://ftp.cwru.edu/pub/bash/readline-5.2.tar.gz
tar xzvf readline-5.2.tat.gz
cd readline-5.2
./configure
make && sudo make install
}}}

Now you have to tell the GHC build about readline:

{{{
cd ~/work
mkdir ghc
cd !$
wget http://www.haskell.org/ghc/dist/6.6/ghc-6.6-src.tar.bz2
wget http://www.haskell.org/ghc/dist/6.6/ghc-6.6-src-extralibs.tar.bz2
tar xjvf ghc-6.6-src.tar.bz2
tar xjvf ghc-6.6-src-extralibs.tar.bz2
cd ghc-6.6
./configure --with-readline-includes=/usr/local \
            --with-readline-libraries=/usr/local
make -j && sudo make install
}}}

(`-j` tells make to spawn lots of processes building in parallel, it will probably save some time especially if you have a multi-core machine).
```
