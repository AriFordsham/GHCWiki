CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Building/RunningNoFib"
  queryString          = "?version=3"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:59:02 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","259"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Building/RunningNoFib\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= The !NoFib Benchmark Suite =

The !NoFib benchmark suite is a collection of (mostly old) Haskell programs that we use for benchmarking GHC.  The !NoFib suite is kept in a separate darcs repository (see DarcsRepositories), and it should be checked out at the top level of a GHC source tree, i.e. at the same level as `compiler` and `libraries`.

To run the tests:

{{{
  $ cd nofib
  $ make clean
  $ make boot
  $ make 2>&1 | tee nofib-log
}}}

will put the results in the file `nofib-log`.

To compare the results of multiple runs, use the program in
[[GhcFile(utils/nofib-analyse)]].  Something like this:

{{{
  $ nofib-analyse nofib-log-6.4.2 nofib-log-6.6
}}}

to generate a comparison of the runs in captured in `nofib-log-6.4.2`
and `nofib-log-6.6`.  When making comparisons, be careful to ensure
that the things that changed between the builds are only the things
that you ''wanted'' to change.  There are lots of variables: machine,
GHC version, GCC version, C libraries, static vs. dynamic GMP library,
build options, run options, and probably lots more.  To be on the safe
side, make both runs on the same unloaded machine.

To get instruction counts, memory reads/writes, and "cache misses",
you'll need to get hold of Cachegrind, which is part of 
[http://valgrind.org Valgrind].

There are some options you might want to tweak; search for nofib in
[[GhcFile(mk/config.mk)]], and override settings in `mk/build.mk` as usual.

```
