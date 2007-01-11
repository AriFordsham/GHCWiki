CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/PAPI"
  queryString          = "?version=10"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 07:00:46 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","247"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/PAPI\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= Measuring program performance using CPU events =

The GHC runtime has been extended to support the use of the [http://icl.cs.utk.edu/papi/ PAPI] library to count occurrences of CPU events such as cache misses and branch mispredictions. The PAPI extension separates the events occurring in the garbage collector and mutator code for more accurate pinpointing of performance problems.

This page describes how to compile the RTS with PAPI enabled and explains the RTS options for CPU event selection. This page also contains patches to collect CPU event information in nofib runs and to allow their comparison using nofib-analyse. This is especially useful to measure the effects of optimisations accross a whole range of programs systematically.

= Status of the implementation =

GHC with PAPI support should compile on any platform where PAPI is installed. It should also be possible to monitor the cache miss events of a ghc compiled program.

At present, the monitoring of branch mispredictions and stalled cycles is AMD Opteron specific. In the case of branch mispredictions, the portable PAPI API only monitors conditional jumps. We would like to monitor all jumps, especially indirect jumps, that is why we used a native AMD PAPI counter. For strange reasons, the PAPI conditional jump counter maps to the native counter we are using, but we cannot rely on this behaviour on other platforms, so we use the native counter anyway.

= Compiling and running programs with PAPI =

First of all, make sure that you have installed the [http://icl.cs.utk.edu/papi/ PAPI library].

Follow the instructions in [wiki:Building/Hacking] and add the following line to {{{build.mk}}} before compiling the RTS:
{{{
GhcRtsWithPapi = YES
}}}

Now, to monitor and report level 1 cache misses, invoke a program compiled by ghc as follows:
{{{
./program +RTS -sstderr -a1 -RTS
}}}
The help screen provides options to monitor more events:
{{{
./program +RTS -h -RTS
}}}

= Using PAPI with the nofib benchmarking suite =

In order to use the nofib suite with PAPI, you have to use apply the three patches at the bottom of this page.

 1. The first patch adds a PAPI flag to the perl testing script.
 2. The second patch adds a make argument to the nofib suite to enable the collection of PAPI number.
 3. The third patch makes nofib-analyse able to process the output produced in the second patch. The standard nofib-analyse won't cut it.

These patches are not submitted to the HEAD (yet?) because they are not mature, but they are useful. Probably the (only?) patch that needs more work is the third one.

To collect statistics just run make inside nofib as usual, as an example let's collect statistics together with cache misses: {{{make papi=1}}}.

= Work in progress =

The PAPI framework has been used to measure the effects of the [wiki:SemiTagging semi-tagging optimisation], in particular, the effects on branch misprediction. We are currently writing a paper and cleaning up the code for this optimisation.

= Resources =

 * [http://icl.cs.utk.edu/papi/] PAPI home page.
 * [http://developer.amd.com/article_print.jsp?id=90] An article introducing the business of using CPU counters for performance measurement.
 * [http://developer.amd.com/articles.jsp?id=2&num=1] An article introducing AMD's code analyst. It even has pipeline simulation, though I haven't tried it out yet.
 * [http://www.cs.mu.oz.au/~njn/pubs.html] The Cache Behaviour of Large Lazy Functional Programs on Stock Hardware.

```
