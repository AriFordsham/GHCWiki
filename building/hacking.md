CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Building/Hacking"
  queryString          = "?version=9"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:57:17 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","255"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Building/Hacking\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac


[http://video.google.com/videoplay?docid=7166458546326012899  Video: Getting and Building], layout of the source tree, how to set up build.mk (23'43")

= Quick start for developers =

This section is for those who want to do more than just build & install GHC.  It
is for those who want to actually modify parts of GHC, and perhaps distribute those
modifications to others.  This section contains a few nuggets of information
that will help get you started right away.  For more detailed documentation
on the build system, read on to the later sections.

== Setting up your build ==

The GHC build tree is set up so that, by default, it builds a compiler
ready for installing and using.  That means full optimisation, and the
build can take a ''long'' time.  If you unpack your source tree and
right away say {{{./configure; make}}}, expect to have to wait a while.

For hacking, you want the build to be quick - quick to build in the
first place, and quick to rebuild after making changes.  Tuning your
build setup can make the difference between several hours to build
GHC, and less than an hour.  Here's how to do it.

If you're just interested in working on GHC, then you probably don't want
the "extralibs" libraries that we normally ship with GHC, having these in your
source tree will just make the build take longer.  So when 
[wiki:Building/GettingTheSources getting the sources], run `darcs-all` without
the `--extra` option.

{{{mk/build.mk}}} is a GNU makefile that contains all your build settings.
By default, this file doesn't exist, and all the parameters are set to
their defaults in {{{mk/config.mk}}} ({{{mk/config.mk}}} is the place to look for
''all'' the things you might want to tune).

A good {{{mk/build.mk}}} to start hacking on GHC is:

{{{
SRC_HC_OPTS     = -H32m -O -fasm -Rghc-timing
GhcStage1HcOpts = -O0 -DDEBUG
GhcLibHcOpts    = -O -fgenerics
GhcLibWays      =
SplitObjs       = NO
}}}

What do these options do?

 {{{SRC_HC_OPTS = -H32m -O -fasm -Rghc-timing}}}::
  These options are added to the command line for all Haskell
  compilations.  We turn on {{{-fasm}}}, because that halves compilation
  time at the expense of a few percent performance.  {{{-Rghc-timing}}}
  prints out a line of timing info about each compilation.  It's handy
  to keep an eye on.

 {{{GhcStage1HcOpts = -O0 -DDEBUG}}}::
  The options for building the stage1 compiler (these come after
  SRC_HC_OPTS, so you can override settings from there).  We turn off
  optimisation here, assuming you'll be modifying and testing stage1.
  With optimisation off, rebuilding GHC after modifying it will be
  ''much'' quicker, not only because the individual compilations will be
  quicker, but also there will be fewer dependencies between modules,
  so less stuff needs to be rebuilt after each modification.
  [[br]][[br]]
  Also we turn on {{{-DDEBUG}}}, because that enables assertions and
  debugging code in the compiler itself.  Turning on DEBUG makes
  the compiler about 30% slower.

 {{{GhcLibHcOpts = -O -fgenerics}}}::
  You almost certainly want optimisation ''on'' when building
  libraries, otherwise the code you build with this compiler
  goes really slowly.  {{{-fgenerics}}} add generics support to the
  libraries - you can turn this off if you like (it'll make the
  libraries a bit smaller), but you won't be able to use Generics in
  the code you build against these libraries.

 {{{GhcLibWays =}}}::
  Normally the profiled libs are built.  Setting {{{GhcLibWays}}} to
  empty disables this, so you only build the normal libs.

 {{{SplitObjs = NO}}}::
  Object splitting causes each module to be split into smaller
  pieces in the final library, to reduce executable sizes when
  linking against the library.  It can be quite time and
  memory-consuming, so turn it off when you're hacking.


== Actually building the bits ==

To just build everything, from the top level:

{{{
  $ autoreconf
  $ ./configure
  $ make
}}}

NB. that's auto'''re'''conf, not just `autoconf`.  The former
works recursively, which is necessary because the GHC tree contains
multiple configure scripts.

(See here for [wiki:Building/Problems what can go wrong].)

If you just want to build stage 1, then instead you can say

{{{
  $ make stage1
}}}

but note that the stage 1 compiler doesn't support GHCi or Template Haskell, those are compiled into stage 2 only (see 
[wiki:Building/Using#BootstrappingGHC BootstrappingGHC]).

To install the compiler you built, you can say

{{{
  $ make install
}}}

However, you don't need to install GHC to use it.  Running `./compiler/stage1/ghc-inplace` from the build tree
will invoke the stage1 compiler, and `./compiler/stage2/ghc-inplace` will invoke the stage2 compiler.

== Building individual parts of the tree ==

The first thing to understand is that the source tree is built in two
passes.  First {{{make boot}}} builds dependencies and any other tools
required as part of the build itself.  For example,
{{{utils/genprimopcode}}} is built as part of {{{make boot}}}, because it is
required to preprocess {{{compiler/prelude/primops.txt.pp}}}.

After {{{make boot}}}, {{{make}}} will build everything.

If you say {{{make}}} from the very top-level, the build system will
arrange to do the appropriate 'make boot' steps for you.  If you just
want to build in a subdirectory (eg. ghc), you have to do {{{make boot}}}
yourself.  You don't need to {{{make boot}}} after every single change,
but you might want to do it to update dependencies, for example.


== Refining the setup ==

If you will be hacking mostly on libraries, then you probably want to
build stage1 with optimisation, because you're only building it once
but using it many times.

{{{
  GhcStage1HcOpts = -O
}}}

If you are working on GHCi or Template Haskell, then you will be
building and modifying the stage 2 compiler.  Hence, you want to build
stage 1 with, and stage 2 without, optimisation.

{{{
  GhcStage1HcOpts = -O
  GhcStage2HcOpts = -O0 -DDEBUG
}}}

Take a look through {{{mk/config.mk}}} for more settings you might want to
override in build.mk.  Remember: don't modify {{{config.mk}}} directly (it
gets overwritten when you run {{{./configure}}}).


== Full optimisation ==

To turn up everything to the max, for running performance tests for
example, try these:

{{{
  SRC_HC_OPTS  = -H64m -O2 
  GhcLibHcOpts = -O2
  SplitObjs    = YES
}}}

You can even add some more aggresive options, such as
{{{-fliberate-case-threshold50}}}, {{{-funfolding-use-threshold50}}}.

Here is a [wiki:Commentary/SourceTree roadmap to the source tree].

```
