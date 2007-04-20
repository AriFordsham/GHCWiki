CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Building/GettingTheSources"
  queryString          = "?version=10"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 07:02:57 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","262"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Building/GettingTheSources\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac


[http://video.google.com/videoplay?docid=7166458546326012899  Video: Getting and Building], layout of the source tree, how to set up build.mk (23'43")

= Getting the GHC Sources =

There are two ways to get sources to GHC: download a source distribution, or get the sources directly from our repository using [http://darcs.net/ darcs].

== Source distributions ==

A source distribution is a file like {{{ghc-6.6-src.tar.bz2}}}, which contains a complete snapshot of the source tree for a particular version of GHC.  Source distributions for all versions of GHC are available from the [http://www.haskell.org/ghc/download.html download page].

Starting with GHC 6.6, we have split the source distribution in two:

 * {{{ghc-<version>-src.tar.bz2}}} contains GHC itself and the minimum libraries needed to bootstrap GHC.
 * {{{ghc-<version>-extralibs.tar.bz2}}} contains a selection of supplemental libraries that can be built
   and installed at the same time as GHC.  Just unpack this on top of {{{ghc-<version>-src.tar.bz2}}}, and
   the extra libraries will be built automatically.

In addition to fixed releases of GHC, source distributions are also made each night from the current source repository, for both the HEAD and STABLE branches.  To download these snapshots, head over to the [http://www.haskell.org/ghc/download.html download page].

Source distributions are easier to build, because we also include the output from running certain external tools like [http://haskell.org/happy Happy], so you don't need to install these tools.  See [wiki:Building/Prerequisites] for details.

== Getting a GHC source tree using darcs ==

The first thing to do is install [http://darcs.net/ darcs].

A source tree consists of the GHC repository, with a set of packages in the libraries directory.  We supply a script to automate the checking out of packages, {{{darcs-all}}}.  Checking out a tree goes like this:

{{{
  $ darcs get --partial http://darcs.haskell.org/ghc
  $ cd ghc
  $ chmod +x darcs-all
  $ ./darcs-all get
}}}

'''NOTE''': You really want {{{--partial}}} when grabbing GHC.  There are some 15000 patches in the repository, which take a long time to download without {{{--partial}}}.  The {{{darcs-all}}} script automatically adds {{{--partial}}} for the packages.  However, if you are a developer and intend to make changes to your GHC source tree, then we recommend ''not'' using `--partial`, and adding `--complete` to the `darcs-all` command-line which disables its default use of `--partial`.  We avoid `--partial` when developing due to bugs in darcs that affect moving patches between partial repositories.  Getting GHC without `--partial` may take a while, so we occasionally make tarballs of the full GHC repo, which you can look for in [http://darcs.haskell.org/ here] (look for files named `ghc-HEAD-<date>.tar.bz2`).

The above will grab the "core" set of packages.  This is the minimal set of packages required to bootstrap GHC.  If you want to get a more comprehensive set of packages and include them in your GHC build, then you can say:

{{{
  $ ./darcs-all --extra get
}}}

This isn't usually necessary: extra packages can be compiled and installed separately using Cabal, after you have built and installed GHC itself with its core packages.  The list of "core" and "extra" packages is below.

Optionally, you might want to grab the testsuite and benchmark suite too, which should also be sub-directories of ghc:

{{{
  $ darcs get --partial http://darcs.haskell.org/testsuite
  $ darcs get --partial http://darcs.haskell.org/nofib
}}}

These can also be fetched with flags to {{{darcs-all}}}:
{{{
  $ ./darcs-all --testsuite get
  $ ./darcs-all --nofib get
}}}

The full list of darcs repositories relating to GHC is at DarcsRepositories.

=== Getting a branch ===

The above instructions will get the HEAD - the main trunk of GHC development.  There are also branches, from which stable releases are made.  The active branches are listed on DarcsRepositories.

To get a branch, add the branch name after http://darcs.haskell.org/.  For example, to get the `ghc-6.6` branch, you would first say 

{{{
  $ darcs get --partial http://darcs.haskell.org/ghc-6.6/ghc
}}}

and then use `darcs-all` as above to get the rest of the respositories.

To get testsuite and/or nofib, you'll need to name the branch repositories:

{{{
  $ darcs get --partial http://darcs.haskell.org/ghc-6.6/testsuite
  $ darcs get --partial http://darcs.haskell.org/ghc-6.6/nofib
}}}

=== Pulling new patches ===

To update your tree from the master repositories, the quickest way is to use the {{{darcs-all}}} script:

{{{
  $ ./darcs-all pull -a
}}}

See [wiki:Building/Rebuilding] for how to update your build after pulling patches.

```
