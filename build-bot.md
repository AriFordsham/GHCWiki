CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/BuildBot"
  queryString          = "?version=3"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 07:01:32 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","250"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/BuildBot\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= Setting up a nightly build =

The GHC buildbot builds GHC on various platforms in various different ways each night, runs the test suite and performance benchmarks, and mails the results to the `cvs-ghc@haskell.org` mailing list.  We're always keen to add more build slaves to the setup, especially if you have a platform that doesn't already have a build slave, so if you'd like to join the fun, please let us know at [mailto:cvs-ghc@haskell.org].  If a platform is represented in the nightly builds, it's more likely we'll be able to identify and fix problems specific to that platform quickly.

To see the current status of the builds:

  [http://darcs.haskell.org:8010/]

== To create a new build slave ==

First you, as a buildbot client, need to agree a buildbot username (`myUser`) and password (`myPass`) with the buildbot admins (just pick a username and password and send it to `igloo@earth.il`).  You'll also need to decide:

  * when the build(s) should happen
  * HEAD or branch builds
  * full build (up to stage 3, with extra-libs, full testsuite, and 5 nofib runs) or a fast build (stage 2, no extra-libs, fast testsuite, no nofib runs), or something in-between

Finally, if there is anything special that needs to be done for the client (e.g. if gcc is in an unusual place) then you'll need to let the admins know.

Then you'll need to install buildbot and its dependencies on the machine that will be doing the nightly build; see the [http://buildbot.sourceforge.net/ BuildBot website] for details.  NB. if you're on Windows, you'll need to install BuildBot under Cygwin using the Cygwin Python; there are various problems getting the GHC build to work via BuildBot using the native Win32 Python, so we've given up on that route for now.

Now create and enter the directory you want the buildbot client to work in
{{{
$ mkdir /buildbot/ghc
$ cd /buildbot/ghc
}}}
and tell buildbot to set up a slave there
{{{
$ buildbot create-slave . darcs.haskell.org:9989 myUser myPass
}}}
This will print a few lines asking you to fill in `info/admin` and `info/host`. In the latter file, please include information on what operating system and architecture the machine is running.

It also created `Makefile.sample`; we recommend renaming this to `Makefile`. You can now start the buildbot client with `make start` and stop it with `make stop`.

You can watch what your slave is doing by looking at the `twistd.log` file in the directory in which you're running your slave.

== Admin steps ==

(for the admins only...)

Pull the buildbot master configuration:

{{{
$ darcs get buildbot@darcs.haskell.org:/home/buildbot/master
$ cd master
}}}

Edit `master.cfg`.  Add new entries to `slaves`, `schedulers`, and `builders` as necessary.  Record and push the changes.  Then restart the build master:

{{{
$ ssh buildbot@darcs.haskell.org "cd master; make reconfig"
}}}

If there is anything unusual about the machine the build is being run on, e.g. the path to `gcc` is different, then you will need to add a field for the unusual thing to !GhcDefaultConfig and alter the build steps to make use of it. Then make a special factory for the build client you are adding with this field changed as appropriate.

== Did it work? ==

Once the master is reconfiged and the client is started, the client should become visible on
http://darcs.haskell.org:8010/

At present there is no way to force an immediate test build.

```
