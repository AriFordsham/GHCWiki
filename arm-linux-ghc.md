CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/ArmLinuxGhc"
  queryString          = "?version=12"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 07:00:39 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","253"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/ArmLinuxGhc\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= GHC port for arm-unknown-linux-gnu =

My goal is to create a registerised port of GHC to the nokia 770.

== Status ==

There is currently an unregisterised build available for Maemo 1.x. This project is temporarily on hold while two big transitions take place:

 1. Maemo 2.0 supports the new EABI standard which affects a bunch of things (done) (for more info see http://wiki.debian.org/ArmEabiPort )
 2. GHC 6.6 release (soon)

== Setting up the build environment ==

I have been using the standard maemo cross-development environment. Instructions for setting
up this environment can be found here:

http://www.maemo.org/platform/docs/tutorials/Maemo_tutorial.html#settingup

== Changes to standard procedure ==

The updated instructions on this page should now work:

http://www.haskell.org/ghc/docs/latest/html/building/sec-porting-ghc.html#unregisterised-porting

With two small changes:

(1) I had to add --srcdir=. 

Anyplace configure is called I get this error:

{{{
This configuration does not support the `--srcdir' option..
}}}

Adding --srcdir=. makes the error go away.

(2) ghc/Makefile SUBDIRS ordering

This has been fixed in head, but if you download the 6.4.2 release you will need to
edit ghc/Makefile and change the ordering of the SUBDIRS so that lib comes before compiler.

This is the default ordering:

{{{
ifeq "$(BootingFromHc)" "YES"
SUBDIRS = includes rts docs compiler lib utils driver
else
}}}

and you want

{{{
ifeq "$(BootingFromHc)" "YES"
SUBDIRS = includes rts docs lib compiler utils driver
else
}}}

That should get to the point of having a ghc-inplace built.

NOTE: if you try to move the directory to a new location or name,
the inplace compiler will stop working because it has absolute paths
hard coded to the current location.

== Build ghc using ghc-inplace ==

(1) unroll the ghc source tarball into a new directory.

(2) ./configure --srcdir=. --with-ghc=/abs/path/to/ghc-inplace

(3) create a mk/build.mk with these two lines:
{{{
GhcUnregisterised = YES
GhcWithNativeCodeGen = NO
}}}

(4) make

(5) make install

I think there may have been one other step in there somewhere...

This should build and install ghc. Unfortunately, the floating point
code will be broken.

== Run the test suite ==

(1) get the testsuite that corresponds to your release, for example:

http://haskell.org/ghc/dist/ghc-testsuite-6.4.2.tar.gz

(2) untar it in the ghc-6.4.2 directory.

(3) edit mk/test.mk and change the -e config.time_prog line to:
{{{
        -e config.timeout_prog=\"\" \
}}}

I had to do this because the timeout program interacted badly with
the scratchbox shell causing all the tests to timeout and fail.

(4) cd to test/ghc-regress

(5) make TEST_HC=ghc fast 

or

(5) make TEST_HC=ghc # for a longer test

= Step By Step Porting to Maemo 2.0 =

{{{
T & H

wget http://www.haskell.org/ghc/dist/6.6/ghc-6.6-src.tar.bz2
tar -xvjf ghc-6.6-src.tar.bz2
cd ghc-6.6
}}}

{{{
T

$ ./configure --enable-hc-boot --enable-hc-boot-unregisterised --srcdir=.
$ cd includes
$ make
}}}


{{{
H

$ ./configure --srcdir=.
}}}

Create H/mk/build.mk, with the following contents:

{{{
H

GhcUnregisterised = YES
GhcLibHcOpts = -O -fvia-C -keep-hc-files
GhcRtsHcOpts = -keep-hc-files
GhcLibWays =
SplitObjs = NO
GhcWithNativeCodeGen = NO
GhcWithInterpreter = NO
GhcStage1HcOpts = -O
GhcStage2HcOpts = -O -fvia-C -keep-hc-files
SRC_HC_OPTS += -H32m
GhcBootLibs = YES
}}}

Change Target* and TARGET* variables in H/mk/config.mk

{{{
H

TARGETPLATFORM			= arm-unknown-linux

TargetPlatform_CPP		= arm_unknown_linux
TargetArch_CPP			= arm

arm_unknown_linux_TARGET       = 1
arm_TARGET_ARCH      = 1
}}}

Copy T/ghc/includes/ghcautoconf.h, T/ghc/includes/DerivedConstants.h, and T/ghc/includes/GHCConstants.h to H/ghc/includes. Note that we are building on the host machine, using the target machine's configuration files. This is so that the intermediate C files generated here will be suitable for compiling on the target system.


Touch the generated configuration files, just to make sure they don't get replaced during the build:

{{{
H

$ cd H/ghc/includes
$ touch ghcautoconf.h DerivedConstants.h GHCConstants.h mkDerivedConstants.c
$ touch mkDerivedConstantsHdr mkDerivedConstants.o mkGHCConstants mkGHCConstants.o
}}}

I just followed the guide upto making the hc bundle. I had to comment out this line in H/Makefile:

{{{
#	echo ghc-$(ProjectVersion)/libraries/haskell-src/Language/Haskell/Parser.hs >> hc-files-to-go
}}}

because that file does not seem to exist anymore.

== Wrong Stuff ==

Oops, I mis-read the directions, so this next section is junk.

Build the compiler on the host. There seems to be a circular depends between utils and compat so I had to hack it a bit. First edit H/utils/Makefile and remove ghc-pkg from the SUBDIRS list in the else clause.

{{{
H

else
SUBDIRS = mkdependC mkdirhier runstdtest hasktags hp2ps hsc2hs \
	  parallel prof unlit genprimopcode genapply runghc
endif
}}}

Then run 'make boot' in the utils directory
{{{
H

$ cd H/ghc-6.6/utils
$ make boot
}}}

Now restore ghc-pkg to the SUBDIRS line:

{{{
H

else
SUBDIRS = mkdependC mkdirhier runstdtest ghc-pkg hasktags hp2ps hsc2hs \
	  parallel prof unlit genprimopcode genapply runghc
endif
}}}

And build H/ghc-6.6/compat and then utils:

{{{
H

$ cd H/ghc-6.6/compat
$ make boot && make
$ cd H/ghc-6.6/utils
$ make boot && make
}}}

```
