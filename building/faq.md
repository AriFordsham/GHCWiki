CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Building/FAQ"
  queryString          = "?version=5"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 07:02:48 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","253"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Building/FAQ\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
[[PageOutline]]

= Frequently asked questions about building GHC =

This page collects questions and advice about building GHC from source.  It is particularly prone to going out of date.  Please help keep it relevant and accurate.  At the moment it's fairly un-structured; please feel free to add structure if that'd make it easier to follow.

Platform-specific guidance belongs under one of the platform-specific help pages linked from the [wiki:Building Building guide contents page].


== Space in TMPDIR ==

One difficulty that comes up from time to time is running out of space
in {{{TMPDIR}}}.  (It is impossible for the configuration stuff to
compensate for the vagaries of different sysadmin approaches to temp
space.)

The quickest way around it is {{{setenv TMPDIR /usr/tmp}}} or
even {{{setenv TMPDIR .}}} (or the equivalent incantation with your shell
of choice).

The best way around it is to say
{{{
export TMPDIR=<dir>
}}}
in your {{{build.mk}}} file.  Then GHC and the other
tools will use the appropriate directory in all cases.


== Warning "warning: assignment from incompatible pointer type" ==

You may occasionally see a warning from the C compiler when compiling some
Haskell code, eg. "warning: assignment from
incompatible pointer type".  These are usually harmless, but it's a good idea to
report it on the mailing list so that we can fix it.

== Warning "ar: filename `GlaIOMonad__1_2s.o` truncated to `GlaIOMonad_`" ==

Similarly, {{{ar}}}chiving warning messages like the following are not a problem:
{{{
ar: filename GlaIOMonad__1_2s.o truncated to GlaIOMonad_
ar: filename GlaIOMonad__2_2s.o truncated to GlaIOMonad_
...
}}}

== GCC 4 issues ==

It has been observed on Gentoo systems that GCC 4 may fail, complaining about there being no {{{-nopie}}} option. You can either use GCC 3, re-emerge ghc or just edit your {{{/usr/bin/ghc}}} script to remove the {{{-nopie}}} flag (the latter is by far the quickest and is perfectly safe).

== Cpp variations ==

GHC's sources go through {{{cpp}}} before being compiled, and {{{cpp}}} varies
a bit from one Unix to another.  One particular gotcha is macro calls
like this:
{{{
SLIT("Hello, world")
}}}
Some {{{cpp}}}s treat the comma inside the string as separating two macro
arguments, so you get
{{{
:731: macro `SLIT' used with too many (2) args
}}}
Alas, {{{cpp}}} doesn't tell you the offending file!
Workaround: don't put weird things in string args to {{{cpp}}} macros.


== Cabal/Distribution/Compat/FilePath.hs: No such file or directory ==

You may see this:
  {{{
Distribution/Compat/FilePath.hs:2: 
  error: Cabal/Distribution/Compat/FilePath.hs: No such file or directory
make[1]: *** [depend] Error 1
make: *** [stage1] Error 1
  }}}
'''Possible Solution'''::
Be sure you have run {{{sh darcs-all get}}} to get all necessary packages. Don't forget to run {{{sh boot}}} again after you pull in new packages.

== xargs: /usr/bin/ar: terminated by signal 11 ==

You may see this when compiling libraries:
{{{
(echo Control/Concurrent_stub.o System/CPUTime_hsc.o System/Time_hsc.o ;
/usr/bin/find Control/Applicative_split Control/Arrow_split
Control/Concurrent_split Control/Concurrent/Chan_split 
   ...long mess...
Text/PrettyPrint/HughesPJ_split Text/Printf_split Text/Read_split
Text/Read/Lex_split Text/Show_split Text/Show/Functions_split -name '*.o'
-print) | xargs /usr/bin/ar q libHSbase.a
/usr/bin/ar: creating libHSbase.a
xargs: /usr/bin/ar: terminated by signal 11
make[2]: *** [libHSbase.a] Error 125
make[2]: *** Deleting file `libHSbase.a'
make[1]: *** [all] Error 1
}}}
What is happening is that the ghc build system is linking thousands and
thousands of tiny .o files into `libHSbase.a`. GNU `ar` isn't optimised for
this use-case and it takes far more memory than it really needs to. So
what happens is that ar takes >500Mb of memory and your virtual
machine / virtual server probably isn't configured with that much memory
and so the linux kernel OOM killer terminates the ar process.

To make this worse, since there are so many .o files, it takes several
invocations of ar to link them all. On each invocation `ar` is building
the symbol index (-q is ignored) and this is what takes the most time
and memory. It's a good deal quicker to use a custom program (100 lines
of Haskell) to build `libHSbase.a` and then use `ranlib` just once to build
the symbol index.

[Duncan Coutts] I submitted a patch to gnu `binutils` to make ar take less memory when
linking 1000's of files so it now only takes around 100Mb rather than
500Mb when linking `libHSbase.a`. That patch is included in version 2.17 I
think (in other words most systems don't have it yet).

What you can do in the mean time is either configure your virtual
machine with more memory or turn off the split-objs feature when you
configure ghc. Just add `SplitObjs=NO` to your `mk/build.mk` file (which
may not exist to start with). (The Gentoo ebuild does this
automatically)



```
