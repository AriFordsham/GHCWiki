CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/WorkingConventions"
  queryString          = "?version=5"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:58:22 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","256"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/WorkingConventions\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
== Coding conventions ==

There are some documents on coding style:

 * [wiki:Commentary/CodingStyle Coding style in the compiler]
 * [wiki:Commentary/Rts/Conventions Coding style in the runtime system]

== Submitting patches ==

To submit patches to the developers, please use {{{darcs send}}}.  You don't need any special permission to do this.

== Committing changes ==

If you have permission to push patches directly to the repository (pretty easy to get, just demonstrate your competence by sending us a patch or two first), then you can use {{{darcs push}}}:

{{{
  $ darcs push <account>@darcs.haskell.org:/home/darcs/ghc
}}}

(change {{{ghc}}} to the name of the repository if you're pushing changes from one of the sub-repositories, like {{{testsuite}}}, or a package such as {{{base}}}.  Note: {{{darcs push}}} requires that SSH is working and can log in to your account on {{{darcs.haskell.org}}}.

Do not forget to {{{darcs record}}} your changes first!

=== When to record/push ===

Guidelines for pushing patches to GHC:

 * We have separate guidelines for proposing changes to standard libraries; see [http://haskell.org/haskellwiki/Library_submissions Library Submissions].

 * Try to make small patches (i.e. work in consistent increments).

 * Separate changes that affect functionality from those that just affect
   code layout, indendation, whitespace, filenames etc.  This means that
   when looking at patches later, we don't have to wade through loads of
   non-functional changes to get to the important parts of the patch.   

 * If possible, push often.  This helps to avoid conflicts.

 * Rather than push conflicting patches followed by conflict resolutions, use
   amend-record (or unrecord/edit/record) to make a single patch.  Darcs currently
   doesn't handle conflicts
   well, so we are trying to keep the HEAD clean of conflicts for now.  It doesn't
   matter so much on the stable branches though.

 * Try not to break anything.  At the minimum, the tree should build on your system with
   the patch, better still [wiki:Building/RunningTests test your changes] before
   pushing.  The nightly builds will show up any breakage on other platforms.
   [[br]][[br]]
   If you do end up breaking the build then it's not the end of the world,
   so don't sweat about it too much. History shows that even people
   called Simon are not immune from doing so!

 * Discuss anything you think might be controversial before pushing it.



```
