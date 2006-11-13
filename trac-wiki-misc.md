CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/TracWikiMisc"
  queryString          = "?version=2"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:58:59 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","251"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/TracWikiMisc\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= Notes on Trac Wiki formatting =

The main guide to the Trac Wiki formatting is [wiki:WikiFormatting Wiki Formatting].  The notes on this page are the GHC team's observations and/or clarifications.

== Formatting list items with multiple paragraphs ==

TracWiki recognizes list items by their indentation and the asterisk preceding the first line. However if a second paragraph is started after an empty line, TracWiki will not consider it part of the list item despite the use of indentation. The "proper" way to do it is to separate the second and subsequent paragraphs by line break macros. For example:

{{{
 * A list item paragraph consists of a leading asterisk
   and indented text lines.
   [[br]][[br]]
   Using line breaks we can start another paragraph.
   [[br]][[br]]
   And another
}}}

----
 * A list item paragraph consists of a leading asterisk
   and indented text lines.
   [[br]][[br]]
   Using line breaks we can start another paragraph.
   [[br]][[br]]
   And another

----

== Formatting list items with multiple paragraphs and nested lists ==

Unfortunately the break line trick does not work for list items that contain nestes lists. Consider for example:

{{{
 * A list item paragraph consists of a leading asterisk
   and indented text lines.
   [[br]][[br]]
    * A nested list starts by adding additional space
      for the next list item.
    * Hey this one has another list element.
   [[br]][[br]]
   See how this line gets added to the wrong list?
}}}

----
 * A list item paragraph consists of a leading asterisk
   and indented text lines.
   [[br]][[br]]
    * A nested list starts by adding additional space
      for the next list item.
    * Hey this one has another list element.
   [[br]][[br]]
   See how this line gets added to the wrong list?
----

For the moment (until TracWiki is fixed) the paragraphs following the nested list maybe formated as quotes.
Quotes happen to have the same indentation is list. Ugly, yes, but good enough for now.
{{{
 * A list item paragraph consists of a leading asterisk
   and indented text lines.
   [[br]][[br]]
    * A nested list starts by adding additional space
      for the next list item.
    * Hey this one has another list element.
  See how this line starts at column two instead of column three?
}}}

----
 * A list item paragraph consists of a leading asterisk
   and indented text lines.
   [[br]][[br]]
    * A nested list starts by adding additional space
      for the next list item.
    * Hey this one has another list element.
  See how this line starts at column two instead of column three?
----

== Links to page sections ==

The GHC TracWiki provides the {{{ref}}} macro to link to sections of a page.
To link to the previous section just invoke the ref macro as follows:

{{{
[[ref(Formatting list items with multiple paragraphs and nested lists)]]
}}}

----
[[ref(Formatting list items with multiple paragraphs and nested lists)]]
----

== Links to sections of different pages ==

For the moment there is no such macro to link to sections of other pages.
What you should do is to take note of the anchor that appears in the HTML source,
and add the anchor to a wiki link right after a hash:

{{{
[wiki:Building/Using#StandardTargets standard targets]
}}}
----
[wiki:Building/Using#StandardTargets standard targets]
----
```
