CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/AboutVideos"
  queryString          = "?version=3"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:56:58 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","252"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/AboutVideos\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= Hackathon presentations =

At the first GHC Hackathon (Portland 2006), the two Simons gave a number of informal presentations about the internals of GHC.  Since they mostly talked around the wiki material in the Commentary, I have tried to attach the videos to the relevant pages.  Here are some technical details about the video:

 * Format: !QuickTime movie
 * Size: 320x240
 * Video Codec: H.264
 * Audio Codec: AAC (MPEG-4)
 * Frame rate: 15 fps
 * Key frames: 75
 * Hinted for streaming: yes

Basically, if you have a Mac or Windows (2000 or later) PC with a recent version of iTunes (incorporates !QuickTime), then they should stream for you.  You may need to wait for 30 seconds or more before they start to play.  If you don't have iTunes, then you can download just !QuickTime for free from Apple:
    http://www.apple.com/quicktime/

The quality of the audio or (particularly) video may not be great.  They are intended mostly as a guide to the wiki commentary, so try to make sure you can see the wiki pages side by side with the video, because it may be difficult to make out the visual details otherwise.

 1. [http://video.google.com/videoplay?docid=-5234070039625162234 Intro] to the Hackathon, purpose of the event (6'47")
 1. [http://video.google.com/videoplay?docid=-948887441048207316 Documentation] - general orientation around the wiki (9'01")
 1. [http://video.google.com/videoplay?docid=7166458546326012899  Getting and Building], layout of the source tree, how to set up build.mk (23'43")
```
