CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary/CmmExceptions"
  queryString          = "?version=5"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 07:00:36 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","260"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary/CmmExceptions\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac


= Cmm: Implementing Exception Handling =

The IEEE 754 specification for floating point numbers defines exceptions for certain floating point operations, including: 
 * range violation (overflow, underflow); 
 * rounding errors (inexact); 
 * invalid operation (invalid operand, such as comparison with a `NaN` value, the square root of a negative number or division of zero by zero); and,
 * zero divide (a special case of an invalid operation).  
Many architectures support floating point exceptions by including a special register as an addition to other exception handling registers.  The IBM PPC includes the `FPSCR` ("Floating Point Status Control Register"); the Intel x86 processors use the `MXCSR` register.  When the PPC performs a floating point operation it checks for possible errors and sets the `FPSCR`.  Some processors allow a flag in the Foating-Point Unit (FPU) status and control register to be set that will disable some exceptions or the entire FPU exception handling facility.  Some processors disable the FPU after an exception has occurred while others, notably Intel's x86 and x87 processors, continue to perform FPU operations.  Depending on whether quiet !NaNs (QNaNs) or signaling !NaNs (SNaNs) are used by the software, an FPU exception may signal an interrupt for the software to pass to its own exception handler.  

Some higher level languages provide facilities to handle these exceptions, including Ada, Fortran (F90 and later), C++ and C (C99, fenv.h, float.h on certain compilers); others may handle such exceptions without exposing a low-level interface.  There are three reasons to handle FPU exceptions, and these reasons apply similarly to other exceptions: 
 * the facilities provide greater control; 
 * the facilities are efficient--more efficient than a higher-level software solution; and, 
 * FPU exceptions may be unavoidable, especially if several FPU operations are serially performed at the machine level so the higher level software has no opportunity to check the results in between operations. 

There is at least one problem in GHC that requires FPU exception handling.  See bug ticket #1042.  The bug occurs in 'show'ing the number (conversion from base_2 to base_10).  Note that the FPU exception does not occur on PowerPC machines.

There was a long message thread on the Haskell-prime mailing list, "realToFrac Issues," beginning with [http://www.haskell.org/pipermail/haskell-prime/2006-February/000791.html John Meacham's message] and ending with [http://www.haskell.org/pipermail/haskell-prime/2006-March/000840.html Simon Marlow's message].  The following code for converting a Float to a Double will ''fail'' to produce a floating point exception or NaN on x86 machines (recall that 0.0/0.0 is NaN ''and'' a definite FPU exception):

[in GHCi-6.6 on PowerPC, OS X]:
{{{
Prelude> 0.0/0.0
NaN

Prelude> realToFrac (0.0/0.0) :: Double
Infinity

Prelude> realToFrac (0.0/0.0 :: Float)
5.104235503814077e38

Prelude> realToFrac (0.0/0.0 :: Float) :: Double
5.104235503814077e38

Prelude> realToFrac (1.0/0.0)
Infinity
Prelude> realToFrac (1.0/0.0 :: Float)
3.402823669209385e38

}}}

This bug is not due to the lack of FPU exceptions in Cmm but bears mention as the internal conversion performed in 'realToFrac' on 'Float's would benefit from FPU exceptions: with Haskell-support for FPU exceptions this realToFrac would be able to issue an exception for NaN, Infinity or rounding errors when converting a Float to a Double and vice versa.  That there is a related problem with rounding errors in the functions 'encodeFloat', 'decodeFloat', 'encodeDouble' and 'decodeDouble', see [wiki:ReplacingGMPNotes/TheCurrentGMPImplementation].  


```
