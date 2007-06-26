CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary/Rts/HaskellExecution/Registers"
  queryString          = "?version=1"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 07:04:30 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","271"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary/Rts/HaskellExecution/Registers\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac


= Haskell Excecution: Registers =

Source files: [[GhcFile(includes/Regs.h)]], [[GhcFile(includes/MachRegs.h)]]

During execution of Haskell code the following (virtual) registers are always valid:
 * `Hp` points to the byte before the first free byte in the (contiguous) allocation space.

 * `HpLim` points to the last available byte in the current chunk of allocation space (see [[ref(Heap/Stack check failures)]]).

 * `Sp` points to the youngest allocated byte of stack.  The stack grows downwards.  Why?  Because that means a return address is at a lower address than the stack frame it "knows about", and that in turn means that we can treat a stack frame very like a heap object, with an info pointer (return address) as its first word.

 * `SpLim` points to the last (youngest) available byte in the current stack.

There are bunch of other virtual registers, used for temporary argument passing, for words, floats and doubles: `R1` .. `R10`, `F1` .. `F4`, `D1` .. `D4`, `L1` .. `L2`.

In a register-rich machine, many of these virtual registers will be mapped to real registers.  In a register-poor machine, they are instead allocated in a static memory record, pointed to by a real register, `BaseReg`.

The code generator knows how many real registers there are, and tries to avoid using virtual registers that are not mapped to real registers.  So, for example, it does not use `R5` if the latter is memory-mapped; instead, it passes arguments on the stack.

```
