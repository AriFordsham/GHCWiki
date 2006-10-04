CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary/EvilMangler"
  queryString          = "?version=2"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:57:05 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","260"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary/EvilMangler\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac


= HC files and the Evil Mangler =


GHC uses {{{gcc}}} as a code generator, in a very stylised way:
 * Generate {{{Foo.hc}}}
 * Compile it with {{{gcc}}}, using {{{register}}} declarations to nail a bunch of things into registers (e.g. the allocation pointer)
 * Post-process the generated assembler code with the Evil Mangler

The Evil Mangler (EM) is a Perl script invoked by GHC after the C compiler (gcc) has translated the GHC-produced C code into assembly. Consequently, it is only of interest if {{{-fvia-C}}} is in effect (either explicitly or implicitly). 

== What the Evil Mangler does ==

The EM reads the assembly produced by gcc and re-arranges code blocks as well as nukes instructions that it considers non-essential. It derives it evilness from its utterly ad hoc, machine, compiler, and whatnot dependent design and implementation. More precisely, the EM performs the following tasks: 

 * The code executed when a closure is entered is moved adjacent to that closure's infotable. Moreover, the order of the info table entries is reversed. Also, SRT pointers are removed from closures that don't need them (non-FUN, RET and THUNK ones). 
 * Function prologue and epilogue code is removed. (GHC generated code manages its own stack and uses the system stack only for return addresses and during calls to C code.) 
 * Certain code patterns are replaced by simpler code (eg, loads of fast entry points followed by indirect jumps are replaced by direct jumps to the fast entry point). 

== Implementation of the Evil Mangler ==

The EM is located in the Perl script [[GhcFile(driver/mangler/ghc-asm.lprl)]].  The script reads the {{{.s}}} file and chops it up into chunks (that's how they are actually called in the script) that roughly correspond to basic blocks. Each chunk is annotated with an educated guess about what kind of code it contains (e.g., infotable, fast entry point, slow entry point, etc.). The annotations also contain the symbol introducing the chunk of assembly and whether that chunk has already been processed or not. 

The parsing of the input into chunks as well as recognising assembly instructions that are to be removed or altered is based on a large number of Perl regular expressions sprinkled over the whole code. These expressions are rather fragile as they heavily rely on the structure of the generated code - in fact, they even rely on the right amount of white space and thus on the formatting of the assembly. 

Afterwards, the chunks are reordered, some of them purged, and some stripped of some useless instructions. Moreover, some instructions are manipulated (eg, loads of fast entry points followed by indirect jumps are replaced by direct jumps to the fast entry point). 

The EM knows which part of the code belongs to function prologues and epilogues as STG C adds tags of the form {{{--- BEGIN ---}}} and {{{--- END ---}}} the assembler just before and after the code proper of a function starts. It adds these tags using gcc's {{{__asm__}}} feature. 

Update: Gcc 2.96 upwards performs more aggressive basic block re-ordering and dead code elimination. This seems to make the whole {{{--- END ---}}} tag business redundant -- in fact, if proper code is generated, no {{{--- END ---}}} tags survive the gcc optimiser. 


```
