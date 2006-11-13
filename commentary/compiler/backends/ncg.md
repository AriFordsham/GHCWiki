CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary/Compiler/Backends/NCG"
  queryString          = "?version=4"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 06:59:06 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","265"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary/Compiler/Backends/NCG\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= Native Code Generator =

For other information related to this page, see:
 * [http://www.cse.unsw.edu.au/~chak/haskell/ghc/comm/the-beast/ncg.html the Old GHC Commentary: Native Code Generator] page (comments regarding Maximal Munch and register allocation optimisations are mostly still valid)
 * [wiki:BackEndNotes] for optimisation ideas regarding the current NCG

=== Overview: Files, Phases ===

After GHC has produced Cmm (use -ddump-cmm or -ddump-opt-cmm to view), the Native Code Generator (NCG) transforms [wiki:Commentary/Compiler/CmmType Cmm] into architecture-specific assembly code.  The NCG is located in [[GhcFile(compiler/nativeGen)]] and is separated into eight modules:

 * [[GhcFile(compiler/nativeGen/AsmCodeGen.lhs)]][[BR]]
   top-level module for the NCG, imported by [[GhcFile(compiler/main/CodeOutput.lhs)]]; also defines the Monad for optimising generic Cmm code, {{{CmmOptM}}}[[BR]][[BR]]
 * [[GhcFile(compiler/nativeGen/MachCodeGen.hs)]][[BR]]
   generates architecture-specific instructions (a Haskell-representation of assembler) from Cmm code[[BR]][[BR]]
 * [[GhcFile(compiler/nativeGen/MachInstrs.hs)]][[BR]]
   contains data definitions and some functions (comparison, size, simple conversions) for machine instructions, mostly carried out through the {{{Instr}}} data type, defined here[[BR]][[BR]]
 * [[GhcFile(compiler/nativeGen/NCGMonad.hs)]][[BR]]
   defines the the main monad in the NCG: the Native code Machine instruction Monad, {{{NatM}}}, and related functions.  ''Note: the NCG switches between two monads at times, especially in {{{AsmCodeGen}}}: {{{NatM}}} and the {{{UniqSM}}} Monad used throughout the compiler.''[[BR]][[BR]]
 * [[GhcFile(compiler/nativeGen/PositionIndependentCode.hs)]][[BR]]
   handles generation of position independent code and issues related to dynamic linking in the NCG; related to many other modules outside the NCG that handle symbol import, export and references, including {{{CLabel}}}, {{{Cmm}}}, {{{codeGen}}} and the RTS, and the Mangler[[BR]][[BR]]
 * [[GhcFile(compiler/nativeGen/PprMach.hs)]][[BR]]
   Pretty prints machine instructions ({{{Instr}}}) to assembler code (currently readable by GNU's {{{as}}}), with some small modifications, especially for comparing and adding floating point numbers on x86 architectures[[BR]][[BR]]
 * [[GhcFile(compiler/nativeGen/RegAllocInfo.hs)]][[BR]]
   defines the main register information function, {{{regUsage}}}, which takes a set of real and virtual registers and returns the actual registers used by a particular {{{Instr}}}; register allocation is in AT&T syntax order (source, destination), in an internal function, {{{usage}}}; defines the {{{RegUsage}}} data type[[BR]][[BR]]
 * [[GhcFile(compiler/nativeGen/RegisterAlloc.hs)]][[BR]]
   one of the most complicated modules in the NCG, {{{RegisterAlloc}}} manages the allocation of registers for each ''basic block'' of Haskell-abstracted assembler code: management involves ''liveness'' analysis, allocation or deletion of temporary registers, ''spilling'' temporary values to the ''spill stack'' (memory) and many other optimisations.  ''Note: much of this detail will be described later; '''basic block''' is defined below.''

and one header file:

 * [[GhcFile(compiler/nativeGen/NCG.h)]][[BR]]
   defines macros used to separate architecture-specific code in the Haskell NCG files; since GHC currently only generates machine code for the architecture on which it was compiled (GHC is not currently a cross-compiler), the Haskell NCG files become considerably smaller after preprocessing; ideally all architecture-specific code would reside in separate files and GHC would have them available to support cross-compiler capabilities.

The NCG runs through two main phases: a '''machine-independent''' phase and a '''machine-dependent''' phase.  

The '''machine-independent''' phase begins with ''Cmm blocks.''  A ''Cmm block'' is roughly parallel to a Cmm function or procedure in the same way as a compiler may generate a C function into a block of assembler instructions.  ''Cmm block''s are held as lists of {{{Cmm}}} statements ({{{[CmmStmt]}}}, defined in [[GhcFile(compiler/cmm/Cmm.hs)]], or {{{type CmmStmts}}}, defined in [[GhcFile(compiler/cmm/CmmUtils.hs)]]).  A machine-specific (assembler) instruction is represented as a {{{Instr}}}. During this phase:
 1. each Cmm block is ''lazily'' converted to abstract machine instructions ({{{Instr}}}) operating on an infinite number of registers--since the NCG Haskell files only contain instructions for the host computer on which GHC was compiled, these {{{Instr}}} are machine-specific;[[BR]][[BR]]
 1. for each ''basic block'' (a, contiguous block of instructions with no branches (jumps) in each ''{{{Cmm}}} block''), real registers are ''lazily'' allocated based on the number of available registers on the target machine (say, 32 integer and 32 floating-point registers on the PowerPC architecture).[[BR]]''Note'': if a basic block simultaneously requires more registers than are available on the target machine and the temporary variable needs to be used (would sill be ''live'') after the current instruction, it will be moved (''spilled'') into memory; and,[[BR]][[BR]]
 1. each Cmm block is optimised by reordering its basic blocks from the original order (the {{{Instr}}} order from the {{{Cmm}}}) to minimise the number of branches between basic blocks, in other words, by maximising fallthrough of execution from one basic block to the next.


```
