CONVERSION ERROR

Error: HttpError (HttpExceptionRequest Request {
  host                 = "ghc.haskell.org"
  port                 = 443
  secure               = True
  requestHeaders       = []
  path                 = "/trac/ghc/wiki/Commentary/Compiler/CPS"
  queryString          = "?version=17"
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}
 (StatusCodeException (Response {responseStatus = Status {statusCode = 403, statusMessage = "Forbidden"}, responseVersion = HTTP/1.1, responseHeaders = [("Date","Sun, 10 Mar 2019 07:04:32 GMT"),("Server","Apache/2.2.22 (Debian)"),("Strict-Transport-Security","max-age=63072000; includeSubDomains"),("Vary","Accept-Encoding"),("Content-Encoding","gzip"),("Content-Length","259"),("Content-Type","text/html; charset=iso-8859-1")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}) "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n<html><head>\n<title>403 Forbidden</title>\n</head><body>\n<h1>Forbidden</h1>\n<p>You don't have permission to access /trac/ghc/wiki/Commentary/Compiler/CPS\non this server.</p>\n<hr>\n<address>Apache/2.2.22 (Debian) Server at ghc.haskell.org Port 443</address>\n</body></html>\n"))

Original source:

```trac
= CPS Conversion =

This part of the compiler is still under construction and it not presently in ghc-HEAD.
These notes are to document it for when it does get merged in.

== Overview ==
This pass takes Cmm with native proceedure calls and an implicit stack and produces Cmm with only tail calls implemented as jumps and an explicit stack.  In a word, it does CPS conversion.  (All right, so that's two words.)

== Design Aspects ==
 * Proc-Point Analysis
 * Calling Conventions
 * Live Value Analysis
 * Stack Layout

== Simple Design ==
 * Split blocks into multiple blocks at function calls
   * TODO: eliminate extra jump at block ends when there is already a jump at the end of the call
 * Do liveness analysis
 * Split every block into a separate function
 * Pass all live values as parameters (probably slow)
   * Must arrange for both the caller and callee to know argument order
     * Simple design: callee just chooses some order and all callers must comply
   * Eventually could be passed implicitly but keeping things explicit makes things easier
   * Evantually could use a custom calling convention
   * Actual syntax is probably virtual.  (I.e. in an external table, not in actual syntax because that would require changes to the type for Cmm code)
     * Input code:
       {{{
f {
  y = 1;
  z = 2;
  x = call g(a, b); // y, z live
  return x+y+z;
}
}}}
     * Output code:
       {{{
f {
  y = 1;
  z = 2;
  push_continuation h [y, z]; // Probably virtual
  jump g(a, b);
}

foreign "ret" h(x) {
  (y, z) = expand_continuation; // Probably virtual
  return x+y+z;
} 
}}}
 * Save live values before a call in the continuation
   * Must arrange for both the caller and callee to know field order
     * Simple design: callee just chooses some order and all callers must comply
   * Eventually needs to be optimized to reduce continuation shuffling
     * Can register allocation algorithms be unified with this into one framework?

== To be worked out ==
 * The continuations for {{{f}}} and {{{g}}} are different.
   {{{
if (test) {
  x = f();
} else {
  y = g();
}
}}}
   * Could make a for each that shuffles the arguments into a common format.
   * Could make one branch primary and shuffle the other to match it, but that might entail unnessisary memory writes.

== Pipeline ==
 * CPS
   * Make closures and stacks manifest
   * Makes all calls are tail calls
 * Parameter Elimination
   * Makes calling convention explicit
   * For externally visible functions calling conventions is machine specific, but not backend specific because functions compiled from different backends must be be able to call eachother
   * For local functions calling convention can be left up to the backend because it can take advantage of register allocation.
     * However, the first first draft will specify the standard calling convention for all functions even local ones because:
       * It's simpler
       * The C code generator can't handle function parameters because of the Evil Mangler
       * The NCG doesn't yet understand parameters

== TODO ==
 * Downstream
   * Argument passing convention
   * Stack check
     * Needs some way to synchronize the branch label with the heap check
 * Midstream
   * Support {{{switch}}} (needed by rts/Apply.cmm)
   * More factoring and cleanup/documentation
   * Wiki document the designed choosen
   * Better stack slot selection
   * Foreign function calls
   * Garbage collector
   * Proc points
     * May cause new blocks
     * May cause new functions
     * Lives could be passes either on stack or in arguments
   * Proc grouping of blocks
 * Upstream
   * Have {{{codeGen}}} emit C-- with functions.

== Current Pipeline ==

=== {{{cmmToRawCmm}}} ===
The {{{Cmm}}}/{{{parseCmmFile}}} pipeline and the {{{Stg}}}/{{{codeGen}}} pipeline
can each independantly use the CPS pass.
However, they currently bypass it untill the CPS code becomes stablized,
but they must both use the {{{cmmToRawCmm}}} pass.
This pass converts the header on each function from a {{{CmmInfo}}}
to a {{{[CmmStatic]}}}.

== Non-CPS Changes ==
 * Cmm Syntax Changes
   * The returns parameters of a function call must be surrounded by parenthesis.
     For example
{{{
foreign "C" fee ();
(x) = foreign "C" foo ();
(x, y) = foreign "C--" bar ();
}}}
     This is simply to avoid shift-reduce conflicts with assignment.
     Future revisions to the parser may eliminate the need for this.

   * Variable declarations may are annotated to indicate
     whether they are GC followable pointers.
{{{
W_ x; // Not GC followable
"ptr" W_ y, z; // Both GC followable
}}}
   * The bitmap of a {{{INFO_TABLE_RET}}} is now specified using
     a parameter like syntax.
{{{
INFO_TABLE_RET(stg_ap_v, RET_SMALL) { ... } // No args
INFO_TABLE_RET(stg_ap_d, RET_SMALL, D_ unused1) { ... } // Single double arg
INFO_TABLE_RET(stg_ap_np, RET_SMALL, W_ non_ptr, "ptr" W_ pointer) { ... }
  // Pointerhood indicated by "ptr" annotation
}}}
     Note that these are not real parameters, they are the stack layout
     of the continuation.  Also, until the CPS algorithm
     gets properly hooked into the {{{Cmm}}} path the parameter names are not used.
   * The return values of a function call may only be {{{LocalReg}}}.
     This is due to changes in the {{{Cmm}}} data type.

 * Cmm Data Type Changes
   * The return parameters of a {{{CmmCall}}} are {{{LocalReg}}} instead of {{{CmmReg}}}.
     This is because a {{{GlobalReg}}} doesn't have a well defined pointerhood,
     and the return values will become parameters to continuations where
     their pointerhood will be needed.
   * The type of info tables is now a separate parameter to {{{GenCmmTop}}}
     * Before
{{{
data GenCmmTop d i
  = CmmProc [d] ...
  | CmmData Section [d]
}}}
     * After
{{{
data GenCmmTop d h i
  = CmmProc h ...
  | CmmData Section [d]
}}}
     This is to support using either {{{CmmInfo}}} or {{{[CmmStatic]}}}
     as the header of a {{{CmmProc}}}.
     * Before info table conversion use {{{Cmm}}}
{{{
type Cmm = GenCmmTop CmmStatic CmmInfo CmmStmt
}}}
     * After info table conversion use {{{RawCmm}}}
{{{
type RawCmm = GenCmmTop CmmStatic [CmmStatic] CmmStmt
}}}
     Same for {{{CmmTop}}} and {{{RawCmmTop}}}.
   * New type aliases {{{CmmActuals}}}, {{{CmmFormals}}} and {{{CmmHintFormals}}}.
     Respectively these are the actual parameters of a function call,
     the formal parameters of a function, and the
     return results of a function call with pointerhood annotation
     (CPS may convert these to formal parameter of the call's continuation).

== Notes ==
 * Changed the parameter to a {{{CmmTop}}} to be {{{CmmFormals}}} instead of {{{[LocalReg]}}}
   * {{{CmmFormals}}} are {{{[(CmmReg,MachHint)]}}}
   * This field seems to not have been being used; it only require a type change
 * GC can be cleaned up b/c of the CPS
   * Before
{{{
f (x, y) {
  if (Hp + 5 > HpLim) {
    jump do_gc;
    // do_gc looks up the function type in it's info table
    // and saves a "RET_FUN" frame on the stack
    // RET_FUN fames are different than every other frame
    // in that the info table isn't on the RET_FUN code
    // but on the slot under RET_FUN which is filled with 'f'
  }
}
g (a, b, c, d, e) {
  if (Hp + 5 > HpLim) {
    jump do_gc;
    // Gen-calls have a special tag
  }
}
}}}
   * After
{{{
f (f, x, y) {
  if (Hp + 5 > HpLim) {
    jump do_gc_pp(f, x, y); // becomes jump do_gc_pp;
    // Tail call, note that arguments are in same order
    // as the arguments to 'f'
    // Also can skip the table lookup and
    // do_gc_pp can be high-level C-- and save us the
    // work of saving f, x, and y
  }
}
g (g, a, b, c, d, e) {
  if (Hp + 5 > HpLim) {
    call do_gc(g, a, b, c, d, e);
    jump g(g, a, b, c, d, e);
    // The general form will generate
    // a custom continuation
  }
}
}}}

 * We need the NCG to do aliasing analysis.  At present the CPS pass will generate the following, and will assume that the NCG can figure out when the loads and stores can be eliminated.  (The global saves part of a CmmProc is dead b/c of this.)
{{{
foo () {
  // Parameters in regs
  a = R1;
  b = R2;
  // Parameters on stack
  c = [Sp-8];
  d = [Sp-4];
  // Saved live variables
  e = [Sp+4];
  f = [Sp+8];

  /*
  ...
  Misc code that might mutate variables
  or branch or loop or any other evil thing
  ...
  */

  // A tail call (there could be multiple blocks that have this)
  a = R1;
  b = R2;
  // Parameters on stack
  c = [Sp-8];
  d = [Sp-4];
  // Saved live variables
  e = [Sp+4];
  f = [Sp+8];
  
}
}}}

 * Simple calls
   * Before
{{{
f(..., z, ...) {
  ...
  r = f(x, y);
  ...
  ... = z;
  ... = r;
}
}}}
   * Output of CPS
{{{
f() {
  z=R1
  ...
  ... = z;
  ...
  R1 = x;
  R2 = y;
  call f;
  r = R1
  ...
  ... = z;
  ... = r;
}
}}}
   * Optimization by the NCG
{{{
f() {
  ...
  ... = R1;
  ...
  z = R1;
  R1 = x;
  R2 = y;
  call f;
  ...
  ... = z;
  ... = R1;
}
}}}

== Not in Scope of Current Work ==
Improvements that could be made but that will not be implemented durring the curent effort.

=== Static Reference Table Handling (SRT) ===
As it stands, each function and thus each call site must be annotated with a bitmap and
a pointer or offset to the SRT shared by the function.
This does not interact with the stack in any way so it ought to be outside the scope of the CPS
algorithm.
However there is some level of interaction because
 1. the SRT information on each call site needs to be attached to the resulting continuation and
 2. functions read from a Cmm file might need to be annotated with that SRT info.
The first is a concern for correctness but may be handled by treating the SRT info as opaque data.
The second is a concern for ease of use and thus the likelyhood of mistakes in hand written C-- code.
At the moment it appears that all of the C-- functions in
the runtime system (RTS) use a null SRT so for now we'll just have the CPS algorithm treat the SRT info as opaque.

In the future it would be nice to have a more satisfactory way to handle both these issues.

```
