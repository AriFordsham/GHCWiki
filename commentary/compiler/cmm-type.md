### Note To Reader


This page was written with more detail than usual since you may need to know how to work with Cmm as a programming language.  Cmm is the basis for the future of GHC, Native Code Generation, and if you are interested in hacking Cmm at least this page might help reduce your learning curve.  As a finer detail, if you read the [Compiler pipeline](commentary/compiler/hsc-main) wiki page or glanced at the diagram there you may have noticed that whether you are working backward from an `intermediate C` (Haskell-C "HC", `.hc`) file or an Assembler file you get to Cmm before you get to the STG language, the Simplifier or anything else.  In other words, for really low-level debugging you may have an easier time if you know what Cmm is about.  Cmm also has opportunities for implementing small and easy hacks, such as little optimisations and implementing new Cmm Primitive Operations.


A portion of the [RTS](commentary/rts) is written in Cmm: [rts/Apply.cmm](/trac/ghc/browser/ghc/rts/Apply.cmm), [rts/Exception.cmm](/trac/ghc/browser/ghc/rts/Exception.cmm), [rts/HeapStackCheck.cmm](/trac/ghc/browser/ghc/rts/HeapStackCheck.cmm), [rts/PrimOps.cmm](/trac/ghc/browser/ghc/rts/PrimOps.cmm), [rts/StgMiscClosures.cmm](/trac/ghc/browser/ghc/rts/StgMiscClosures.cmm), [rts/StgStartup.cmm](/trac/ghc/browser/ghc/rts/StgStartup.cmm) and [StgStdThunks.cmm](/trac/ghc/browser/ghc/StgStdThunks.cmm).  (For notes related to `PrimOps.cmm` see the [PrimOps](commentary/prim-ops) page; for much of the rest, see the [HaskellExecution](commentary/rts/haskell-execution) page.)  Cmm is optimised before GHC outputs either HC or Assembler.  The C compiler (from HC, pretty printed by [compiler/cmm/PprC.hs](/trac/ghc/browser/ghc/compiler/cmm/PprC.hs)) and the [Native Code Generator](commentary/compiler/backends/ncg) (NCG) [Backends](commentary/compiler/backends) are closely tied to data representations and transformations performed in Cmm.  In GHC, Cmm roughly performs a function similar to the intermediate [ Register Transfer Language (RTL)](http://gcc.gnu.org/onlinedocs/gccint/RTL.html) in GCC.

# Table of Contents

1. [Additions in Cmm](commentary/compiler/cmm-type#additions-in-cmm)
1. [Compiling Cmm with GHC](commentary/compiler/cmm-type#compiling-cmm-with-ghc)
1. [Basic Cmm](commentary/compiler/cmm-type#basic-cmm)

  1. [Code Blocks in Cmm](commentary/compiler/cmm-type#code-blocks-in-cmm)

    - [Basic Blocks and Procedures](commentary/compiler/cmm-type#basic-blocks-and-procedures)
  1. [Variables, Registers and Types](commentary/compiler/cmm-type#)

    1. [Local Registers](commentary/compiler/cmm-type#)
    1. [Global Registers and Hints](commentary/compiler/cmm-type#)
    1. [Declaration and Initialisation](commentary/compiler/cmm-type#)
    1. [Memory Access](commentary/compiler/cmm-type#)
  1. [Literals and Labels](commentary/compiler/cmm-type#)

    - [Labels](commentary/compiler/cmm-type#)
  1. [Sections and Directives](commentary/compiler/cmm-type#)

    - [Target Directive](commentary/compiler/cmm-type#)
  1. [Expressions](commentary/compiler/cmm-type#)

    - [Quasi-operator Syntax](commentary/compiler/cmm-type#)
  1. [Statements and Calls](commentary/compiler/cmm-type#)

    - [Cmm Calls](commentary/compiler/cmm-type#)
  1. [Operators and Primitive Operations](commentary/compiler/cmm-type#)

    1. [Operators](commentary/compiler/cmm-type#)
    1. [Primitive Operations](commentary/compiler/cmm-type#)
1. [Cmm Design: Observations and Areas for Potential Improvement](commentary/compiler/cmm-type#)

# The Cmm language

`Cmm` is the GHC implementation of the `C--` language; it is also the extension of Cmm source code files: `.cmm` (see [What the hell is a .cmm file?](commentary/rts/cmm)).  The GHC [Code Generator](commentary/compiler/code-gen) (`CodeGen`) compiles the STG program into `C--` code, represented by the `Cmm` data type.  This data type follows the [ definition of \`C--\`](http://www.cminusminus.org/) pretty closely but there are some remarkable differences.  For a discussion of the Cmm implementation noting most of those differences, see the [Basic Cmm](commentary/compiler/cmm-type#basic-cmm) section, below.

- [compiler/cmm/Cmm.hs](/trac/ghc/browser/ghc/compiler/cmm/Cmm.hs): the main data type definition.
- [compiler/cmm/MachOp.hs](/trac/ghc/browser/ghc/compiler/cmm/MachOp.hs): data types defining the machine operations (e.g. floating point divide) provided by `Cmm`.
- [compiler/cmm/CLabel.hs](/trac/ghc/browser/ghc/compiler/cmm/CLabel.hs): data type for top-level `Cmm` labels.

- [compiler/cmm/PprCmm.hs](/trac/ghc/browser/ghc/compiler/cmm/PprCmm.hs): pretty-printer for `Cmm`.
- [compiler/cmm/CmmUtils.hs](/trac/ghc/browser/ghc/compiler/cmm/CmmUtils.hs): operations over `Cmm`

- [compiler/cmm/CmmLint.hs](/trac/ghc/browser/ghc/compiler/cmm/CmmLint.hs): a consistency checker.
- [compiler/cmm/CmmOpt.hs](/trac/ghc/browser/ghc/compiler/cmm/CmmOpt.hs): an optimiser for `Cmm`.

- [compiler/cmm/CmmParse.y](/trac/ghc/browser/ghc/compiler/cmm/CmmParse.y), [compiler/cmm/CmmLex.x](/trac/ghc/browser/ghc/compiler/cmm/CmmLex.x): parser and lexer for [.cmm files](commentary/rts/cmm).

- [compiler/cmm/PprC.hs](/trac/ghc/browser/ghc/compiler/cmm/PprC.hs): pretty-print `Cmm` in C syntax, when compiling via C.

## Additions in Cmm


Although both Cmm and C-- allow foreign calls, the `.cmm` syntax includes the 

```wiki
foreign "C" cfunctionname(R1) [R2];
```


The \[R2\] part is the (set of) register(s) that you need to save over the call.


Other additions to C-- are noted throughout the [Basic Cmm](commentary/compiler/cmm-type#basic-cmm) section, below.

## Compiling Cmm with GHC


GHC is able to compile `.cmm` files with a minimum of user-effort.  To compile `.cmm` files, simply invoke the main GHC driver but remember to:

- add the option `-dcmm-lint` if you have handwritten Cmm code;
- add appropriate includes, especially [includes/Cmm.h](/trac/ghc/browser/ghc/includes/Cmm.h) if you are using Cmm macros or GHC defines for certain types, such as `W_` for `bits32` or `bits64` (depending on the machine word size)--`Cmm.h` is in the `/includes` directory of every GHC distribution, i.e., `usr/local/lib/ghc-6.6/includes`; and,
- if you do include GHC header files, remember to pass the code through the C preprocessor by adding the `-cpp` option.


For additional fun, you may pass GHC the `-keep-s-file` option to keep the temporary assembler file in your compile directory.
For example:

```wiki
ghc -cpp -dcmm-lint -keep-s-file -c Foo.cmm -o Foo.o
```


This will only work with very basic Cmm files.  If you noticed that GHC currently provides no `-keep-cmm-file` option and `-keep-tmp-files` does not save a `.cmm` file and you are thinking about redirecting output from `-ddump-cmm`, beware. The output from `-ddump-cmm` contains equal-lines and dash-lines separating Cmm Blocks and Basic Blocks; these are unparseable.  The parser also cannot handle `const` sections.  For example, the parser will fail on the first `0` or alphabetic token after `const`:

```wiki
section "data" {
    rOG_closure:
        const rOG_info;	// parse error `rOG_info'
        const 0;	// parse error `0'
        const 0;
        const 0;
}
```


Although GHC's Cmm pretty printer outputs C-- standard parenthetical list of arguments after procedure names, i.e., `()`, the Cmm parser will fail at the `(` token.  For example:

```wiki
__stginit_Main_() {	// parse error `('
    cUX:
        Sp = Sp + 4;
        jump (I32[Sp + (-4)]);
}
```


The Cmm procedure names in [rts/PrimOps.cmm](/trac/ghc/browser/ghc/rts/PrimOps.cmm) are not followed by a (possibly empty) parenthetical list of arguments; all their arguments are Global (STG) Registers, anyway, see [Variables, Registers and Types](commentary/compiler/cmm-type#), below.  Don't be confused by the procedure definitions in other handwritten `.cmm` files in the RTS, such as [rts/Apply.cmm](/trac/ghc/browser/ghc/rts/Apply.cmm): all-uppercase procedure invocations are special reserved tokens in [compiler/cmm/CmmLex.x](/trac/ghc/browser/ghc/compiler/cmm/CmmLex.x) and [compiler/cmm/CmmParse.y](/trac/ghc/browser/ghc/compiler/cmm/CmmParse.y).  For example, `INFO_TABLE` is parsed as one of the tokens in the Alex `info` predicate:

```wiki
info	:: { ExtFCode (CLabel, [CmmLit],[CmmLit]) }
	: 'INFO_TABLE' '(' NAME ',' INT ',' INT ',' INT ',' STRING ',' STRING ')'
		-- ptrs, nptrs, closure type, description, type
		{ stdInfo $3 $5 $7 0 $9 $11 $13 }
```


GHC's Cmm parser also cannot parse nested code blocks.  For example:

```wiki
s22Q_ret() {
	s22Q_info {  	// parse error `{'
		const Main_main_srt-s22Q_info+24;
		const 0;
		const 2228227;
	}
    c23f:
	R2 = base_GHCziHandle_stdout_closure;
	R3 = 10;
	Sp = Sp + 4;    /* Stack pointer */
	jump base_GHCziIO_zdwhPutChar_info;
}
```


The C-- specification example in section 4.6.2, "Procedures as section contents" also will not parse in Cmm:

```wiki
section "data" { 
	const PROC = 3; 	// parse error `PROC'
	bits32[] {p_end, PROC}; // parse error `[' (only bits8[] is allowed)
				// parse error `{' (no {...} variable initialisation)

	p (bits32 i) {	// parse error `{' (Cmm thinks "p (bits32 i)" is a statement)
		loop: 
			i = i-1; 
		if (i >= 0) { goto loop ; }	// no parse error 
						// (if { ... } else { ... } *is* parseable)
		return; 
	} 
	p_end: 
} 
```


Note that if `p (bits32 i) { ... }` were written as a Cmm-parseable procedure, as `p { ... }`, the parse error would occur at the closing curly bracket for the `section "data" { ... p { ... } }`\<- here.

## Basic Cmm


Cmm is a high level assembler with a syntax style similar to C.  This section describes Cmm by working up from assembler--the C-- papers and specification work down from C.  At the least, you should know what a "high level" assembler is, see [ What is a High Level Assembler?](http://webster.cs.ucr.edu/AsmTools/HLA/HLADoc/HLARef/HLARef3.html#1035157).  Cmm is different than other high level assembler languages in that it was designed to be a semi-portable intermediate language for compilers; most other high level assemblers are designed to make the tedium of assembly language more convenient and intelligible to humans.  If you are completely new to C--, I highly recommend these papers listed on the [ C-- Papers](http://cminusminus.org/papers.html) page:

- [ C--: A Portable Assembly Language that Supports Garbage Collection (1999)](http://cminusminus.org/abstracts/ppdp.html) (Paper page with Abstract)
- [ C--: A Portable Assembly Language (1997)](http://cminusminus.org/abstracts/pal-ifl.html) (Paper page with Abstract)
- [ A Single Intermediate Language That Supports Multiple Implementations of Exceptions (2000)](http://cminusminus.org/abstracts/c--pldi-00.html) (Paper page with Abstract)
- [ The C-- Language Specification Version 2.0 (CVS Revision 1.128, 23 February 2005)](http://cminusminus.org/extern/man2.pdf) (PDF)


Cmm is not a stand alone C-- compiler; it is an implementation of C-- embedded in the GHC compiler.  One difference between Cmm and a C-- compiler like [ Quick C--](http://cminusminus.org/code.html) is this: Cmm uses the C preprocessor (cpp).  Cpp lets Cmm *integrate* with C code, especially the C header defines in [includes](/trac/ghc/browser/ghc/includes), and among many other consequences it makes the C-- `import` and `export` statements irrelevant; in fact, according to [compiler/cmm/CmmParse.y](/trac/ghc/browser/ghc/compiler/cmm/CmmParse.y) they are ignored.  The most significant action taken by the Cmm modules in the Compiler is to optimise Cmm, through [compiler/cmm/CmmOpt.hs](/trac/ghc/browser/ghc/compiler/cmm/CmmOpt.hs).  The Cmm Optimiser generally runs a few simplification passes over primitive Cmm operations, inlines simple Cmm expressions that do not contain global registers (these would be left to one of the [Backends](commentary/compiler/backends), which currently cannot handle inlines with global registers) and performs a simple loop optimisation. 

### Code Blocks in Cmm


The Haskell representation of Cmm separates contiguous code into:

- *modules* (compilation units; a `.cmm` file); and
- *basic blocks*


Cmm modules contain static data elements (see [Literals and Labels](commentary/compiler/cmm-type#)) and [Basic Blocks](commentary/compiler/cmm-type#), collected together in `Cmm`, a type synonym for `GenCmm`, defined in [compiler/cmm/Cmm.hs](/trac/ghc/browser/ghc/compiler/cmm/Cmm.hs):

```wiki
newtype GenCmm d i = Cmm [GenCmmTop d i]
 
type Cmm = GenCmm CmmStatic CmmStmt

data GenCmmTop d i
  = CmmProc
     [d]	       -- Info table, may be empty
     CLabel            -- Used to generate both info & entry labels
     [LocalReg]        -- Argument locals live on entry (C-- procedure params)
     [GenBasicBlock i] -- Code, may be empty.  The first block is
                       -- the entry point.  The order is otherwise initially 
                       -- unimportant, but at some point the code gen will
                       -- fix the order.

		       -- the BlockId of the first block does not give rise
		       -- to a label.  To jump to the first block in a Proc,
		       -- use the appropriate CLabel.

  -- some static data.
  | CmmData Section [d]	-- constant values only

type CmmTop = GenCmmTop CmmStatic CmmStmt
```

`CmmStmt` is described in [Statements and Calls](commentary/compiler/cmm-type#);
`Section` is described in [Sections and Directives](commentary/compiler/cmm-type#);

the static data in `[d]` is \[`CmmStatic`\] from the type synonym `Cmm`;
`CmmStatic` is described in [Literals and Labels](commentary/compiler/cmm-type#).

#### Basic Blocks and Procedures


Cmm procedures are represented by the first constructor in `GenCmmTop d i`:

```wiki
    CmmProc [d] CLabel [LocalReg] [GenBasicBlock i]
```


For a description of Cmm labels and the `CLabel` data type, see the subsection [Literals and Labels](commentary/compiler/cmm-type#), below.


Cmm Basic Blocks are labeled blocks of Cmm code ending in an explicit jump.  Sections (see [Sections and Directives](commentary/compiler/cmm-type#)) have no jumps--in Cmm, Sections cannot contain nested Procedures (see, e.g., [Compiling Cmm with GHC](commentary/compiler/cmm-type#compiling-cmm-with-ghc)).  In Basic Blocks represent parts of Procedures.  The data type `GenBasicBlock` and the type synonym `CmmBasicBlock` encapsulate Basic Blocks; they are defined in [compiler/cmm/Cmm.hs](/trac/ghc/browser/ghc/compiler/cmm/Cmm.hs):

```wiki
data GenBasicBlock i = BasicBlock BlockId [i]

type CmmBasicBlock = GenBasicBlock CmmStmt

newtype BlockId = BlockId Unique
  deriving (Eq,Ord)

instance Uniquable BlockId where
  getUnique (BlockId u) = u
```


The `BlockId` data type simply carries a `Unique` with each Basic Block.  For descriptions of `Unique`, see 

- the [Renamer](commentary/compiler/renamer) page;
- the [Known Key Things](commentary/compiler/wired-in#known-key-things) section of the [Wired-in and Known Key Things](commentary/compiler/wired-in) page; and, 
- the [Type variables and term variables](commentary/compiler/entity-types#type-variables-and-term-variables) section of the [Entity Types](commentary/compiler/entity-types) page.
