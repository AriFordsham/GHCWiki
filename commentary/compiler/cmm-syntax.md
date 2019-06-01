# Cmm syntax

The GHC has three different syntaxes for Cmm:
  * Hand-writing syntax by [`CmmParse.y`][1] (e.g. [`PrimOps.cmm`][2])
  * Pretty-printing syntax by [`PprCmm.hs`][3] (as [`ghc --ddump-cmm`][4])
  * Intermediate representation in [`Cmm.hs`][5]

To improve readability of Cmm files, this page shows the hand-writing syntax defined by `CmmParse.y`.

See [Code Generator][9] and [The Runtime System][10].  
See also [cmm-type][6].


## Cmm hand-writing syntax

Following syntax was manually extracted from [`CmmParse.y`][1] at commit 1ffee940a0.


```
cmm    →  {- empty -}
        | cmmtop cmm

cmmtop →  cmmproc
        | cmmdata
        | decl
        | 'CLOSURE' '(' NAME ',' NAME lits ')' ';'  

cmmdata →  'section' STRING '{' data_label statics '}'

data_label →  NAME ':'

statics →  {- empty -}
         | static statics
    
static →  type expr ';'
        | type ';'
        | 'bits8' '[' ']' STRING ';'
        | 'bits8' '[' INT ']' ';'
        | typenot8 '[' INT ']' ';'
        | 'CLOSURE' '(' NAME lits ')'

lits   →  {- empty -}
        | ',' expr lits

cmmproc →  info maybe_conv maybe_formals maybe_body

maybe_conv →  {- empty -}
            | 'return'

maybe_body →  ';'
            | '{' body '}'

info   →  NAME
        | 'INFO_TABLE' '(' NAME ',' INT ',' INT ',' INT ',' STRING ',' STRING ')'
        | 'INFO_TABLE_FUN' '(' NAME ',' INT ',' INT ',' INT ',' STRING ',' STRING ',' INT ')'
        | 'INFO_TABLE_CONSTR' '(' NAME ',' INT ',' INT ',' INT ',' INT ',' STRING ',' STRING ')'
        | 'INFO_TABLE_SELECTOR' '(' NAME ',' INT ',' INT ',' STRING ',' STRING ')'
        | 'INFO_TABLE_RET' '(' NAME ',' INT ')'
        | 'INFO_TABLE_RET' '(' NAME ',' INT ',' formals0 ')'

body   →  {- empty -}
        | decl body
        | stmt body

decl   →  type names ';'
        | 'import' importNames ';'
        | 'export' names ';'

importNames →  importName
             | importName ',' importNames

importName →  NAME
            | 'CLOSURE' NAME
            | STRING NAME
        
names  →  NAME
        | NAME ',' names

stmt   →  ';'
        | NAME ':'
        | lreg '=' expr ';'
        | type '[' expr ']' '=' expr ';'
        | foreign_results 'foreign' STRING foreignLabel '(' cmm_hint_exprs0 ')' safety opt_never_returns ';'
        | foreign_results 'prim' '%' NAME '(' exprs0 ')' ';'
        | NAME '(' exprs0 ')' ';'
        | 'switch' maybe_range expr '{' arms default '}'
        | 'goto' NAME ';'
        | 'return' '(' exprs0 ')' ';'
        | 'jump' expr vols ';'
        | 'jump' expr '(' exprs0 ')' ';'
        | 'jump' expr '(' exprs0 ')' '(' exprs0 ')' ';'
        | 'call' expr '(' exprs0 ')' ';'
        | '(' formals ')' '=' 'call' expr '(' exprs0 ')' ';'
        | 'if' bool_expr cond_likely 'goto' NAME
        | 'if' bool_expr cond_likely '{' body '}' else
        | 'push' '(' exprs0 ')' maybe_body
        | 'reserve' expr '=' lreg maybe_body
        | 'unwind' unwind_regs ';'

unwind_regs →  GLOBALREG '=' expr_or_unknown ',' unwind_regs
             | GLOBALREG '=' expr_or_unknown

expr_or_unknown →  'return'
                 | expr

foreignLabel →  NAME

opt_never_returns →  {- empty -}
                   | 'never' 'returns'

bool_expr →  bool_op
           | expr

bool_op →  bool_expr '&&' bool_expr
         | bool_expr '||' bool_expr
         | '!' bool_expr
         | '(' bool_op ')'

safety →  {- empty -}
        | STRING

vols   →  '[' ']'
        | '[' '*' ']'
        | '[' globals ']'

globals →  GLOBALREG
         | GLOBALREG ',' globals

maybe_range →  '[' INT '..' INT ']'
             | {- empty -}

arms   →  {- empty -}
        | arm arms

arm    →  'case' ints ':' arm_body

arm_body →  '{' body '}'
          | 'goto' NAME ';'

ints   →  INT
        | INT ',' ints

default →  'default' ':' '{' body '}'
         | {- empty -}

else   →  {- empty -}
        | 'else' '{' body '}'

cond_likely →  '(' 'likely' ':' 'True'  ')'
             | '(' 'likely' ':' 'False' ')'
             | {- empty -}

expr   →  expr '/' expr
        | expr '*' expr
        | expr '%' expr
        | expr '-' expr
        | expr '+' expr
        | expr '>>' expr
        | expr '<<' expr
        | expr '&' expr
        | expr '^' expr
        | expr '|' expr
        | expr '>=' expr
        | expr '>' expr
        | expr '<=' expr
        | expr '<' expr
        | expr '!=' expr
        | expr '==' expr
        | '~' expr
        | '-' expr
        | expr0 '`' NAME '`' expr0
        | expr0

expr0  →  INT   maybe_ty
        | FLOAT maybe_ty
        | STRING
        | reg
        | type '[' expr ']'
        | '%' NAME '(' exprs0 ')'
        | '(' expr ')'

maybe_ty →  {- empty -}
          | '::' type

cmm_hint_exprs0 →  {- empty -}
                 | cmm_hint_exprs

cmm_hint_exprs →  cmm_hint_expr
                | cmm_hint_expr ',' cmm_hint_exprs

cmm_hint_expr →  expr
               | expr STRING

exprs0 →  {- empty -}
        | exprs

exprs  →  expr
        | expr ',' exprs

reg    →  NAME
        | GLOBALREG

foreign_results →  {- empty -}
                 | '(' foreign_formals ')' '='

foreign_formals →  foreign_formal
                 | foreign_formal ','
                 | foreign_formal ',' foreign_formals

foreign_formal →  local_lreg
                | STRING local_lreg

local_lreg →  NAME

lreg       →  NAME
           | GLOBALREG

maybe_formals →  {- empty -}
               | '(' formals0 ')'

formals0 →  {- empty -}
          | formals

formals  →  formal ','
          | formal
          | formal ',' formals

formal   →  type NAME

type   →  'bits8'
        | typenot8

typenot8 →  'bits16'
          | 'bits32'
          | 'bits64'
          | 'bits128'
          | 'bits256'
          | 'bits512'
          | 'float32'
          | 'float64'
          | 'gcptr'
```

See also [`CmmLex.x`][7] for lexical tokens. For instance:

```
GLOBALREG →  P@decimal
             R@decimal
             F@decimal
             D@decimal
             L@decimal
             Sp
             SpLim
             Hp
             HpLim
             CCCS
             CurrentTSO
             CurrentNursery
             HpAlloc
             BaseReg
             MachSp
             UnwindReturnReg
```


## Built-in macros and primitives

Some built-in macros and primitives are defined for Cmm hand-writing syntax in [`CmmParse.y`][1].

Built-in expression macros (exprMacros):

```
  %ENTRY_CODE
  %INFO_PTR
  %STD_INFO
  %FUN_INFO
  %GET_ENTRY
  %GET_STD_INFO
  %GET_FUN_INFO
  %INFO_TYPE
  %INFO_PTRS
  %INFO_NPTRS
```

Built-in primitives (machOps):

```
  %add
  %sub
  %eq
  %ne
  %mul
  %neg
  %quot
  %rem
  %divu
  %modu

  %ge
  %le
  %gt
  %lt

  %geu
  %leu
  %gtu
  %ltu

  %and
  %or
  %xor
  %com
  %shl
  %shrl
  %shra

  %fadd
  %fsub
  %fneg
  %fmul
  %fquot

  %feq
  %fne
  %fge
  %fle
  %fgt
  %flt

  %lobits8
  %lobits16
  %lobits32
  %lobits64

  %zx16
  %zx32
  %zx64

  %sx16
  %sx32
  %sx64

  %f2f32
  %f2f64
  %f2i8
  %f2i16
  %f2i32
  %f2i64
  %i2f32
  %i2f64
```

Built-in callish primitives (callishMachOps):

```
  %write_barrier
  %memcpy
  %memset
  %memmove
  %memcmp

  %prefetch0
  %prefetch1
  %prefetch2
  %prefetch3

  %popcnt8
  %popcnt16
  %popcnt32
  %popcnt64

  %pdep8
  %pdep16
  %pdep32
  %pdep64

  %pext8
  %pext16
  %pext32
  %pext64

  %cmpxchg8
  %cmpxchg16
  %cmpxchg32
  %cmpxchg64
```

Built-in statement macros (stmtMacros):

```
  CCS_ALLOC
  ENTER_CCS_THUNK
  CLOSE_NURSERY
  OPEN_NURSERY
  HP_CHK_GEN
  STK_CHK_GEN
  STK_CHK_GEN_N
  LOAD_THREAD_STATE
  SAVE_THREAD_STATE
  LDV_ENTER
  LDV_RECORD_CREATE
  PUSH_UPD_FRAME
  SET_HDR
  TICK_ALLOC_PRIM
  TICK_ALLOC_PAP
  TICK_ALLOC_UP_THK
  UPD_BH_UPDATABLE
```


## C-preprocessor macros

There are also C-preprocessor level macros, such as `ccall`, `I64`, `Sp(n)`, `LOAD_INFO`, `ENTER(x)`, and `MAYBE_GC`.

Those macros are defined in [`includes/Cmm.h`][8].



[1]: https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/cmm/CmmParse.y
[2]: https://gitlab.haskell.org/ghc/ghc/blob/master/rts/PrimOps.cmm
[3]: https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/cmm/PprCmm.hs
[4]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugging.html#c-representation
[5]: https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/cmm/Cmm.hs
[6]: https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/cmm-type
[7]: https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/cmm/CmmLex.x
[8]: https://gitlab.haskell.org/ghc/ghc/blob/master/includes/Cmm.h
[9]: https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/code-gen
[10]: https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts