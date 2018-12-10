# The [ExpPatFrame](design/exp-pat-frame) Parser Refactoring


This page outlines a new design for the expression/pattern parser and the motivation for it.

## The Problem


There are places in the grammar where we do not know whether we are parsing an expression or a pattern without infinite lookahead (which we do not have in `happy`):

1. View patterns:

  ```
       f (Con a b     )=...-- 'Con a b' is a pattern
       f (Con a b -> x)=...-- 'Con a b' is an expression
  ```

1. do-notation:

  ```
  do{Con a b <- x }-- 'Con a b' is a patterndo{Con a b }-- 'Con a b' is an expression
  ```

1. Guards:

  ```
       x |True<- p && q =...-- 'True' is a pattern
       x |True=...-- 'True' is an expression
  ```

1. Top-level value/function declarations (FunBind/PatBind):

  ```
       f !a         -- TH splice
       f !a =...-- function declaration
  ```

  Until we encounter the `=` sign, we don't know if it's a top-level [TemplateHaskell](template-haskell) splice where `!` is an infix operator, or if it's a function declaration where `!` is a strictness annotation.


The approach GHC uses is to parse patterns as expressions and rejig later. This turns out to be suboptimal:

- We can't handle corner cases. For instance, the following function declaration LHS is not a valid expression (see [\#1087](https://gitlab.haskell.org//ghc/ghc/issues/1087)):

  ```
  !a +!b =...
  ```

- There are points in the pipeline where the representation is awfully incorrect. For instance,

  ```
        f !a b !c =...
  ```

  is first parsed as

  ```
  (f ! a b)! c =...
  ```

- We have to extend HsExpr with pattern-specific constructs: `EAsPat`, `EViewPat`, `ELazyPat`, etc. It isn't particularly elegant and we don't want such constructors to show up in GHC API.

## Backtracking with Parser Combinators


One might think we could avoid this issue by using a backtracking parser and doing something along the lines of `try pExpr <|> pPat`. I proposed this in a ghc-devs thread: [ https://mail.haskell.org/pipermail/ghc-devs/2018-October/016291.html](https://mail.haskell.org/pipermail/ghc-devs/2018-October/016291.html). The situation turned out to be more complex. As there can be patterns inside expressions (e.g. via `case`, `let`, `do`) and expressions inside patterns (e.g. view patterns), naive backtracking would be devastating to performance (asymptotically).

## Common Structure


There are common syntactic elements of expressions and patterns (e.g. both of them must have balanced parentheses), and we can capture this common structure in an intermediate data type, `ExpPatFrame`:

```
dataExpPatFrame=FrameVarRdrName-- ^ Identifier: Just, map, BS.length|FrameIPVarHsIPName-- ^ Implicit parameter: ?x|FrameOverLabelFastString-- ^ Overloaded label: #label|FrameTuple[LTupArgFrame]Boxity-- ^ Tuple (section): (a,b) (a,b,c) (a,,) (,a,)|FrameViewPatLExpPatFrameLExpPatFrame-- ^ View pattern: e -> p|FrameTySigLExpPatFrame(LHsSigWcTypeGhcPs)-- ^ Type signature: x :: ty|FrameAsPatLExpPatFrameLExpPatFrame-- ^ As-pattern: x@(D a b)...
```

`ExpPatFrame` is a union of all syntactic elements between expressions and patterns, so it includes both expression-specific constructs (e.g. overloaded labels) and pattern-specific constructs (e.g. view/as patterns).


As soon as we have parsed far enough to decide whether it is an expression or a pattern, we can convert to `HsExpr` or `HsPat` accordingly. In the future we might want to extend '[ExpPatFrame](design/exp-pat-frame)' (expression/pattern frame) to `ExpPatTyFrame` (expression/pattern/type frame), because with Dependent Haskell (or even smaller features, such as visible dependent quantification in terms) we will have types inside expressions and expressions inside types.


The nice thing about having a dedicated type such as `ExpPatFrame` is keeping parsing concerns local to the parser: no matter what hacky intermediate structures we add to `ExpPatFrame`, we can keep `HsExpr` and `HsPat` clean.

## Trees that Grow


During the discussion of [ Phab:D5408](https://phabricator.haskell.org/D5408), an alternative plan came up: create a new GHC pass, `GhcPrePs`, and extend `HsExpr GhcPrePs` with pattern-specific and command-specific constructors. Then disambiguate during the conversion from `GhcPrePs` to `GhcPs`.


The reason this design does not work is that some parts of `HsExpr` should be disambiguated much sooner than we parse an expression in its entirety. We have:

```wiki
data HsExpr p
    ...
  | HsLet ...
                (LHsLocalBinds p)
                ...

data Match p body
  = Match {
        ...
        m_pats :: [LPat p],
        ...
  }

data GRHSs p body
  = GRHSs {
      ...
      grhssLocalBinds :: LHsLocalBinds p
    }

data StmtLR idL idR body
  ...
  | BindStmt
             (LPat idL)
             ...
```


Imagine we're parsing a `HsExpr GhcPrePs` – it will contain `LHsLocalBinds GhcPrePs` and `LPat GhcPrePs`. Converting them to `GhcPs` is extra code and extra runtime – we don't want that. Instead, in `ExpPatFrame` we store `LHsLocalBinds GhcPs` and `LPat GhcPs` in corresponding places. Therefore, `ExpPatFrame` does not constitute a proper pass: we pre-parse little fragments that store `GhcPs` subtrees and then convert these fragments to `HsExpr GhcPs`, `HsPat GhcPs`, or `HsPat GhcPs`.

## Minimizing `ExpPatFrame`


We'd like to keep `ExpPatFrame` as small as possible. It means that instead of duplicating all of `HsExpr` and `HsPat` constructors in it, we'd rather embed them directly when unambiguous. For example, patterns cannot contain `if`, `case`, or `do`, so we'd rather have this:

```wiki
data ExpPatFrame
  = ...
  | ...
  | FrameExpr (HsExpr GhcPs)
```


than this:

```wiki
data ExpPatFrame
  = ...
  | ...
  | ...
  | FrameIf LExpPatFrame LExpPatFrame LExpPatFrame
    -- ^ If-expression: if p then x else y
  | FrameMultiIf [LFrameGRHS]
    -- ^ Multi-way if-expression: if | p = x \n | q = x
  | FrameCase LExpPatFrame [LFrameMatch]
    -- ^ Case-expression: case x of { p1 -> e1; p2 -> e2 }
  | FrameDo (HsStmtContext Name) [LFrameStmt]
    -- ^ Do-expression: do { s1; a <- s2; s3 }
  ...


data FrameStmt
  = ...
  | ...
  | ...
  | FrameBindStmt (LPat GhcPs) LExpPatFrame
    -- ^ Binding statement: p <- e
  | FrameBodyStmt LExpPatFrame
    -- ^ Body statement: e
  | FrameLetStmt (LHsLocalBinds GhcPs)
    -- ^ Let statement: let p = t
  ...
```


Unfortunately, while `do` or `let` cannot be used in patterns, they can be used in commands, so we end up duplicating most of `HsExpr` constructors for the sake of `HsCmd`. If not for this, we'd be able to make `ExpPatFrame` smaller.


Nevertheless, in the final iteration we will include constructors for unambiguous cases:

```wiki
data ExpPatFrame
  = ...
  | ...
  | FrameExpr (HsExpr GhcPs) -- unambiguously an expression
  | FramePat (HsPat GhcPs) -- unambiguously a pattern
  | FrameCommand (HsCmd GhcPs) -- unambiguously a command
```


Hopefully, this will allow us to remove at least some duplication, even if not much.

## Implementation Plan


Initially I (int-index) tried to do the refactoring and fix [\#1087](https://gitlab.haskell.org//ghc/ghc/issues/1087) simultaneously, and I have a semi-working proof-of-concept passing (almost all) tests: [ https://github.com/serokell/ghc/commit/39c5db2d77c96d4b0962f581b60908343edf8624](https://github.com/serokell/ghc/commit/39c5db2d77c96d4b0962f581b60908343edf8624)


This prototype also revealed that doing the `ExpPatFrame` refactoring allows for better error messages in some cases. Compare:

```wiki
GHCi, version 8.4.3: http://www.haskell.org/ghc/  :? for help
Prelude> foo# = ()

<interactive>:1:6: error:
   parse error on input '='
   Perhaps you need a 'let' in a 'do' block?
   e.g. 'let x = 5' instead of 'x = 5'
```


and

```wiki
GHCi, version 8.7.20181026: http://www.haskell.org/ghc/  :? for help
Prelude> foo # = ()

<interactive>:1:5: error:
   Operator is missing a right-hand side argument: #
Prelude> foo# = ()

<interactive>:2:4: error:
   Operator is missing a right-hand side argument: #
   Perhaps you meant to enable MagicHash?
```


However, after a few hellish rebases, I decided to split this effort into many smaller patches. Here's the new plan:

1. Introduce `ExpPatFrame` with as little churn as possible.
1. Use it to clean up the definition of `HsExpr`, removing `EAsPat`, `EWildPat`, `EViewPat`, `ELazyPat`. Perhaps also `HsArrApp` and `HsArrForm`.
1. Use it as the basis for fixing [\#1087](https://gitlab.haskell.org//ghc/ghc/issues/1087)
1. Investigate its viability for term/type parser unification
