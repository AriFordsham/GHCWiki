
Video: [ GHC Core language](http://video.google.com/videoplay?docid=-8778691434558190641) (14'04")

# The `Core` type


The Core language is GHC's central data types.  Core is a very small, explicitly-typed, variant of System F.  The exact variant is called [System FC](commentary/compiler/fc), which embodies equality constraints and coercions.


The `CoreSyn` type, and the functions that operate over it, gets an entire directory [compiler/coreSyn](/trac/ghc/browser/ghc/compiler/coreSyn):

- [compiler/coreSyn/CoreSyn.lhs](/trac/ghc/browser/ghc/compiler/coreSyn/CoreSyn.lhs): the data type itself.

- [compiler/coreSyn/PprCore.lhs](/trac/ghc/browser/ghc/compiler/coreSyn/PprCore.lhs): pretty-printing.
- [compiler/coreSyn/CoreFVs.lhs](/trac/ghc/browser/ghc/compiler/coreSyn/CoreFVs.lhs): finding free variables.
- [compiler/coreSyn/CoreSubst.lhs](/trac/ghc/browser/ghc/compiler/coreSyn/CoreSubst.lhs): substitution.
- [compiler/coreSyn/CoreUtils.lhs](/trac/ghc/browser/ghc/compiler/coreSyn/CoreUtils.lhs): a variety of other useful functions over Core.

- [compiler/coreSyn/CoreUnfold.lhs](/trac/ghc/browser/ghc/compiler/coreSyn/CoreUnfold.lhs): dealing with "unfoldings".

- [compiler/coreSyn/CoreLint.lhs](/trac/ghc/browser/ghc/compiler/coreSyn/CoreLint.lhs): type-check the Core program. This is an incredibly-valuable consistency check, enabled by the flag `-dcore-lint`.

- [compiler/coreSyn/CoreTidy.lhs](/trac/ghc/browser/ghc/compiler/coreSyn/CoreTidy.lhs): part of the [the CoreTidy pass](commentary/compiler/hsc-main) (the rest is in [compiler/main/TidyPgm.lhs](/trac/ghc/browser/ghc/compiler/main/TidyPgm.lhs)).
- [compiler/coreSyn/CorePrep.lhs](/trac/ghc/browser/ghc/compiler/coreSyn/CorePrep.lhs): [the CorePrep pass](commentary/compiler/hsc-main)


Here is the entire Core type [compiler/coreSyn/CoreSyn.lhs](/trac/ghc/browser/ghc/compiler/coreSyn/CoreSyn.lhs):

```wiki
type CoreExpr = Expr Var

data Expr b	-- "b" for the type of binders, 
  = Var	  Id
  | Lit   Literal
  | App   (Expr b) (Arg b)
  | Lam   b (Expr b)
  | Let   (Bind b) (Expr b)
  | Case  (Expr b) b Type [Alt b]
  | Cast  (Expr b) Coercion
  | Note  Note (Expr b)
  | Type  Type

type Arg b = Expr b
type Alt b = (AltCon, [b], Expr b)

data AltCon = DataAlt DataCon | LitAlt  Literal | DEFAULT

data Bind b = NonRec b (Expr b) | Rec [(b, (Expr b))]
```


That's it.  All of Haskell gets compiled through this tiny core.

`Expr` is parameterised over the type of its *binders*, `b`.  This facility is used only rarely, and always temporarily; for example, the let-floater `SetLevels` pass attaches a binding level to every binder.  By far the most important type is `CoreExpr`, which is `Expr` with `Var` binders.


Here are some notes about the individual constructors of `Expr`.

- `Lam` is used for both term and type abstraction (small and big lambdas).

- `Type` appears only in type-argument positions (e.g. `App (Var f) (Type ty)`).  To emphasise this, the type synonym `Arg` is used as documentation when we expect that a `Type` constructor may show up.  Anything not called `Arg` should not use a `Type` constructor.

- `Let` handles both recursive and non-recursive let-bindings; see the the two constructors for `Bind`.

- `Case` expressions need [more explanation](commentary/compiler/core-syn-type#case-expressions).

- `Cast` is used for an [FC cast expression](commentary/compiler/fc).  `Coercion` is a synonym for `Type`.

- `Note` is used for profiling and debugging information.

## Case expressions


Case expressions are the most complicated bit of `Core`.  In the term `Case scrut case_bndr res_ty alts`:

- `scrut` is the scrutinee
- `case_bndr` is the **case binder** (see notes below)
- `res_ty` is the type of the entire case expression (redundant once [FC](commentary/compiler/fc) is in HEAD -- was for GADTs)
- `alts` is a list of the case alternatives


A case expression can scrutinise 

- **a data type** (the alternatives are `DataAlt`s), or 
- **a primitive literal type** (the alternatives are `LitAlt`s), or 
- **a value of any type at all** (if there is one `DEFAULT` alternative).


A case expression is **always strict**, even if there is only one alternative, and it is `DEFAULT`.  (This differs from Haskell!)  So

```wiki
case error "urk" of { DEFAULT -> True }
```


will call `error`, rather then returning `True`.


The `case_bndr` field, called the **case binder**, is an unusual feature of GHC's case expressions.
The idea is that *in any right-hand side, the case binder is bound to the value of the scrutinee*. If the
scrutinee was always atomic nothing would be gained, but real expressiveness is added when the scrutinee is not atomic.
Here is a slightly contrived example:

```wiki
case (reverse xs) of y 
  Nil       -> Nil
  Cons x xs -> append y y
```


(Here, "`y`" is the case binder; at least that is the syntax used by the Core pretty printer.)
This expression evaluates `reverse xs`; if the result is `Nil`, it returns
`Nil`, otherwise it returns the reversed list appended to itself.  Since
the returned value of `reverse xs` is present in the implementation, it makes
sense to have a name for it!


The most common application is to model call-by-value, 
by using `case` instead of `let`. For example, here is how we might compile
the call `f (reverse xs)` if we knew that `f` was strict:

```wiki
case (reverse xs) of y { DEFAULT -> f y }
```


Case expressions have several invariants

- The `res_ty` type is the same as the type of any of the right-hand sides (up to refining unification -- coreRefineTys in [compiler/types/Unify.lhs](/trac/ghc/browser/ghc/compiler/types/Unify.lhs) -- in pre-[FC](commentary/compiler/fc)).

- If there is a `DEFAULT` alternative, it must appear first.  This makes finding a `DEFAULT` alternative easy, when it exists.

- The remaining non-DEFAULT alternatives must appear in order of

  - tag, for `DataAlt`s
  - lit, for `LitAlt`s

>
> This makes finding the relevant constructor easy, and makes comparison easier too.

- The list of alternatives is **always exhaustive**, meaning that it covers **all reachable cases**.  Note, however, that an "exhausive" case does not necessarily mention all constructors:

  ```wiki
  data Foo = Red | Green | Blue

  ...case x of 
  	Red   -> True
  	other -> f (case x of 
  			Green -> ...
  			Blue  -> ... )
  ```

  The inner case does not need a `Red` alternative, because x can't be `Red` at that program point. Furthermore, GADT type-refinement might mean that some alternatives are not reachable, and hence can be discarded.  
