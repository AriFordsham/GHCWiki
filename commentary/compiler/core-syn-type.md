# The `Core` type


The Core language is GHC's central data types.  Core is a very small, explicitly-typed, variant of System.  The exact variant is called System FC, and described by our paper [ System F with equality coercions](http://research.microsoft.com/~simonpj/papers/ext-f).  (Note: the move to FC was done in Autumn 2006, but earlier versions of GHC had a very similar language.)


The `CoreSyn` type, and the functions that operate over it, gets an entire directory [compiler/coreSyn](/trac/ghc/browser/ghc/compiler/coreSyn):

- [compiler/coreSyn/CoreSyn.lhs](/trac/ghc/browser/ghc/compiler/coreSyn/CoreSyn.lhs): the data type itself.
- [compiler/coreSyn/PprCore.lhs](/trac/ghc/browser/ghc/compiler/coreSyn/PprCore.lhs): pretty-printing.
- [compiler/coreSyn/CoreFVs.lhs](/trac/ghc/browser/ghc/compiler/coreSyn/CoreFVs.lhs): finding free variables.
- [compiler/coreSyn/CoreSubst.lhs](/trac/ghc/browser/ghc/compiler/coreSyn/CoreSubst.lhs): substitution.
- [compiler/coreSyn/CoreUtils.lhs](/trac/ghc/browser/ghc/compiler/coreSyn/CoreUtils.lhs): a variety of other useful functions over Core.
- [compiler/coreSyn/CoreUnfold.lhs](/trac/ghc/browser/ghc/compiler/coreSyn/CoreUnfold.lhs): dealing with "unfoldings".
- [compiler/coreSyn/CoreLint.lhs](/trac/ghc/browser/ghc/compiler/coreSyn/CoreLint.lhs): type-check the Core program. This is an incredibly-valuable consistency check, enabled by the flag `-dcore-lint`.
- [compiler/coreSyn/CorePrep.lhs](/trac/ghc/browser/ghc/compiler/coreSyn/CorePrep.lhs): [the CorePrep pass](commentary/compiler/hsc-main)
- [compiler/coreSyn/CoreTidy.lhs](/trac/ghc/browser/ghc/compiler/coreSyn/CoreTidy.lhs): part of the [the CoreTidy pass](commentary/compiler/hsc-main) (the rest is in [compiler/Main/TidyPgm.lhs](/trac/ghc/browser/ghc/compiler/Main/TidyPgm.lhs)).


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

- `Type` appears only in type-argument positions (e.g. `App (Var f) (Type ty)`).  To emphasise this, the type synonym `Arg` is used as documentation when we expect that a `Type` constructor may show up.

- `Let` handles both recursive and non-recursive let-bindings; see the the two constructors for `Bind`.

- `Case` need [more explanation](commentary/compiler/core-syn-type#case-expressions).

- `Cast` is used for an FC cast expression.  `Corecion` is a synonym for `Type`.

- `Note` is used for profiling and debugging information.

## Case expressions


Case expressions are the most complicated bit of `Core`.

- The case expression can scrutinise 

  - a data type (the alternatives are `DataAlt`s), or 
  - a primitive literal type (the alternatives are `LitAlt`s), or 
  - a value of any type at all (if there is one `DEFAULT` alternative).

- If there is a `DEFAULT` alternative, it must appear first.

- The remaining non-DEFAULT alternatives must appear in order of

  - tag, for DataAlts
  - lit, for LitAlts

>
> This makes finding the relevant constructor easy, and makes comparison easier too.

- The list of alternatives is **always exhaustive**, meaning that it covers all cases that can occur.  An "exhausive" case does not necessarily mention all constructors:

  ```wiki
  data Foo = Red | Green | Blue

  ...case x of 
  	Red   -> True
  	other -> f (case x of 
  			Green -> ...
  			Blue  -> ... )
  ```

  The inner case does not need a `Red` alternative, because x can't be `Red` at that program point.
