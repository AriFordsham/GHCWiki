# Trees that Grow Guidance

[ The Trees that Grow (TTG) idiom](http://www.jucs.org/jucs_23_1/trees_that_grow/jucs_23_01_0042_0062_najd.pdf) can be used to provide different forms of extensions and variations on an AST. Since April 2018, the [HsSyn](implementing-trees-that-grow/hs-syn) AST inside GHC supports the TTG idiom. This page provides a set of guiding principles for GHC developers on how to understand and use the TTG idiom in [HsSyn](implementing-trees-that-grow/hs-syn).

## Context and Scope


The new [HsSyn](implementing-trees-that-grow/hs-syn) AST supporting the TTG idiom (from now on referred to as TTG [HsSyn](implementing-trees-that-grow/hs-syn)) is engineered to subsume five different representations of Haskell syntax: 

- AST GhcPs: the AST used in GHC's parsing phase
- AST GhcRn: the AST used in GHC's renaming phase
- AST GhcTc: the AST used in GHC's typechecking phase
- AST TH:    the AST used in Template Haskell
- AST HSE:   the AST used in an external tool such as Haskell-Src-Exts


The subsumption of above five ASTs is done by providing instances for the extension type families.
For instance, the AST for GHC's parsing, renaming, and typechecking are defined by providing instances of the extension type families using accordingly the indices `GhcPs`, `GhcRn`, and `GhcTc`.
[ Here](https://github.com/ghc/ghc/blob/master/compiler/hsSyn/HsExpr.hs#L737-L835) is the actual code providing such instances for the `HsExpr` datatype of expressions in the TTG [HsSyn](implementing-trees-that-grow/hs-syn).
 


Subsuming above five trees fixes the scope of the design space. For example, TTG [HsSyn](implementing-trees-that-grow/hs-syn) is not intended to subsume the AST in the GHC's backend (i.e., GHC Core), but it can indeed be used for other purposes like prettyprinting and IDEs.

## Guiding Principles

1. The instantiation of TTG [HsSyn](implementing-trees-that-grow/hs-syn) should result in a tree that does not have extra fields and constructors. 

>
> For example, the `HsExpr GhsPs` expressions of AST GhcPs should not have the constructor `HsUnboundVar` of the post-renaming phases, or its `HsMultiIf` constructor should also not have an unused field (of the type `Type`) to store the related type produced in the typechecking phase.

>
> As a result, the instantiated TTG [HsSyn](implementing-trees-that-grow/hs-syn) should not depend on the code from the other phases. Hence, the base (uninstantiated) TTG [HsSyn](implementing-trees-that-grow/hs-syn) should not depend on any GHC/TH/HSE-specific code.

>
> For example, if `HsExpr GhsPs` expressions of AST GhcPs had the constructor `HsUnboundVar` then it had to depend on the code defining `UnboundVar` (a field of `HsUnboundVar`) in the renaming phase, or if its constructor `MultiIf` had a field of type `Type` then it had to depend on the code defining `Type` in the typechecking phase.

1. The base TTG [HsSyn](implementing-trees-that-grow/hs-syn) should have all the constructors common across all five ASTs, and these constructors should have all the fields common across all five ASTs (even if the type of some fields vary from an AST to another).

>
> SPJ refers to these common fields as "payload fields" (as opposed to extension fields). 

1. The constructors that are not common are introduced using TTG's new constructor extensions.

1. For common constructors, their fields that are not common are grouped together and introduced using TTG's new field extensions.

1. For common constructors, their common fields with a varying type, are given a type using a new type family that extracts from the phase descriptor the type specific to each AST.

>
> For example, the type of the common (payload) field of the common constructor `HsVar`of `HsExpr x` is `IdP x` where `IdP` is a type family and `x` the phase descriptor. 

## Example


Consider the following three simple datatypes `ExpPs`, `ExpRn`, and `ExpTc` representing correspondingly expressions in a parsing, renaming and typechecking phase:

```wiki
module Parsing where

-- the definition of RdrName
-- ...

data ExpPs 
  = Var RdrName
  | Abs RdrName ExpPs
  | App ExpPs   ExpPs
```

```wiki
module Renaming where

-- the definition of `Name` and `UnboundVar`
-- ...

data ExpRn
  = Var Name
  | Abs Name  ExpRn
  | App ExpRn ExpRn
  | UVar UnboundVar
```

```wiki
module Typechecking where

-- the definition of `Id`, `UnboundVar`, and `Type`
-- ...

data ExpTc
  = Var  Id
  | Abs  Id   ExpTc
  | App  Type ExpTc ExpTc
  | UVar UnboundVar
```


Based on the TTG idiom, we will have a base declaration such as the following.

```wiki
module AST where

data Exp x 
  = Var (XVar x) (XId x)
  | Abs (XAbs x) (XId x) (Exp x)
  | App (XApp x) (Exp x) (Exp x)
  | New (XNew x)

type family XVar x
type family XAbs x
type family XApp x
type family XNew x

type family XId  x
```


and the following three instantiations:

```wiki
module Parsing where

import AST
-- the definition of RdrName
-- ...

data Ps

type ExpPs = Exp Ps

type instance XVar Ps = ()
type instance XAbs Ps = ()
type instance XApp Ps = ()
type instance XNew Ps = Void

type instance XId  Ps = RdrName
```

```wiki
module Renaming where

import AST
-- the definition of `Name` and `UnboundVar`
-- ...
data Rn

type ExpRn = Exp Rn

type instance XVar Rn = ()
type instance XAbs Rn = ()
type instance XApp Rn = ()
type instance XNew Rn = UnboundVar

type instance XId  Rn = Name
```

```wiki
module Typechecking where

import AST
-- the definition of `Id`, `UnboundVar`, and `Type`
-- ...
data Tc

type ExpTc = Exp Tc

type instance XVar Tc = ()
type instance XAbs Tc = ()
type instance XApp Tc = Type
type instance XNew Tc = UnboundVar

type instance XId  Tc = Id
```