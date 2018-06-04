# Handling of Source Locations in Trees that Grow

## Problem


The current design of TTG [HsSyn](implementing-trees-that-grow/hs-syn) AST in GHC stores source locations for terms of a datatype `Exp` in a separate wrapper datatype `LExp` which is mutually recursive with `Exp` such that every recursive reference to `Exp` is done **indirectly**, via a reference to the wrapper datatype `LExp`. We refer to this style of storing source locations as the ping-pong style.


Besides the indirection and the complications the ping-pong style causes, there are two key problems with it: 

1. It bakes-in the source locations in the base AST, forcing all instances to store source locations, even if they don't need them.
  For example, TH AST does not carry source locations. 

1. It results in a form of conceptual redundancy: source locations are tree decorations and they belong in the extension points.
  (see [ TTG Guidance](https://ghc.haskell.org/trac/ghc/wiki/ImplementingTreesThatGrow/TreesThatGrowGuidance))

### Example


For example, here is a simple [ TTG](https://ghc.haskell.org/trac/ghc/wiki/ImplementingTreesThatGrow/TreesThatGrowGuidance) representation of lambda expressions in the ping-pong style.

```wiki
{-# LANGUAGE TypeFamilies
           , ConstraintKinds
#-}
module Original where

import GHC.Exts(Constraint)
import Data.Void

-- ...

data RdrName
-- = the definition of RdrName

data SrcSpan
-- = the definition of SrcSpan

data Located a = L SrcSpan a

-- ----------------------------------------------
-- AST Base
-- ----------------------------------------------
type LExp x = Located (Exp x)

data Exp x
  = Var (XVar x) (XId x)
  | Lam (XLam x) (XId x)  (LExp x)
  | App (XApp x) (LExp x) (LExp x)
  | Par (XPar x) (LExp x)
  | New (XNew x)

type family XVar x
type family XLam x
type family XApp x
type family XPar x
type family XNew x

type family XId  x

type ForallX (p :: * -> Constraint) x
  = ( p (XVar x)
    , p (XLam x)
    , p (XApp x)
    , p (XPar x)
    , p (XNew x)
    )

-- ----------------------------------------------
-- AST Ps (parsing phase)
-- ----------------------------------------------

data Ps

type ExpPs  = Exp  Ps
type LExpPs = LExp Ps

type instance XVar Ps = ()
type instance XLam Ps = ()
type instance XApp Ps = ()
type instance XPar Ps = ()
type instance XNew Ps = Void

type instance XId  Ps = RdrName

-- ----------------------------------------------
-- Example Function
-- ----------------------------------------------

par :: LExp Ps -> LExp Ps
par l@(L sp m) = L sp (Par () l)
```

## Solutions


The key solution is to move source locations to the extension points, remove the indirection (e.g., the wrapper datatype `LExp`) altogether, and update the related code (e.g., functions over `Exp`) accordingly. 
There are a couple of ways to implement such a solution:

1. TODO Using a typeclass
1. TODO Nesting extension typefamilies
1. TODO inlining the typeclass methods of (a) by hand

### Example

```wiki
{-# LANGUAGE TypeFamilies
           , ConstraintKinds
           , FlexibleInstances
           , FlexibleContexts
           , UndecidableInstances
           , PatternSynonyms
           , ViewPatterns
#-}
module New where

import GHC.Exts(Constraint)
import Data.Void

-- ...

data RdrName
-- = the definition of RdrName

data SrcSpan
-- = the definition of SrcSpan

-- ----------------------------------------------
-- AST Base
-- ----------------------------------------------

data Exp x
  = Var (XVar x) (XId x)
  | Abs (XAbs x) (XId x) (Exp x)
  | App (XApp x) (Exp x) (Exp x)
  | Par (XPar x) (Exp x)
  | New (XNew x)

type family XVar x
type family XAbs x
type family XApp x
type family XPar x
type family XNew x

type family XId  x

type ForallX (p :: * -> Constraint) x
  = ( p (XVar x)
    , p (XAbs x)
    , p (XApp x)
    , p (XPar x)
    , p (XNew x)
    )

-- ----------------------------------------------
-- AST Ps
-- ----------------------------------------------

data Ps

type ExpPs  = Exp Ps

type instance XVar Ps = SrcSpan
type instance XAbs Ps = SrcSpan
type instance XApp Ps = SrcSpan
type instance XPar Ps = SrcSpan
type instance XNew Ps = Void

type instance XId  Ps = RdrName

-- ----------------------------------------------
-- HasSpan Typeclass
-- ----------------------------------------------

class HasSpan a where
  getSpan :: a -> SrcSpan
  setSpan :: a -> SrcSpan -> a

instance HasSpan SrcSpan where
  getSpan   = id
  setSpan _ = id

instance HasSpan Void where
  getSpan x   = absurd x
  setSpan x _ = absurd x

instance ForallX HasSpan x => HasSpan (Exp x) where
  getSpan (Var ex _)      = getSpan ex
  getSpan (Abs ex _ _)    = getSpan ex
  getSpan (App ex _ _)    = getSpan ex
  getSpan (New ex)        = getSpan ex

  setSpan (Var ex x)   sp = Var (setSpan ex sp) x
  setSpan (Abs ex x n) sp = Abs (setSpan ex sp) x n
  setSpan (App ex l m) sp = App (setSpan ex sp) l m
  setSpan (New ex)     sp = New (setSpan ex sp)

getSpan' :: HasSpan a => a -> (SrcSpan , a)
getSpan' m = (getSpan m , m)

pattern L :: HasSpan a => SrcSpan -> a -> a
pattern L s m <- (getSpan' -> (s , m))
  where
        L s m =  setSpan m s

-- ----------------------------------------------
-- Example Function
-- ----------------------------------------------

par :: Exp Ps -> Exp Ps
par l@(L sp m) = Par sp l
```