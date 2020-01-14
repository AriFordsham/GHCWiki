This page is intended as a more durable discussion point for issues identified in !2418 and #17638.

You can find more about GHC's TTG design on its wiki pages:
* [GHC Trees That Grow design](https://gitlab.haskell.org/ghc/ghc/wikis/implementing-trees-that-grow)

Current open questions are

- What about `Located RdrName`?
- What about annotations on lists?

From https://gitlab.haskell.org/ghc/ghc/issues/17638#note_245760 pointing out !2315, consider using `XRec`.

## Adding API Annotations to `Located RdrName`, 

Possible approach

```haskell
type family XRec p (f :: * -> *) = r | r -> p f
type instance XRec GhcPs f = LocatedA (f GhcPs)
type instance XRec GhcRn f = Located  (f GhcRn)
type instance XRec GhcTc f = Located  (f GhcTc)

type LocatedA = GenLocated SrcSpanAnn

data SrcSpanAnn = SrcSpanAnn { ann :: ApiAnn, loc :: SrcSpan }

---------------
-- Add an (unused) type parameter to each name type, so it can be used in `XRec`
data RdrName p = RdrName String
data Name    p = Name String
data Id      p = Id String

-- | Maps the "normal" id type for a given pass
type family IdPP p :: * -> *
type instance IdPP GhcPs   = RdrName
type instance IdPP GhcRn   = Name
type instance IdPP GhcTc   = Id
type instance IdPP GhcTcId = Id

type IdP p = IdPP p p

-----------------
-- So, for an example data type

data Exp p = Exp1 (LIdP p)
           | Exp2 (LIdP p) (LIdP p)

type LIdP p = XRec p (IdPP p)
type LExp p = XRec p Exp

-- Monomorphic stuff is straightforward
doFooR :: LIdP GhcRn -> LExp GhcRn
doFooR ln = noLoc (Exp1 ln)

doFooP :: IdP GhcPs -> LExp GhcPs
doFooP n = noLocP (Exp1 (noLocP n))

noLocP :: a GhcPs -> XRec GhcPs a
noLocP e = L (SrcSpanAnn ApiAnnNotUsed noSrcSpan) e

-- --------------------------

-- This is messy
baz :: XRec (GhcPass p) Exp ~ GenLocated l (Exp (GhcPass p))
    => LExp (GhcPass p) -> Int
baz (L _ (Exp1 ln)) = 4

```


