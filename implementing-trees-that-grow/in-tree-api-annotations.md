This page is intended as a more durable discussion point for issues identified in !2418 and #17638.

You can find more about GHC's TTG design on its wiki pages:
* [GHC Trees That Grow design](https://gitlab.haskell.org/ghc/ghc/wikis/implementing-trees-that-grow)

Current open questions are

- What about `Located RdrName`?
- What about annotations on lists?

From https://gitlab.haskell.org/ghc/ghc/issues/17638#note_245760 pointing out !2315, consider using `XRec`.

## Doodling

For `Located RdrName`, 


```haskell
type family XRec p (f :: * -> *) = r | r -> p f
type instance XRec GhcPs f = LocatedA (f GhcPs)
type instance XRec GhcRn f = Located  (f GhcRn)
type instance XRec GhcTc f = Located  (f GhcTc)

type LocatedA = GenLocated SrcSpanAnn

data SrcSpanAnn = SrcSpanAnn { ann :: ApiAnn, loc :: SrcSpan }

type family IdP p :: * -> *
type instance IdP GhcPs   = RdrName
type instance IdP GhcRn   = Name
type instance IdP GhcTc   = Id
type instance IdP GhcTcId = Id

type LIdP p = XRec p (IdP p)
```

To make this work, there needs to be a phantom parameter for the pass to `RdrName`, `Name`, and `Id`.

```haskell
data RdrName p
  = Unqual OccName
...
```

Could this work?
