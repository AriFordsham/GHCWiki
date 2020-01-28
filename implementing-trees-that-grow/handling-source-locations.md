# Handling of Source Locations in Trees that Grow

This wikipage describes a design for putting source locations inside a new *extension point* of TTG.
The motivation is to allow GHC to sprinkle source locations across many nodes of the syntax tree,
without forcing every client of `HsSyn` to do so.  For example Template Haskell probably does not want to.

(NB: This wiki page was overhauled in Jan 2020. If you came here via a link, a very similar design to this was called 'Solution D' in the previous version: see the version history.)

## Design

Recall that [the TTG paper](http://www.jucs.org/jucs_23_1/trees_that_grow/jucs_23_01_0042_0062_najd.pdf) 
indexes each data type with a *type index*, and defines types with two extension points:

1. **Each data type has an *extension constructor*.**
2. **Each data construcutor has an *extension field*.**

For example:
```
data Expr p = Var (XVar p) (Var p)
            | App (XApp p) (Expr p) (Expr p)
            | Lam (XLam p) (Var p) (Expr p)
            | XExpr (XXExpr p)

type family XVar p
type family XApp p
type family XLam p
type family XXExpr p
```
Here the type index is `p`; the extension constructor is `XExpr`; and
the extension fields are `XVar p`, `XApp p`, etc (i.e. the first field
of each constructor).  The types `XVar`, `XApp` etc are type families,
that can be extended with new instances as you add new indexing types.

We add a third extension point:

3. **Selected fields, often the recursive uses, have an *extension wrapper*, `XRec`.**

Thus:
```
data Expr p = Var (XVar p) (Var p)
            | App (XApp p) (XRec p (Expr p)) (XRec p (Expr p))
            | Lam (XLam p) (XRec p (Var p)) (XRec p (Expr p))
            | XExpr (XXExpr p)

type family XRec p a
```
Again, `XRec` is typically a type family.  For some indices (say `Vanilla`)
we can easily elide all these `XRec` wrappers:
```
type instance XRec Vanilla a = a

But for GHC we can use it to add a source location for each `XRec`:
```
type instance XRec (GhcPass p) a = Located a
```
Note: There is nothing inherently recursive about `XRec`. It has that name
because it is often
used to wrap recursive fields, such as the arguments to `App` above.
But it is also used to wrap non-recursive fields, such as the `XRec p (Var p)` field of
`Lam`.
For name bikeshedding, see #17587.)

## Using XRec in GHC

Becoming specific to GHC, wherever we used `Located` in the AST previously, we now use `XRec pass`:
```diff
-type LConDecl pass = Located (ConDecl pass)
+type LConDecl pass = XRec pass (ConDecl pass)

data ConDecl pass
  = ConDeclGADT { ... }
  | ConDeclH98
      { ...
-      , con_name :: Located (IdP pass)
+      , con_name :: XRec pass (IdP pass)
      ...
      }
```

When we have an `LHsDecl (GhcPass p)` this yields the same AST as before, which makes refactoring much easier.

The idea of this refactoring is that `XRec pass` really just replaces the usage of `Located` across the AST everywhere,
this can be inside the `LHs*` type synonyms or record fields.
Remember: We have to replace every `Located` in the AST to achieve the goal of moving `SrcLoc`s to an extension point.

---

Here are some other usecases of `XRec` across the AST:
```hs
type HsDeriving pass = XRec pass [LHsDerivingClause pass]

type LHsDerivingClause pass = XRec pass (HsDerivingClause pass)

-- essentially expanding to:
type HsDeriving pass = XRec pass [XRec pass (HsDerivingClause pass)]
```
(`Located` could be interlieved with other functors (e.g. `[]`) before wrapping anything GHC AST again.)

```hs
data HsDataDefn pass
  = HsDataDefn { ...
                 dd_cType  :: Maybe (XRec pass CType),
                 ...
               }
```
(`Located` was sometimes wrapped around non-AST data like `CType` or simply `Bool`)

---

## Some of the thinking


* We want to have an AST that can be used for both TH and normal Hs, for example. This means we need to store source locations in an extension point of TTG ([trees that grow](https://gitlab.haskell.org/ghc/ghc/wikis/implementing-trees-that-grow)).
* In #15495 we agreed that the currently available extension points in TTG don't suffice to annotate the AST with `SrcLoc`s in a satisfyingly type-safe manner: Something akin to ['Ping-pong' style](#ping-pong-style) is desireable.
* Using a type family `XRec` instead of `Located` enables the extension points to carry source locations, or not, or even something else, everywhere that `Located` would be used today.

Applications for this include:
* Using the GHC TTG AST for TemplateHaskell. TH doesn't have any `SrcLoc`s attached to it, so it would use
  ```hs
  type instance XRec TemplateHaskell a = a
  ```
* Attaching [api annotations]() to the GHC TTG AST directly, instead of through the [`pm_annotations` field](https://gitlab.haskell.org/ghc/ghc/blob/3dae006fc424e768bb43fc73851a08fefcb732a5/compiler/main/GHC.hs#L813) in `ParsedModule`. This is outlined as one possible approach in the wiki page ['in tree api annotations'](https://gitlab.haskell.org/ghc/ghc/wikis/implementing-trees-that-grow/in-tree-api-annotations).

## Changes

### In GHC

GHC's functions' bodies will mostly **not need to change** (with some exceptions). This refactor pretty much only touches the types.
Some of GHC's functions used `unLoc :: Located a -> a`, but were polymorphic in the pass before:
```diff
-isForeignImport :: LForeignDecl pass -> Bool
+isForeignImport :: LForeignDecl (GhcPass p) -> Bool
isForeignImport (L _ (ForeignImport {})) = True
isForeignImport _                        = False
```

And lastly, some instance declarations that used `TypeSynonymInstances` now need to be expanded, now that we're using type families inside those type synonyms.

### In Haddock

Haddock doesn't use `GhcPass p`, but it uses source locations and GHC's AST heavily. Luckily we can just define an `XRec` instance for their pass datakind: `DocNameI`:

```hs
type instance XRec DocNameI a = Located a
```

All functions that both GHC and Haddock depend on need to be polymorphic over the `pass` type variable however. Luckily again, there are not many of these functions, they mostly live in `Ghc/Hs/Utils.hs`. They will need another constraint: `XRec pass (Match pass) ~ Located (Match pass)`, for example.

## Appendix

### Status

The design described here is not merged into GHC as of yet. Current status is:
* An older variant of the design (pre #17587) is implemented and merged for `Pat.hs`.
* The newer variant for `Pat.hs` is implemented in !2315
* The design is applied throughout GHC in !2315, but that is still WIP.

For previous status/discussions, see the older version of this file or the related issues.

### Related Issues / MRs

- Initial discussion: #15495
  -> Discussion settles in the design of !1970 (From now on referred to as the '`LPat` experiment')
- Follow up ticket: #17587 -> Simplifies extension point design and makes it more flexible
- Follow up merge request: !2315 (Expanding the '`LPat` experiment' across the compiler)
- Relevant applications for this:
  1. !2182
  1. [In tree Api annotations](https://gitlab.haskell.org/ghc/ghc/wikis/implementing-trees-that-grow/in-tree-api-annotations) see #17638


### 'Ping-pong style'

Say we have an expression type `Expr`. Ping-pong style refers to the recursion being made through a type synonym `LExpr`, for example, which would allow adding source locations easily:
```hs
data Expr
  = Add LExpr LExpr
  | Mul LExpr LExpr
  | Num Int

type LExpr = Located Expr

newtype Located a = ...
```

This way it is possible to distinguish an expression which has source locations attached and one which doesn't have source locations attached on the type level. `LExpr` doesn't unify with `Expr`.

If this were done with the constructor extension point of TTG, then one would lose some type safety: There would no longer be a guaruntee that there will always be a `Located` layer between the `Expr` layers in our huge expression sandwich.

There are [two very relevant comments](https://gitlab.haskell.org/ghc/ghc/issues/15495#note_227959) in #15495.