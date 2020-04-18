# Type & Class Dependency Analysis

## Background: The `HsGroup` Story

In GHC, every Haskell module is split into so-called `HsGroup`s. The boundaries of these groups are determined by top-level `TemplateHaskell` declaration splices (see `findSplice` in `GHC.Rename.Source`).

For example, here we have two `HsGroup`s:

```haskell
data A
x = 5

$(return [])

data B
```

The first `HsGroup` contains `data A` and `x = 5`, and the second `HsGroup` contains `data B`.

It is impossible to refer to a name defined in a later `HsGroup`: 

```
data A = MkA B  -- error, B is not visible here
$(return [])
data B
```

The boundary this creates has significant implications for the Template Haskell code that runs in-between. A splice can only reify types and declarations that are defined in preceding groups, and the declarations it generates are only visible in the groups that follow.

Consider the following program:

```
data Nat = Zero | Succ Nat
$(genSingletons [''Nat])
data SomeNat = forall n. SomeNat (SNat n)
```

Here, `genSingletons` can reify a declaration from a preceding group (`Nat`), and the declaration it generates (`SNat`) is used in a group that follows.

Swapping these lines would break the program:

```
data SomeNat = forall n. SomeNat (SNat n)  -- Error: SNat not available here.
$(genSingletons [''Nat])                   -- Error: Nat not available here.
data Nat = Zero | Succ Nat
```

In other words, when it comes to `HsGroup`s, GHC does not try to be smart and reorder anything. Splices are processed in a top-to-bottom order. And even when a splice is a no-op (`$(return [])`), it still introduces a boundary.

This is simple and predictable. However, it can also be inconvenient. For example:

```
data Foo = MkFoo { ... }
makeLenses ''Foo

fn a b c = ... -- Cannot use `Bar` lenses here! :-(

data Bar = MkBar { ... }
makeLenses ''Bar
```

This is not considered a major issue.

## The `TyClGroup` Story

Within one `HsGroup`, the order of declarations should not matter. These programs should both work:

```
data X = MkX
data Y (a :: X)
```

```
data Y (a :: X)
data X = MkX
```

The definition of `Y` depends on `X`. Both examples are valid, even though in the latter one `X` is defined after `Y`. This works because the renamer handles the entire `HsGroup` at once. It collects the identifiers bound by every LHS, brings them into scope, and then does name resolution in another pass over the declarations.

The type-checker (kind-checker, really) cannot work this way. In the example above, kind-checking `Y` requires an already kind-checked `TyCon` for `X`. So the kind-checker needs to process declarations in a specific order, with mutually-recursive declarations grouped together.

That's why within every `HsGroup`, after renaming and before type-checking, we perform dependency analysis, producing `TyClGroup`s.

A `TyClGroup` is defined thus:

```
-- | Type or Class Group
data TyClGroup pass  -- See Note [TyClGroups and dependency analysis]
  = TyClGroup { group_ext    :: XCTyClGroup pass
              , group_tyclds :: [LTyClDecl pass]
              , group_roles  :: [LRoleAnnotDecl pass]
              , group_kisigs :: [LStandaloneKindSig pass]
              , group_instds :: [LInstDecl pass] }
  | XTyClGroup (XXTyClGroup pass)

type instance XCTyClGroup (GhcPass _) = NoExtField
type instance XXTyClGroup (GhcPass _) = NoExtCon
```

Therefore, it contains zero or more of the following:

* data and newtype declarations (e.g. `data T = ...`)
* type synonyms (e.g. `type T = ...`)
* class declarations (e.g. `class C a where ...`)
* class instances (e.g. `instance C Int where ...`)
* closed type families (e.g. `type family F a where ...`)
* open type family headers (e.g. `type family F a`)
* open type family instances (e.g. `type instance F Int = ...`)
* data family headers (e.g. `data family D a`)
* data family instances (e.g. `data instance D Int = ...`)
* standalone kind signatures (e.g. `type T :: ...`)
* role annotations (e.g. `type role F nominal`)

It does not contain term-level declarations (e.g. `x = 5`), term-level type signatures (e.g. `x :: Int`), or any other varieties of declarations.

The purpose of the type and class dependency analysis is to take a list of declarations in the order that they appear in the source file and to sort them into `TyClGroup`s in such a way that kind-checking any given `TyClGroup` requires type constructors from preceding groups only.

For example, if the user writes:

```
data Y (a :: X)
data X = MkX
```

Then the `TyClGroup`s are:

1. `data X = MkX`
2. `data Y (a :: X)`

Kind-checking `Y` requires the type constructor `X`, so `X` must be put into a preceding `TyClGroup`.

## Dependency Analysis in GHC 8.10

As of GHC 8.10, the dependency analysis that builds `TyClGroup`s works as follows:

1. Build a directed graph where the nodes are `TyClDecl`s, the keys identifying those nodes are `Name`s, and the edges are free variables.

    For example:

    ```
    data A = MkA1 B | MkA2
    data B = MkB (Y A)
    type family F a
    type instance F (Y _) = Bool
    data Y a = MkY (F a)
    ```


    ```mermaid
    graph RL;
      A --> B
      B --> A
      B --> Y
      Y --> F
    ```

    Note that the type family instance is not subject to dependency analysis at this stage of the pipeline.

2. Do the SCC (strongly-connected component) analysis on the graph, producing `[[TyClDecl GhcRn]]`:

   1. ```haskell
      type family F a
      ```
   2. ```haskell
      data Y a = MkY (F a)
      ```
   3. ```haskell
      data A = MkA1 B | MkA2
      data B = MkB (Y A)
      ```

3. Go over these `[[TyClDecl GhcRn]]`, adding instances as soon as all of their free variables are available. For example, `type instance F (Y _) = Bool` has free variables `Y`, `Bool`, so it will be added to the group which introduces `Y`.

   At this point we produce the final `[TyClGroup]`:

   1. ```haskell
      type family F a
      ```
   2. ```haskell
      data Y a = MkY (F a)
      type instance F (Y _) = Bool
      ```
   3. ```haskell
      data A = MkA1 B | MkA2
      data B = MkB (Y A)
      ```

   At this stage we also handle role annotations and standalone kind signatures in a manner similar to instances, adding them after the graph has already been built.



