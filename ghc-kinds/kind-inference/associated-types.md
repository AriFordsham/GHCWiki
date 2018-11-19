# Associated type consistency

## The basic setup


Consider this, ignoring kind polymorphism for now

```wiki
  class C b where
    type F a b c
  instance C [x] where
    type F p [x] q = blah
```


In the *class* decl, the second parameter of F is the same as the first parameter of C.
We'll call these the *corresponding parameter positions of C and F*.  For each associated
type `F` in a class `C`, we can (based on the class declaration alone) make a list of
corresponding argument positions, such as:

- Arg 1 of C corresponds to Arg 2 of F


There may be any number of these pairs.  (Although zero would suggest it is not really
an associated type at all.)


Note that both C and F can have other, unrelated parameters.


The basic consistency property for associated-family instances is this

- In an *instance* decl, we want C and F to have the same arguments in their corresponding argument positions.

### Kind polymorphism


Where kind polymorphism is involved, we think of ``argument position* as applying uniformly
to both visible and invisible arguments.  For example:
*

```wiki
class C (a :: k) where
  type T k :: Type
```


Here `C` has an invisible `k` argument, thus:

```wiki
class C @k (a :: k) where
  type T k :: Type 
```


So the first (invisible) argument position of `C` corresponds to the first (visible) argument
position of `T`.

### Alpha conversion


Should we allow this?

```wiki
  instance C [x] where
    type F p [y] q = blah
```


Here we have chosen a different name for the argument to F, but we could relax our criterion a bit, to say

- In an *instance* decl, we want C and F to have the same arguments in their corresponding argument positions,

*modulo alpha-conversion*.

---

## Two possible positions


There seem to be two possible approaches to kind-checking an instance
declaration for an associated types.  Here is a running example:

```wiki
  instance C [x] where
    type F p [y] q = blah
```

1.  (Simpler, but more kind annotations.) Kind-check the `type instance` declaration entirely independently from the class `instance` declaration.  Then check the family instance consistency condition.

1.  (More complex, but fewer annotations.) Include family-instance consistency when kind-checking the `type instance` decl.  This may or may not make a subsequent family instance consistency check redundant.


Note that if we adopt (B) we should do so consistently in other related situations. For example:

- Trac [\#15895](https://gitlab.haskell.org//ghc/ghc/issues/15895)

  ```wiki
  class Ríki ob where
    type Arr :: ob -> ob -> Type
    io  :: forall (a :: ob). Arr a a

  instance Ríki Type where
    type Arr = (->)    -- type Arr @Type = (->)
    io :: Arr a a      -- Does this meean  forall k (a::k). Arr @k a a
    io a = a           --  or              forall (a::Type). Arr @Type a a
  ```

- Trac [\#14111](https://gitlab.haskell.org//ghc/ghc/issues/14111) comment:2

  ```wiki
  data family   F (a :: k)
  data instance F (a :: Bool) where
    MkF :: F a
  ```

  This is currently rejected but under (B) perhaps it should not be.

---

## Examples

### indexed-types/should_compile/T10815

```wiki
type family Any :: k

class C (a :: k) where  -- C :: forall k. k -> Constraint
  type F (b :: k)       -- F :: forall k. k -> Type
  -- Corresponding positions: first arg position of Funct and Codomain
  -- (But the second argument position does not correspond.)

instance C 'True where
  type F x = Int
```


Looked at in isolation, there is nothing to say that `x :: Bool`.  But arguably
we want to infer

```wiki
class C @k (a :: k) where
  type F @k (b :: k)
   -- Corresponding positions: first arg of C and F

instance C @Bool 'True where
  type F @Bool (x :: Bool) = Int
```


It's a bit indirect: you can only tell that `x::Bool` by looking at the
corresponding-argument info from the class declaration.  You can't tell
jsut from `F`'s kind.

### indexed-types/should_fail/T13972

```wiki
class C (a :: k) where
  type T k :: Type

instance C Left where
  type T (a -> Either a b) = Int
```


This should obviously be accepted.  If we write out the invisible bits we have:

```wiki
class C @k (a :: k) where
  type T k :: Type

instance C @(p -> Either p q) (Left @p @q) where
  type T (a -> Either a b) = Int
```


The instantiation of `T` is the same as that for `C`, in the first arg position of each.

### polykinds/T9574

```wiki
data KProxy (t :: Type) = KProxy  -- KProxy :: Type -> Type
data Proxy p                      -- Proxy :: forall k. k -> Type
data NatTr (c :: o -> Type)       -- NatTr :: forall o. (o -> Type) -> Type

class Funct f where             -- Funct :: forall k. k -> Constraint
  type Codomain f :: Type       -- Codomain :: forall k. k -> Type
  -- Corresponding positions: first two arg positions of Funct and Codomain

instance Funct ('KProxy :: KProxy o) where
  type Codomain 'KProxy = NatTr (Proxy :: o -> Type)
```


One question is whether `o` even scopes over the associated type.


To make this typecheck we need

```wiki
instance Funct @(KProxy o) ('KProxy :: KProxy o) where
  type Codomain @(KProxy o) 'KProxy = NatTr (Proxy :: o -> Type)
```

### indexed-types/should_fail/T12041

```wiki
class Category (p :: i -> i -> Type) where
                                 -- Category :: forall i. (i->i->Type) -> Constraint
  type Ob p :: i -> Constraint   -- Ob :: forall i. (i->i->Type) -> i -> Constraint

data I a b  -- I :: forall k1 k2. k1 -> k2 -> Type

instance Category I where 
  type Ob I = (~) Int
     -- NB:  (~) Int :: Type -> Type
```


Perhaps we should reject with

```wiki
T12041.hs:15:8: error:
  Type indexes must match class instance head
      Expected: Ob i (I i i)
        Actual: Ob * (I * *)
        In the type instance declaration for Ob   
```


Or I suppose we could even say that the nested type instance influences
the kind at which `Category` is intantiated in the instance.
