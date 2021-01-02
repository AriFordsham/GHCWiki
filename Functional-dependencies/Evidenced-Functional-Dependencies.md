This page seeks to elaborate the ideas of [Elaboration on functional dependencies](https://people.cs.kuleuven.be/~tom.schrijvers/portfolio/haskell2017a.html), Karachalias and Schrijvers, Haskell Symposium 2017, and further connect them to issues.

The repo https://gitlab.haskell.org/obsidiansystems/efd-everywhere has some examples which should probably be worked into this wiki page.

### Example 0: Injective type families, too

Injective type families also benefit from this. ["Injective type families for Haskell"](http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/injective-type-families-acm.pdf) from The Haskell Symposium 2015 introduces two borderline type families

```haskell
type family W1 a = r | r -> a 
type instance W1 [a] = a
```

```haskell
type family W2 a = r | r -> a
type instance W2 [a] = W2 a
```

The first "works" only if no new instances are added, the second "works" only if the instance is considered vacuous so `W2` in fact has an empty domain.

Using the basic idea of ["Constrained Type Families"](https://arxiv.org/pdf/1706.09715.pdf) of making all type families associated so we can reason about their domains we can in fact make both of these accepted:

For the first:

```haskell
{-# LANGUAGE TypeFamilies #-}

-- | W1 from "Injective Type Families"
module W1 where

type family FInv a

class (FInv (F a) ~ a) => FClass a where
  type F a

----

type instance FInv a = [a]

instance FClass [a] where
  type F [a] = a
```

The instance head `FInv a` eloquently prevents further instances without violating any open world rules!

The second fails with a translation following the above, because it needs `GInv (G a):= [a]`, which is not an instance we are allowed to define. But, we can postulate it as in instance constraint!

Also, a little imagination demonstrates that if we had `μ a. [a]` as a type, `W2` could also be defined on that, and indeed, postulating `a ~ [a]` works too.

```haskell
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | from "Injective Type Families"
module W2 where

type family GInv a

class (GInv (G a) ~ a) => GClass a where
  type G a

instance (GInv (G a) ~ [a], GClass a) => GClass [a] where
  type G [a] = G a

-- A little thinking shows us that `μ a. [a]` would work, if we had it, and
-- indeed GHC agrees (counterfactually):

class (GInv (G a) ~ a) => G'Class a where
  type G' a

instance (a ~ [a], GClass a) => G'Class [a] where
  type G' [a] = G a
```

This all goes to show that not only does the desugaring put functional dependencies on firmer ground thanks to the trust we have in System FC, it also makes functional dependencies more flexible with a better treatment of restricted domains and absurd reasoning. 

Still, while an automated desugaring could make `W1` work, it probably shouldn't try to make `W2` work.

### Example 1: liberal coverage breaks termination

See https://gitlab.haskell.org/ghc/ghc/-/wikis/Functional-dependencies#example-1-liberal-coverage-breaks-termination. We can encode this as:

```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Nontermination where

type family MulRes a b
class MulRes a b ~ c => Mul a b c

data Vec a = Vec a a a

type instance MulRes a (Vec b) = Vec (MulRes a b)
instance Mul a b c => Mul a (Vec b) (Vec c)

f :: Mul alpha (Vec beta) beta => (alpha, beta)
f = (undefined, undefined)

-- solver overflows stack
g :: (alpha, beta)
g = f
```

And we get the same issue. This shows that getting rid of LICC is no panacea. I think that's OK. My instinct is that the wanted constraint should be "blamed" rather than the instances. Specifically, as long as the pattern restrictions are obeyed, any solution is finite, and so i think solving should remain decidable? The wanted constraint exploits the solver's weaknesses to cause the issue.

### Example 2: LCC and LICC do weird improvement (#10675)

Compare https://gitlab.haskell.org/ghc/ghc/-/wikis/Functional-dependencies#example-2-lcc-and-licc-do-weird-improvement-10675-op

```haskell
class CX x a b | a -> b where
  op :: x -> a -> b
instance                                CX Bool [x] [x]
instance {-# LIBERAL #-} CX Char x y => CX Char [x] [Maybe y]

f x = op True [x]
```
Per the issue, there are two ways to interpret this situation:

1. The program should be rejected because `[x]` is mapped two different ways
2. The program is OK, because the second instance is vacuous.

The type family desugaring sides with the first one:

```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module T10675_Conflicting where

type family CX_FD0 a

class CX_FD0 a ~ b => CX x a b where
  op :: x -> a -> b

type instance CX_FD0 [x] = [x]
instance CX Bool [x] [x]

-- conflicting head
type instance CX_FD0 [x] = [Maybe (CX_FD0 x)]
instance CX Char x y => CX Char [x] [Maybe y]

f x = op True [x]
```
Even if `CX Char x y` is unsatisfied, but we stuck with conflicting instances heads for `CX_FD0`.

There are perhaps variant encoding that make this go: e.g. Making `CX_FD0 [x] ~ [Maybe (CX_FD0 x)]` and instance constraint for the second instance, but, like `W2`, this is probably best left as out of scope for an automated desugaring.

### Example 3: LCC and LICC threaten confluence

C.f. https://gitlab.haskell.org/ghc/ghc/-/wikis/Functional-dependencies#example-3-lcc-and-licc-threaten-confluence. Just like the previous example, when we translate to using type families the instances are rejected so we don't get as far as conflicting rewrites.

```haskell
type family FD a
class FD b ~ c => D a b c

-- D instances rejected as FD instances never match

type instance FD Int = (Int, Int)
instance {-# LIBERAL #-} (q ~ Int)  => D Int  p (Int,q)

type instance FD Bool = (Bool, Bool)
instance {-# LIBERAL #-} (s ~ Bool) => D Bool r (s,Bool)
```

### Examples: Overlapping Instances not considered

#### Deciding type equality

The Schrijvers et al 2017 paper Definition 6.12 (Non-overlapping Instances) rules out frequently-used coding idioms, including a type-level type equality test:

```haskell
class TypeEq a b (res :: Bool)  | a b -> res
instance r ~ True  => TypeEq a a r
instance r ~ False => TypeEq a b r
```

Owing to the fact that this class has no methods, we can collapse into a single instance using a closed type family:

```haskell
type family TypeEqTF a b where
  TypeEqTF a a = True
  TypeEqTF _ _ = False

class TypeEqTF a b ~ r => TypeEq a b r
instance TypeEqTF a b ~ r => TypeEq a b r
```

#### Adding natural numbers with 3-way fun dep

Here's a particularly extreme use of Overlaps+Liberal coverage+breaking Paterson+exploiting bogus FunDep Instance consistency for adding type-level Nats.

```haskell
class Add a b c  | a b -> c, a c -> b, b c -> a  
    
instance Add Z b b 
instance {-# OVERLAPPABLE #-} (a ~ S a', c ~ S c', Add a' b c') => Add a b c
```

Note the three-way FunDep improvement -- a feat not usually considered possible. The `OVERLAPPABLE` instance relies on type improvement from bare tyvars in the instance head (which are required to ensure the head is more general) via equality constraints `a ~ S a'` -- a kinda type-level lazy pattern.

I [ADC] don't see how that could be automatically translated into TFs -- it at least needs closed TFs to mimic the overlap. (How would a translation know how to sequence the equations?) The improvement `a ~ S a'` would need a TF something like this, which is not allowed:

```haskell
type family ForceSa a  where ForceSa a = S a'
```

Here's my (@AntC3's) working:

```haskell
class (FAddab a b ~ c, FSubtac a c ~ b, FSubtbc b c ~ a) => AddTF a b c
instance (FAddab a b ~ c, FSubtac a c ~ b, FSubtbc b c ~ a) => AddTF a b c
    
type family FAddab a b  where
    FAddab Z      b = b
    FAddab (S a') b = S (FAddab a' b)
type family FSubtac a c  where               -- subtract a from c, by recursive descent on a
    FSubtac Z      c      = c
    FSubtac (S a') (S c') = FSubtac a' c'
type family FSubtbc b c  where                -- subtract b from c, by type eq test??
    FSubtbc b b      = Z
    FSubtbc b (S c') = S (FSubtbc b c')
```

For the last TF `FSubtbc`, I've been faithful to the FunDep version by relying on a type equal match. I've not represented the `c ~ S c'` type improvement explicitly, but just built a parameter `(S c')` directly. It seems to work.

For a more mechanistic translation of `{-# OVERLAPPABLE #-}`, https://arxiv.org/pdf/1706.09715.pdf has "closed type classes" which work analogously to closed type families. This works only for closed world, not open world overlapping, however.

@Ericson2314 doesn't know whether that is enough for idiomatic `{-# OVERLAPS #-}`. If not, perhaps we'll need to generalize the total ordering of closed type families to a partial ordereing. We'll also have to be much more careful with cross-module overlapping instances today about consistency, as the soundness of the type system is at stake. In particular, it might be possible only have the `{-# OVERLAPPING #-}`, rather than `{-# OVERLAPPABLE #-}` instance in the upstream module, unless they agree w.r.t all fun deps.