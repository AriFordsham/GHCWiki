# Kind polymorphism and datatype promotion


This page gives additional implementation details for the `-XPolyKinds` flag. The grand design is described in the paper [ Giving Haskell a Promotion](http://dreixel.net/research/pdf/ghp.pdf). Most of the work has been done and merged into GHC 7.4.1. The relevant user documentation is in \[the user's guide (add link when it's up)\] and on the [ Haskell wiki page](http://haskell.org/haskellwiki/GHC/Kinds). What still doesn't work, or doesn't work correctly, is described here.

# Explicit kind variables


Currently we do not handle kind variables in the source language. So the following is invalid, for instance:

```wiki
type family Apply (f :: k1 -> k2) (a :: k1)
```


Naturally we want to allow this. The syntax we propose is the one above, as described in the paper.
(At least until [ExplicitTypeApplication](explicit-type-application) gets implemented.)

**Future work:** allow kind variable annotation.
Since the core language has all the support for kind variables, this shouldn't be too hard.

# Kind defaulting in type families


At the moment, when you define a type family without `-XPolyKinds` like this:

```wiki
type family F a
```


it gets kind `* -> *`. There are no constraints on the kind of `a`, so we
default it to `*`. We also default the return kind of `F` to `*`.
The same happens for data families, and also for plain datatypes with phantom
types.


When you turn `-XPolyKinds` on, however, we currently give `F` the kind
`forall (k :: BOX). k -> *`. This is unsatisfactory for two reasons:

1. The behaviour of kind generalisation changes when we turn `-XPolyKinds` on,
  even though it doesn't really have to. We could still default to `*` unless
  you give a kind signature. So if you want `F` to be kind polymorphic, you
  should write `type family F (a :: k)`. This, of course, requires supporting
  [explicit kind variables](ghc-kinds#explicit-kind-variables).

1. Unlike the parameters, however, the return kind of `F` is defaulted to `*`.
  This seems rather arbitrary. We should either generalise both arguments and
  return kind, or default both. In case we choose to default, the more
  general kind can be obtained by giving a signature:

  ```wiki
  type family F (a :: k1) :: k2
  ```

**Future work:** do more consistent kind defaulting.
 

# [ \#5682](http://hackage.haskell.org/trac/ghc/ticket/5682) (proper handling of infix promoted constructors)


Bug report [ \#5682](http://hackage.haskell.org/trac/ghc/ticket/5682) shows a
problem in parsing promoted infix datatypes.

**Future work:** handle kind operators properly in the parser.

# Kind synonyms (from type synonym promotion)


At the moment we are not promoting type synonyms, i.e. the following is invalid:

```wiki
data Nat = Ze | Su Nat
type Nat2 = Nat

type family Add (m :: Nat2) (n :: Nat2) :: Nat2
```

**Future work:** promote type synonyms to kind synonyms.

# Better support for kinds in Template Haskell


Currently there is no support for promoted datatypes, or the kind `Constraint`, in Template Haskell.

**Future work:** address [ \#5612](http://hackage.haskell.org/trac/ghc/ticket/5612), designing and implementing a way for Template Haskell to reify the new kinds.

# Kind-polymorphic `Typeable`


The paper describes an improved implementation of `Typeable` (section 2.5). This has not
yet been implemented; the current `Typeable` class is:

```wiki
class Typeable (a :: *) where
  typeOf :: a -> TypeRep
```


The new proposal makes it into:

```wiki
data Proxy a = Proxy

class Typeable a where
  typeRep :: Proxy a -> TypeRep
```


Note that `Proxy` is kind polymorphic, and so is the new `Typeable`: its type argument
`a` can have any kind `k`. The paper goes on to describe how we can then give
kind-specific instances:

```wiki
instance Typeable Int where typeRep _ = ...

instance Typeable []  where typeRep _ = ...
```


The following changes need to done in the compiler:

- Update `Data.Typeable` in `base` (mostly deleting classes and adding `Proxy`).

- Rewrite the `deriving Typeable` mechanism in `TcGenDeriv`.


From the user's perspective nothing has to change. We can make the new implementation
backwards-compatible by:

- Calling the method of `Typeable``typeRep`, and not `typeOf`
- Defining `typeOf`, `typeOf1`, ..., separately


Concretely, the new `Data.Typeable` will look something like this:

```wiki
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds           #-}

-- Type representation: unchanged
data TypeRep = ...

-- Kind-polymorphic proxy
data Proxy t = Proxy

-- Kind-polymorphic Typeable
class Typeable a where
  typeRep :: Proxy a -> TypeRep

-- Instances for base types
instance Typeable Char   where ...
instance Typeable []     where ...
instance Typeable Either where ...

-- Old methods for backwards compatibility  
typeOf :: forall a. Typeable a => a -> TypeRep
typeOf x = typeRep (getType x) where
  getType :: a -> Proxy a
  getType _ = Proxy

typeOf1 :: forall t (a :: *). Typeable t => t a -> TypeRep
typeOf1 x = typeRep (getType1 x) where
  getType1 :: t a -> Proxy t
  getType1 _ = Proxy
```


This is nearly enough; remember that currently we can do things like this:

```wiki
typeOf  "p"
typeOf1 "p"
```


And they mean different things: the first is the representation of `[Char]`,
whereas the second is the representation of `[]`. In particular, 
`typeOf1 "p" == typeOf1 [()]`, for instance. To keep this behavior we have
to guarantee that a datatype `T` with type parameters `a1` through `an` gets instances:

```wiki
data T a1 ... an

instance Typeable T
instance (Typeable a1) => Typeable (T a1)
...
instance (Typeable a1, ..., Typeable an) => Typeable (T a1 ... an)
```


We can do this as before, by defining the arity `n-1` instance from the
arity `n` instance:

```wiki
instance (Typeable t, Typeable (a :: *)) => Typeable (t a)
instance (Typeable t, Typeable (a :: *), Typeable (b :: *)) => Typeable (t a b)
```


If we're willing to use `-XUndecidableInstances`, we can even do this with
a single instance, relying on `-XPolyKinds`:

```wiki
instance (Typeable t, Typeable a) => Typeable (t a)
```


In this instance, `t` has kind `k -> *` and `a` has kind `k`.

# Generalized Algebraic Data Kinds (GADKs)

**Future work:** this section deals with a proposal to collapse kinds and sorts into a single system
so as to allow Generalised Algebraic DataKinds (GADKs). The sort `BOX` should
become a kind, whose *kind* is again `BOX`. Kinds would no longer be classified by sorts;
they would be classified by kinds.


(As an aside, sets containing themselves result in an inconsistent system; see, for instance,
[ this example](http://www.cs.nott.ac.uk/~txa/g53cfr/l20.agda). This is not of practical
concern for Haskell.)


Collapsing kinds and sorts would allow some form of indexing on kinds. Consider the
following two types, currently not promotable in FC-pro:

```wiki
data Proxy a = Proxy

data Ind (n :: Nat) :: * where ...
```


In `Proxy`, `a` has kind `forall k. k`. This type is not promotable because
`a` does not have kind `*`. This is unfortunate, since a new feature (kind
polymorphism) is getting on the way of another new feature (promoting
datatypes). As for `Ind`, it takes an argument of kind (promoted) `Nat`,
which renders it non-promotable. Why is this? Well, promoted `Proxy` and `Ind`
would have sorts:

```wiki
Proxy  :: forall s. s -> BOX

Ind    :: 'Nat -> BOX
```


But `s` is a sort variable, and `'Nat` is the sort arising from promoting
the kind `Nat` (which itself arose from promoting a datatype). FC-pro has
neither sort variables nor promoted sorts. However, if there are no sorts, and
`BOX` is the **kind** of all kinds, the "sorts" ("kinds", now) of promoted `Proxy`
and `Ind` become:

```wiki
Proxy  :: forall k. k  -> BOX

Ind    :: Nat          -> BOX
```


Now instead of sort variables we have kind variables, and we do not need to promote
`Nat` again.


Kind indexing alone should not require kind equality constraints; we always
require type/kind signatures for kind polymorphic stuff, so then
[ wobbly types](http://research.microsoft.com/en-us/um/people/simonpj/papers/gadt/gadt-rigid-contexts.pdf)
can be used to type check generalised algebraic kinds, avoiding the need for
coercions. While this would still require some implementation effort, it
should be "doable".
