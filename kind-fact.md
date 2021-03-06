# Adding kind `Constraint`


This page describes an extension to kind system that supporsts a
kind **Constraint** to cover constraints as well as types, in
order to reuse existing abstraction mechanisms, notably **type
synonyms**, in the constraint language.


Much of the motivation for this proposal can be found in [Haskell Type Constraints Unleashed](http://www.cs.kuleuven.be/%7Etoms/Research/papers/constraint_families.pdf) which identifies the shortage of abstraction mechanisms for constraints relative to types. See ticket #788 for the resulting **constraint synonym** proposal, which seeks to fill some of the gaps with new declaration forms. Here, however, the plan is to extend the kind system, empowering the existing mechanisms to work with constraints. [ Max Bolingbroke](http://blog.omega-prime.co.uk/?p=61), commenting on [ context aliases](http://www.haskell.org/haskellwiki/Context_alias) (in turn based on John Meacham's [ class alias](http://repetae.net/recent/out/classalias.html) proposal) makes a similar suggestion, remarking that a new kind would probably help. The final design is largely the work of Conor McBride.


The new design has now been implemented by Max Bolingbroke.  It's in HEAD and upcoming GHC 7.4, and is described in [Max's Sept 2011 blog post](http://blog.omega-prime.co.uk/?p=127).

## The design: user's eye view

- Add a kind `Constraint` for constraints, so that, e.g. `Monad :: (* -> *) -> Constraint`.

- Close `Constraint` under tuples, so `(F1, .. Fn) :: Constraint` iff each `Fi :: Constraint`.

- Allow (rather, neglect to forbid) the use of `type` to introduce synonyms for Constraint(-constructing) things.  Thus one might say

  ```wiki
  type Transposable f = (Traversable f, Applicative f)
  type Reduce m x = (Monad m, Monoid (m x))
  type Stringy x = (Read x, Show x)
  ```

- Allow these synonym facts to appear wherever a class constraint can appear.  For example

  ```wiki
  class Stringy a => C a where ....
  f :: Reduce m x => x -> m x
  ```

- Allow nested tuple constraints, with componentwise unpacking and inference, so that `(Stringy x, Eq x)` is a valid constraint without flattening it to `(Read x, Show x, Eq x)`.

- Retain the policy of defaulting to kind `*` in ambiguous inference problems -- notably `()` is the unit type and the trivial constraint -- except where overridden by kind signatures.  For example:

  ```wiki
  type MyUnit = () -- gives the unit type by default
  type MyTrue = () :: Constraint  -- needs the kind signature to override the default
  ```

- Allow the **type family** mechanism to extend to the new kinds, pretty much straight out of the box. For example:

  ```wiki
  type family   HasDerivatives n     f :: Constraint
  type instance HasDerivatives Z     f  = ()
  type instance HasDerivatives (S n) f  = (Differentiable f, HasDerivatives n (D f))
  ```

  where `Differentiable` is the class of differentiable functors and `D f` is the associated derivative functor.

- You can abtract over type variables of kind `Constraint`. Here is a contrived example:

  ```wiki
  data T a where     -- Notice a :: Constraint
    MkT :: a => T a  -- Note the cool type:  a => blah

  f :: T (Ord x) -> x -> Bool
  f MkT x = x > x

  g :: T (Ord Int)
  g = MkT

  main = print (f g 4)   -- Computes 4>4 = False
  ```

  We could do with some convincing examples of why this is a good thing, but it simply falls out.

### More syntax


One might consider a syntax for giving fully explicit kinds to type synonyms, like this:

```wiki
type Reduce :: (* -> *) -> * -> Constraint where
  Reduce m x = (Monad m, Monoid (m x))
```

### Discussion

`Constraint` synonyms appear to overlap with superclases.  For example one could say

```wiki
class (Read x, Show x) => Stringy x where {}
```


But there are significant differences

- Using the class mechanism forces you to give an `instance` declaration too.  Perhaps something like

  ```wiki
  instance (Read x, Show x) => Stringy x where {}
  ```

  This is painful duplication, and (worse) there is little to stop you writing an overlapping instance later.  At least, looking at the instance doesn't tell you that no overlapping instance is intended.

- The type family mechanism gives new power.  For example, consider the celebrated collection example:

  ```wiki
  class Collection c where
    type family X c :: * -> Constraint
    empty :: c a
    insert :: (X c) => c a -> a -> c a

  instance Collection [] where
    type instance X [] a = Eq a
    insert xs x = ...

  instance Collection Data.Set where
    type instance X Data.Set a = Ord a
    insert s x = ...
  ```

  See [Bulk types with class](http://research.microsoft.com/en-us/um/people/simonpj/papers/collections.ps.gz).

---

## The design: implementation


These notes about the implementation are intended for GHC hackers, and logically from part of the GHC Commentary.

- A major change is that the data type `Type` (in module `TypeRep`) no longer has a `PredTy` construct.  Instead, we have just `Type`.  In a function type `t1 -> t2`, the argument `t1` is a *constraint argument* iff `t1 :: Constraint`.

- Constraint arguments are pretty-printed before a double arrow "`=>`" when displaying types.  Moreover they are passed implicitly in source code; for example if `f :: ty1 => ty2 -> ty3` then the Haskell programmer writes a call `(f e2)`, where `e2 :: ty2`, and the compiler fills in the first argument of type `ty1`.

- A constraint type (of kind `Constraint`) can take one of these forms

  - **Equality constraint**: `TyConApp eqTyCon [ty1, ty2]` 
  - **Class constraint**: `TyConApp tc [ty1, ty2]`, where `tc` is a `TyCon` whose `tyConClass_maybe` is `Just cls`.
  - **Implicit parameter**: `TyConApp tc [ty1]`, where `tc` is a `TyCon` whose `tyConIP_maybe` is `Just ip`.
  - **Tuple constraint**: `TyConApp tup_tc [ty1, ..., tyn]`, where `tup_tc` is a constraint tuple `TyCon`.  
  - **Constraint variable**: `TyVarTy tv` where `tv` has kind `Constraint`.

- Constraint types (i.e. types with kind `Constraint`) are always boxed. The constraint solver in the type checker deals solely in terms of boxed constraints.

- **Implicit parameters** have a type written `?x::Int`, say.  Concretely, this is represented as `TyConApp ?x [intTy]`, where `intTy` is the representation of the type for `Int`, and `?x` is a `TyCon` of kind `(* -> *)`.  There is an infinite family of such implicit-parameter `TyCon`s; see data constructor `IPTyCon` in data type `TyConParent` in `TyCon`.

- **Equality constraints**.  Constraint types are always boxed, including equality constraints.  So `(a ~ b)` is a *boxed* value.   We also have a primitive type of *unboxed* equality constraints, written `(a ~# b)`.  Roughly the former is declared thus:

  ```wiki
  data a ~ b = Eq# (a ~# b)
  ```

  where `Eq#` is a data constructor with a single, unboxed, zero-width field of type `(a ~# b)`.  See `TysWiredIn.eqTyCon`. 

  The reason we have both boxed and unboxed forms of equality constraint is that 

  - Boxed equality constraints `(a~b)` can be treated uniformly with all other constraints.  This is a big win in the type checker and, more particularly, in sitautions like

    ```wiki
      type Bla a b = (Eq a, a~b)
    ```

    We have no way to deal with a tuple with some boxed and some unboxed constraints.
  - Unboxed equality constraints `(a~#b)` can be implemented much more efficiently at runtime; they take no space, and are passed in zero-width registers (of which we have many!).

- **Constraint tuples** are needed for situations like

  ```wiki
  types X a = (Show a, Ix a)
  ```

  Although we use standard tuple syntax, internally we use a separate infinite family of tuple `TyCon`s, just as we separate the boxed and unboxed type families.  See `BasicTypes.TupleSort`.
