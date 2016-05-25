
As discussed in [ \#1965](https://ghc.haskell.org/trac/ghc/ticket/1965), consider this data type declaration

```wiki
data T where
  MkT :: !(Foo a) -> T
```


So `a` is an existentially bound variable, and we cannot use a newtype for `T`.  And yet, since `MkT` is strict in its only argument, we could (at codegen time) *represent* a value of type `T ty` by a value of type `Foo ty`.  


Under what conditions can we do this? 

1. Only one constructor in the data type
1. Only one field with nonzero width in that constructor (counting constraints as fields).
1. That field is marked strict
1. That field has a boxed (or polymorphic) type


Notes

- An equality constraint arising from a GADT has zero width, and thus is covered by (2).  E.g.

  ```wiki
  data T a where
    MkT :: !Int -> T Int
  ```

  The constructor actually has type

  ```wiki
    MkT :: forall a. (a ~# Int) => Int -> T a
  ```

  So we could represent a value of type `(MkT a)` by a plain `Int`, without an indirection, because the evidence for `(a ~# Int)` is zero width.

>
> Mind you, a single constructor GADT is probably not much use.

>
> David Feuer: A single-constructor GADT can add a payload to something like `Refl`; it could also be used with a strict type-aligned sequence to "count", layering on length indexing. Admittedly not earth-shattering, but not totally useless. *SLPJ: I still don't get it.  Could you give an example?*


For instance,

```
dataTList c x y whereNil::TList c x x
  Cons::!(c x y)->TList c y z ->TList c x z

dataNat=Z|SNatdataLengthIncrement c p q whereInc::!(c x y)->LengthIncrement c '(S n, x)'(n, y)
```


Now `TList (LengthIncrement c) '(m, x) '(n, y)` represents a type-aligned list taking a path from `x` to `y`, and having a length of `m - n`.


I believe condition 2 can be relaxed very slightly, to allow constraints known to be zero-width. For example, equality constraints should be fine. So should classes that have no methods and no superclasses with methods.  *SLPJ: I do not understand this paragraph.  Example please! *


For example, given

```
classThis a ~Int=>Foo a
classFoo a =>Bar a wheredataBarType a
classBar a =>Baz a where
  prox ::Proxy# a
```


I *imagine* that `Foo a`, `Bar a`, and `Baz a` contexts are zero-width.


Unlike a true `newtype`, pattern matching on the constructor *must* force the contents to maintain type safety. *SLPJ: I do not understand this paragraph.  Example please! *


In particular, matching on the constructor reveals an existential and/or type information. As Dan Doel found, and pumpkin relayed in [ https://ghc.haskell.org/trac/ghc/ticket/1965\#comment:16](https://ghc.haskell.org/trac/ghc/ticket/1965#comment:16), we have to be careful not to reveal such information without forcing the evidence. Since we're using the newtype optimization, the evidence is in the contained field itself.

## Sample uses

### Avoiding indirection while maintaining shape invariants


You might think that an existential data type with only one field is a bit unusual.  Here is a example:

```
dataShape=Empty|NonEmptydataIntMap a = forall (e ::Shape).IntMap!(IMGadt e a)dataIMGadt(e ::Shape) a whereBin::Prefix->Mask->IMGadtNonEmpty a ->IMGadtNonEmpty a ->IMGadtNonEmpty a
  Tip::Key-> a ->IMGadtNonEmpty a
  Nil::IMGadtEmpty a
```


Here `IntMap` obeys (1)-(4) and so `IntMap ty` could be represented (without indirection) by the underlying `(IMGadt e ty)` value, thereby saving an indirection at the root of every `IntMap`.


A more direct rendering would look like this

```wiki
data IntMap a = Empty | NonEmpty (NE a)
data NE a = Bin Prefix Mask (NE a) (NE a)
          | Tip Key a
```


No GADTs, no existentials.  But we get an indirection at the root of every non-empty `IntMap`.

### Layering evidence

```
dataFoo a b c =Foo1 a b |Foo2 a b
dataBar f a b c whereBar::Family1 a b ~True=>!(f a b c)->Bar a b c
dataBaz f a b c whereBaz::Family2 b c ~True=>!(f a b c)->Baz a b c
newtypeQuux f a b c =Quux(Baz(BarFoo) a b c)
```


With the newtype optimization, I can layer the `Baz` type information on top of the `Bar` type information on top of the `Foo` type without having to pay a penalty. The alternative today is to make an entirely separate GADT for each combination of type information I want to hold evidence of.

# Implementation


I know few details about Core, but I would *guess* that the only Core feature we'd need to add is a flag for each data type indicating whether it is eligible for the newtype optimization. If so, then that optimization can be applied in code generation when types are dropped. There might be some future tuning to adjust how the inliner treats the constructor, but that doesn't seem at all urgent for now.


That said, it would be nice also to try to avoid "double forcing" when digging through the constructors. 
