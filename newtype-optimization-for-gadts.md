
As discussed in [ \#1965](https://ghc.haskell.org/trac/ghc/ticket/1965), it would be useful to use the newtype optimization for GADTs and existentials under the following conditions, as described by SPJ:

1. Only one constructor
1. Only one field with nonzero width in that constructor (counting constraints as fields)
1. That field is marked strict
1. That field has a boxed (or polymorphic) type


I believe condition 2 can be relaxed very slightly, to allow constraints known to be zero-width. For example, equality constraints should be fine. So should classes that have no methods and no superclasses with methods.


Unlike a true `newtype`, pattern matching on the constructor *must* force the contents to maintain type safety.


Sample uses:

### Avoiding indirection while maintaining shape invariants

```
dataShape=Empty|NonEmptydataIntMap a = forall (e ::Shape).IntMap!(IMGadt e a)dataIMGadt(e ::Shape) a whereBin::!Prefix->!Mask->!(IMGadtNonEmpty a)->!(IMGadtNonEmpty a)->IMGadtNonEmpty a
  Tip::!Key-> a ->IMGadtNonEmpty a
  Nil::IMGadtEmpty a
```


If the `IntMap` type gets the newtype optimization, then we'd drop the extra indirection on top.

### Layering evidence

```
dataFoo a b c =Foo1 a b |Foo2 a b
dataBar f a b c whereBar::Family1 a b ~True=>!(f a b c)->Bar a b c
dataBaz f a b c whereBaz::Family2 b c ~True=>!(f a b c)->Baz a b c
newtypeQuux f a b c =Quux(Baz(BarFoo) a b c)
```


With the newtype optimization, I can layer the `Baz` type information on top of the `Bar` type information on top of the `Foo` type without having to pay a penalty. The alternative today is to make an entirely separate GADT for each combination of type information I want to hold evidence of.


I know few details about Core, but I would *guess* that the only Core feature we'd need to add is a flag for each data type indicating whether it is eligible for the newtype optimization. If so, then that optimization can be applied in code generation. There might be some future tuning to adjust how the inliner treats the constructor, but that doesn't seem at all urgent for now.
