# Optimising newtype-like data types with existentials

## Motivation


Consider this data type declaration

```wiki
data T where
  MkT :: !(Foo a) -> T
```


So `a` is an existentially bound variable, and we cannot use a newtype for `T`.  So, as things stand, a value of type `T` will be represented by a heap-allocated box containing a single pointer to a value of type `(Foo ty)` for some `ty`.  


This is tantalising.  The extra indirection and allocation gains nothing.  Since `MkT` is strict in its only argument, we could (at codegen time) *represent* a value of type `T` by a value of type `Foo ty`.

*This page does not propose any change whatsoever to the language*. Rather, it proposes
that we guarantee an efficient unboxed representation for certain data types.


The ticket to track this idea is [\#1965](https://gitlab.haskell.org//ghc/ghc/issues/1965). 

## Main design


Under what conditions can we guarantee to remove the box for a data type entirely?

1. Only one constructor in the data type
1. Only one field with nonzero width in that constructor (counting constraints as fields).
1. That field is marked strict
1. That field has a boxed (or polymorphic) type


Note that an equality constraint arising from a GADT has zero width, and thus is covered by (2).  E.g.

```wiki
data T a where
  MkT :: !Int -> T Int
```


The constructor actually has type

```wiki
  MkT :: forall a. (a ~# Int) => Int -> T a
```


So we could represent a value of type `(MkT a)` by a plain `Int`, without an indirection, because the evidence for `(a ~# Int)` is zero width.


You might think that a single constructor GADT is probably not much use, but see Example 2 below.

## Some discussion

### Superclasses


David F: I believe condition 2 can be relaxed very slightly, to allow constraints known to be zero-width. For example, equality constraints should be fine. So should classes that have no methods and no superclasses with methods.  For example, given

```
classThis a ~Int=>Foo a
classFoo a =>Bar a wheredataBarType a
classBar a =>Baz a where
  prox ::Proxy# a
```


David F: I *imagine* that `Foo a`, `Bar a`, and `Baz a` contexts are zero-width.


SLPJ: no, class constraints are always boxed because they can be bottom (with recursive classes).  I don't know how to avoid this.

### Strictness


Unlike a true `newtype`, pattern matching on the constructor *must* force the contents to maintain type safety.  In particular, matching on the constructor reveals an existential and/or type information. As Dan Doel found, and pumpkin relayed in [ https://ghc.haskell.org/trac/ghc/ticket/1965\#comment:16](https://ghc.haskell.org/trac/ghc/ticket/1965#comment:16), we have to be careful not to reveal such information without forcing the evidence. Since we're using the newtype optimization, the evidence is in the contained field itself.

*SLPJ: I do not understand this paragraph.  Remember, we propose no change to the source language semantics*.

## Sample uses

### Example 1


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

### Example 2

>
> David Feuer: A single-constructor GADT can add a payload to something like `Refl`; it could also be used with a strict type-aligned sequence to "count", layering on length indexing. Admittedly not earth-shattering, but not totally useless. *SLPJ: I still don't get it.  Could you give an example?*


For instance,

```
dataTList c x y whereNil::TList c x x
  Cons::!(c x y)->TList c y z ->TList c x z

dataNat=Z|SNatdataLengthIncrement c p q whereInc::!(c x y)->LengthIncrement c '(S n, x)'(n, y)
```


Now `TList (LengthIncrement c) '(m, x) '(n, y)` represents a type-aligned list taking a path from `x` to `y`, and having a length of `m - n`. So while a single-constructor GADT may not be much use *on its own*, it can do something interesting when combined with another, multi-constructor GADT!

### Example 3

>
> AntC: For anonymous records, you can wrap a payload in a newtype to label it. But if you want to restrict (say) PersonId to being an Int, and yet have it look the same, you have to make that an existential GADT. So now you're paying for the box.


For instance,


{{{\#lhs
data Lab1 a = Lab1 a deriving (Eq, Read, Show)


{\* can't do either of these:
data PersonId = PersonId Int
data PersonId a = PersonId Int
\*}


data PersonId a where

<table><tr><th>PersonId</th>
<td>(a \~ Int) =\> !a -\> PersonId a
</td></tr></table>


instance GetLabel (PersonId a) a where getLabel (PersonId x) = x


-- using tuples as anon records:
instance (a \~ a') =\> HasField PersonId (PersonId a, lb, lc) a' where

>
> getField _ x =  getLabel x


}}}


Of course there are many design choices for anon records and how to label their fields. But they'll all need that sort of type-indexed lookup.

### Layering evidence

```
dataFoo a b c =Foo1 a b |Foo2 a b
dataBar f a b c whereBar::Family1 a b ~True=>!(f a b c)->Bar a b c
dataBaz f a b c whereBaz::Family2 b c ~True=>!(f a b c)->Baz a b c
newtypeQuux f a b c =Quux(Baz(BarFoo) a b c)
```


With the newtype optimization, I can layer the `Baz` type information on top of the `Bar` type information on top of the `Foo` type without having to pay a penalty. The alternative today is to make an entirely separate GADT for each combination of type information I want to hold evidence of.

## Implementation


I know few details about Core, but I would *guess* that the only Core feature we'd need to add is a flag for each data type indicating whether it is eligible for the newtype optimization. If so, then that optimization can be applied in code generation when types are dropped. There might be some future tuning to adjust how the inliner treats the constructor, but that doesn't seem at all urgent for now.


That said, it would be nice also to try to avoid "double forcing" when digging through the constructors. 
