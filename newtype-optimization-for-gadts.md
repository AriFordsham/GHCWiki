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


The ticket to track this idea is #1965.


See also [this OCaml pull request](https://github.com/ocaml/ocaml/pull/606), which does exactly the same kind of thing for OCaml.

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
class This a ~ Int => Foo a
class Foo a => Bar a where
  data BarType a
class Bar a => Baz a where
  prox :: Proxy# a
```


David F: I *imagine* that `Foo a`, `Bar a`, and `Baz a` contexts are zero-width.


SLPJ: no, class constraints are always boxed because they can be bottom (with recursive classes).  I don't know how to avoid this.



David F: Does this mean even `~` constraints are boxed? If so, that monkeys with GADT constructors that have equality constraints involving type families.


```
data Foo a b where
  Foo :: TF1 a ~ TF2 b => Foo a b
```


seems rather nicer than the alternative


```
data Foo a b where
  Foo :: !(TF1 a :~: TF2 b) -> Foo a b
```


Do such constraints just pile on extra junk?

### Strictness


Unlike a true `newtype`, pattern matching on the constructor *must* force the contents to maintain type safety.  In particular, matching on the constructor reveals an existential and/or type information. As Dan Doel found, and pumpkin relayed in [https://ghc.haskell.org/trac/ghc/ticket/1965\#comment:16](https://ghc.haskell.org/trac/ghc/ticket/1965#comment:16), we have to be careful not to reveal such information without forcing the evidence. Since we're using the newtype optimization, the evidence is in the contained field itself.

*SLPJ: I do not understand this paragraph.  Remember, we propose no change to the source language semantics*.


David F: I was simply clarifying that while this is the "newtype optimization", it is \*not\* about an actual `newtype`. In both Haskell and Core it is `data`, although Core could flag it as unusual if that's helpful.

## Sample uses

### Example 1



You might think that an existential data type with only one field is a bit unusual.  Here is a example:


```
data Shape = Empty | NonEmpty

data IntMap a = forall (e :: Shape) . IntMap !(IMGadt e a)

data IMGadt (e :: Shape) a where
  Bin :: Prefix -> Mask -> IMGadt NonEmpty a -> IMGadt NonEmpty a -> IMGadt NonEmpty a
  Tip :: Key -> a -> IMGadt NonEmpty a
  Nil :: IMGadt Empty a
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
>
> David Feuer: A single-constructor GADT can add a payload to something like `Refl`; it could also be used with a strict type-aligned sequence to "count", layering on length indexing. Admittedly not earth-shattering, but not totally useless. *SLPJ: I still don't get it.  Could you give an example?*
>
>


For instance,


```
data TList c x y where
  Nil :: TList c x x
  Cons :: !(c x y) -> TList c y z -> TList c x z

data Nat = Z | S Nat

data LengthIncrement c p q where
  Inc :: !(c x y) -> LengthIncrement c '(S n, x) '(n, y)
```


Now `TList (LengthIncrement c) '(m, x) '(n, y)` represents a type-aligned list taking a path from `x` to `y`, and having a length of `m - n`. So while a single-constructor GADT may not be much use *on its own*, it can do something interesting when combined with another, multi-constructor GADT!


### Example 3


>
>
> AntC: For anonymous records, you can wrap a payload in a newtype to label it within the record. But if you want to restrict (say) PersonId to being an Int, and yet have it look polymorphic, you have to make that a GADT. So now you're paying for the box.
>
>


For instance,

```wiki
newtype Label1 a = Label1 a deriving (Eq, Read, Show)
instance GetLabel (Label1 a) a where getLabel (Label1 x) = x

{- can't do either of these:
newtype PersonId = PersonId Int      -- no type param
newtype PersonId a = PersonId Int    -- type param not used
-}

data PersonId a where
    PersonId :: !Int -> PersonId Int
instance GetLabel (PersonId a) a where getLabel (PersonId x) = x

-- using tuples as anon records:
instance (a ~ a', GetLabel (l a) a') => HasField l (l a, lb, lc) a' where
    getField _ (x, _, _) =  getLabel x
```


Of course there are many design choices for anon records and how to label their fields. (I'm trying not to pre-judge that.) But they'll all need that sort of type-indexed lookup -- including I think if the label is a type-level string.


>
>
> Those GetLabel instances are tedious. When we have full-bore ORF, we can declare every data type using record syntax, all with field name unLabel.
>
>

```wiki
data PersonId a where
    PersonId :: { unLabel :: !Int } -> PersonId Int

... getField _ (x, _, _) = unLabel x
```

### Layering evidence


```
data Foo a b c = Foo1 a b | Foo2 a b
data Bar f a b c where
  Bar :: Family1 a b ~ True => !(f a b c) -> Bar a b c
data Baz f a b c where
  Baz :: Family2 b c ~ True => !(f a b c) -> Baz a b c
newtype Quux f a b c = Quux (Baz (Bar Foo) a b c)
```


With the newtype optimization, I can layer the `Baz` type information on top of the `Bar` type information on top of the `Foo` type without having to pay a penalty. The alternative today is to make an entirely separate GADT for each combination of type information I want to hold evidence of.

## Implementation


Implementation is unfortunately tricky. Simply eliminating the boxing in Stg is
easy, and this by itself saves us two words per value + pointer dereferencing.
However, the generated code will be ugly, and if we could do this in Core
instead of Stg, the simplifier would be able to do some follow-up optimizations
and generate good code.


To be more specific, we want to do these transformations:


First:

```wiki
D arg1 arg2 ... argN
==>
nv_arg (where nv_arg is the only non-void argument)
```


(but we somehow need to bind other args or do substitution. If we do this Stg
though we don't need to bind those args as unarise doesn't care about what a
void argument is as long as it's void it gets rid of it and it can check
void-ness by looking at Id's type)


Second:

```wiki
case <exp1> of
  D arg1 arg2 ... argN -> <exp2>
==>
let arg1 = ...
    arg2 = ...
    arg3 = ...
 in <exp2>
```


(we know only one of these args will be non-void, but all of them should be
bound as they can be referred in \<exp2\>)


If we do this in Stg we lose some optimization opportunities and generate ugly
code. For example, if the first transformation happens in a let-binding RHS
maybe simplifier decides to inline it as it can't duplicate work after the
transformation. Similarly it can decide to inline the non-void argument after
second transformation which may lead to further optimizations etc.


For an example of an ugly code, suppose we had this:

```wiki
case <exp1> of
  D (T x) -> <exp2>
```


in Stg this looks like

```wiki
case <exp1> of
  D v -> case v of
           T x -> <exp2>
```


So now if we do the second transformation we get

```wiki
let v = <exp1> in
case v of
  T x -> <exp2>
```


but ideally we'd get

```wiki
case <exp1> of
  T x -> <exp2>
```


Simplifier would be able to do this after the second transformation.


So the problem is

- If we implement this in Stg we generate ugly code, and miss some optimization
  opportunities (and arguably it doesn't buy us much, it saves 2 words per
  allocation + pointer dereferencing)

- Implementing this in Core is very hard, if not impossible, without losing
  type safety.


(copied from [the mailing list discussion](https://mail.haskell.org/pipermail/ghc-devs/2016-September/012741.html))
