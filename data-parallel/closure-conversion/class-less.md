DataParallel/ClosureConversion Up?

## Closure conversion without indexed types


The following scheme approaches the problem of mixing converted and unconverted code from the point of view of GHC's Core representation, avoiding the use of classes as much as possible.  In particular, the scheme gracefully handles any declarations that themselves cannot be converted, but occur in a converted module.  The two essential ideas are that (1) we move between converted and unconverted values/code using a conversion isomorphism and (2) we treat unconverted declarations differently depending on whether or not they involve arrows; e.g., the definition of `Int` by way of unboxed values (which we cannot convert) doesn't prevent us from using `Int`s *as is* in converted code.

### Conversion status


All `TyCon`s, `DataCon`s, and `Id`s have a *conversion status* that determines how occurences of these entities are treated during conversion.  For an `Id` named `v`, we have two alternatives:

1. The binding of `v` was compiled without conversion and we have to use `v` itself in converted code, which requires the use of an in-place conversion function.
1. Otherwise, we have a converted variant `v_CC`, and we use `v_CC` instead of `v` in converted code.


For a type constructor `T` and its data constructors `C`, we have three alternatives:

1. The declaration introducing `T` and its constructors was compiled without conversion or we were unable to convert it, as it uses some language feature that prevents conversion.
1. A converted variant `T_CC` exists, but coincides with `T` (e.g., because `T` neither directly nor indirectly involves arrows).
1. A converted variant `T_CC` exists and differs from `T`.


In the last two cases, we also have a *conversion constructor*`isoT` whose type and meaning is described below.


An example of a feature that prevents conversion are unboxed values.  We cannot make a closure from a function that has an unboxed argument, as we can neither instantiate the parametric polymorphic closure type with unboxed types, nor can we put unboxed values into the existentially quantified environment of a closure.

### Converting types

#### The closure type


We represent closures by

```wiki
data a :-> b = forall e. !(e -> a -> b) :$ e
```


and define closure application as

```wiki
($:) :: (a :-> b) -> a -> b
(f :$ e) $: x = f e x
```


So, we have `(->)_CC == (:->)`.

#### Conversion of type terms


We determine the converted type `t^` of `t` as follows:

```wiki
T^            = T_CC , if T_CC exists
              = T    , otherwise
a^            = a_CC
(t1 -> t2)^   = t1 -> t2   , if kindOf t1 == #
                             or kindOf t2 == #
              = t1^ :-> t2^, otherwise
(t1 t2)^      = t1^ t2^
(forall a.t)^ = forall a_CC.t^
```


Here some examples,

```wiki
(Int -> Int)^           = Int :-> Int
(forall a. [a] -> [a])^ = [a] :-> [a]
([Int -> Int] -> Int)^  = [Int :-> Int] :-> Int
(Int# -> Int# -> Int#)^ = Int# -> Int# -> Int#
((Int -> Int) -> Int#)^ = (Int -> Int) -> Int#
(Int -> Int -> Int#)^   = Int :-> (Int -> Int#)
```


Why do we use `(t1 -> t2)^ = t1 -> t2` when either argument type is unboxed, instead of producing `t1^ -> t2^`?  Because we want to avoid creating conversion constructors (see below) for such types.  After all, the conversion constructor `isoArr` for function arrows works only for arrows of kind `*->*->*`.

### Conversion constructors


To move between `t` and `t^` we use conversion functions.  And to deal with type constructors, we need *conversion constructors*; i.e., functions that map conversion functions for type arguments to conversion functions for compound types.

#### Conversion pairs


Conversion functions come in pairs, which we wrap with the following data type for convenience:

```wiki
data a :<->: b = (:<->:) {to :: a -> b, fr ::b -> a}
```


The functions witness the isomorphism between the two representations, as usual.

#### Types of convercion constructors


The type of a conversion constructor depends on the kind of the converted type constructor:

```wiki
isoTy (t::k1->k2) = forall a a_CC.
                      isoTy (a::k1) -> isoTy (t a::k2)
isoTy (t::*)      = t :<->: t^
```


where type conversion `t^` is defined below.


As an example, consider

```wiki
data T (f::*->*) = T1 (f Int) | T2 (f Bool)
```


The type of the conversion constructor is as follows :

```wiki
isoTy (T::(*->*)->*) =
  forall f f_CC. 
    (forall a a_CC. 
       (a :<->: a_CC) -> (f a :<->: f_CC a_CC)) ->
    T f :<->: T_CC f_CC
```


The conversion constructor might be implemented as

```wiki
isoT isof = toT :<->: frT
  where
    toT (T1 x) = T1 (to (isof isoInt ) x)
    toT (T2 y) = T2 (to (isof isoBool) y)
    frT (T1 x) = T1 (fr (isof isoInt ) x)
    frT (T2 y) = T2 (fr (isof isoBool) y)
```


where `isoInt` and `isoBool` are the conversion constructors for `Int`s and `Bool`s.


Moreover, the conversion constructor for function arrows is

```wiki
isoArr :: a :<->: a_CC   -- argument conversion
       -> b :<->: b_CC   -- result conversion
       -> (a -> b) :<->: (a_CC :-> b_CC)
isoArr (toa :<->: fra) (tob :<->: frb) = toArr :<->: frArr
  where
    toArr f        = const (tob . f . fra) :$ ()
    frArr (f :$ e) = frb . f e . toa
```

### Conversions

#### Rules


To perform the actual conversion of values of a type `t::*`, we generate a conversion `iso<t>` of type `t :<->: t^` as follows:

```wiki
iso<T>          = isoT           , if T_CC exists
                = idIso<*>       , otherwise
iso<a::k>       = idIso<k>
iso<t1 -> t2>   = idIso<*>       , if kindOf t1 == #
                                   or kindOf t2 == #
                = isoArr         , otherwise 
                    iso<t1> iso<t2>
iso<t1 t2>      = iso<t1> iso<t2>
iso<forall a.t> = iso<t>
```


where

```wiki
idIso<*>      = id :<->: id
idIso<k1->k2> = \_ -> (idIso<k2>)
```

#### Examples


Here some example conversions:

```wiki
iso<Int -> Int>     = isoArr isoInt isoInt
iso<Int -> Int#>    = id :<->: id
iso<[a -> a]>       = isoList (isoArr (id :<->: id) 
                                      (id :<->: id))
iso<f (Int -> Int)> = (\_ -> (id :<->: id))
                        (isoArr isoInt isoInt)
                    = id :<-> id
```

### Converting type declarations

#### Conversion rules


If a type declaration for constructor `T` occurs in a converted module, we need to decide whether to convert the declaration of `T`.  We decide this as follows:

1. If the declaration of `T` mentions another algebraic type constructor `S` for which there is **no**`S_CC`, then we cannot convert `T`.
1. If **all** algebraic type constructors `S` mentioned in `T`'s definiton have a conversion `S_CC  == S`, we do not convert `T`, but set `T_CC == T` and generate a suitable conversion constructor `isoT`.  (NB: The condition implies that `T` does not mention any function arrows.)
1. If the declaration of `T` uses any features that we cannot (or for the moment, don't want to) convert, simply don't convert it.
1. Otherwise, we generate a converted type declaration `T_CC` together with a conversion constructor `isoT`.  Conversion proceeds by converting all data constructors (see below).


Moreover, we handle other forms of type constructors as follows:

- `FunTyCon`: We have `(->)_CC = (:->)`.
- `TupleTyCon`: We have `(,..,)_CC = (,..,)`.  We may either have a (long) list of conversion constructors `iso(,..,)` pre-defined or need to generate them inline by generating a suitable case expression where needed.
- `SynTyCon`: Closure conversion operates on `coreView`; hence, we will see no synonyms.  (Well, we may see synonym families, but will treat them as not convertible for the moment.)
- `PrimTyCon`: We essentially ignore primitive types during conversion, assuming that their converted and unconverted forms coincide.  As they cannot contain values of other types, we need no conversion constructor.
- `CoercionTyCon` and `SuperKindTyCon`: They don't categorise values and are ignored during conversion.

#### Conversion constructor


Whenever we have a converted type constructor `T_CC`, we also need to generate a conversion constructor `isoT`.  If `T` has one or more arguments, the conversion is non-trivial, even for `T_CC == T`.

#### Converting data constructors


We convert a data constructor `C :: t1 -> ... -> tn` by generating a converted constructor `C_CC :: t1^ -> .. -> tn^`.  This includes the generation of a corresponding new worker `Id`.  For example, if the original worker has the type signature

```wiki
MkT :: (Int -> Int) -> Int
```


the converted worker is 

```wiki
MkT_CC :: (Int :-> Int) -> Int
```


As a consequence, whenever we convert a *partial* wrapper application in an expression, we need to introduce a closure on the spot.  (Simon pointed out that this is a rare case anyway.)


We do not specially handle wrappers of data constructors.  They are converted just like any other toplevel function.

#### Examples


For example, when we convert

```wiki
data Int = I# Int#
```


we get `Int_CC = Int` and we have

```wiki
isoInt :: Int :<->: Int
isoInt = toInt :<->: frInt
  where
    toInt (I# i#) = I# i#
    frInt (I# i#) = I# i#
```


As another example,

```wiki
data Maybe a = Nothing | Just a
```


implies `Maybe_CC = Maybe` and

```wiki
isoMaybe :: (a :<->: a_CC) -> (Maybe a :<->: Maybe a_CC)
isoMaybe isoa = toMaybe :<->: frMaybe
  where
    toMaybe isoa Nothing  = Nothing
    toMaybe isoa (Just x) = Just (to isoa x)
    frMaybe isoa Nothing  = Nothing
    frMaybe isoa (Just x) = Just (fr isoa x)
```

### Converting classes and instances


We don't alter class and instance declarations in any way.  However, the dictionary type constructors and dfuns are converted in the same way as other data types and value bindings, respectively.


As an example, assume `Num Int` were defined as

```wiki
class Num a where
  (+)    :: a -> a -> a
  negate :: a -> a
instance Num Int where
  (+)    = primAddInt
  negate = primNegateInt
```


with the Core code being

```wiki
data Num a = 
  Num {
    (+)    :: a -> a -> a,
    negate :: a -> a
  }
dNumInt = Num Int
dNumInt = Num primAddInt primNegateInt
```


Then, closure conversion gives us

```wiki
data Num_CC a =
  Num_CC {
    (+_CC)    :: a :-> a :-> a,
    negate_CC :: a :-> a
  }
dNumInt_CC :: Num_CC Int   -- as Int_CC = Int
dNumInt_CC = Num_CC 
               (to isoIntToIntToInt primAddInt) 
               (to isoIntToInt primNegateInt)
  where
    isoIntToIntToInt = isoArr isoInt isoIntToInt
    isoIntToInt      = isoArr isoInt isoInt
```

### Converting value bindings

#### Bindings


For every binding

```wiki
f :: t = e
```


we generate

```wiki
f_CC :: t^ = e^
```

#### Toplevel


When converting a toplevel binding for `f :: t`, we generate `f_CC :: t^` and redefine `f` as

```wiki
f :: t = fr iso<t> f_CC
```

#### Examples

---


chak: revision front

---

### Converting core terms


Apart from the standard rules, we need to handle the following special cases:

- We come across a value variable `v` where `idCC v == NoCC` whose type is `t`: we generate `convert t v` (see below).
- We come across a case expression where the scrutinised type `T` has `tyConCC T == NoCC`: we leave the case expression as is (i.e., unconverted), but make sure that the `idCC` field of all variables bound by patterns in the alternatives have their `idCC` field as `NoCC`.  (This implies that the previous case will kick in and convert the (unconverted) values obtained after decomposition.)
- Whenever we have an FC `cast` from or to a newtype `T`, where `tyConCC T == NoCC`, we need to add a `convert tau` or `trevnoc tau`, respectively.  We can spot these casts by inspecting the kind of every coercion used in a cast.  One side of the equality will have the newtype constructor.
- We come across a dfun: If its `idCC` field is `NoCC`, we keep the selection as is, but apply `convert t e` from it, where `t` is the type of the selected method and `e` the selection expression.  If `idCC` is `ConvCC d_CC`, and the dfun's class is converted, `d_CC` is fully converted.  If it's class is not converted, we also keep the selection unconverted, but have a bit less to do in `convert t e`.  **TODO** This needs to be fully worked out.

### TODO

#### Examples


Have an example with two modules one unconverted, where the converted imports the unconverted.


Also have an example that motivates why we have to vectorise/CC declarations such as `Int`.

#### Conversion functions


Similar to `HasGenerics` and instead of storing `Id` of conversion constructors, we can derive from the name of the `TyCon`.

#### Data constructors


How to exactly handle the worker and wrapper?  Can we replace arrows by closure types in the worker?  Or do we always have to add a wrapper?

**Simpler''' Don't try to make a complete cloned data constructor.  By the time of CC, its all just Core and so wrappers are just like any other global function.
**

#### Original functions


The previous story was that when vectorising `f` and generating `f_CC`, we now define

```wiki
f :: tau
f = trevnoc tau f_CC
```


Now, with the approximate conversion scheme above, we may not have `trevnoc tau`.  In this case, we still generate `f_CC`, but also leave the rhs of `f` alone (i.e., compile the original functions).


When we give up on converting a complete right-hand side, we still want to convert all subexpressions that we can convert.
