
Using `Data.Reflection` has some runtime costs. Notably, there can be no inlining or unboxing of reified values. I think it would be nice to add a GHC special to support it.

## The problem


The following is a somewhat modified version of the main idea in `Data.Reflection`, with some relatively minor changes to clean up the very core of it.

```
-- Edward Kmett found these were necessary for his library, so they're likely necessary here too, for now.{-# OPTIONS_GHC -fno-cse #-}{-# OPTIONS_GHC -fno-full-laziness #-}{-# OPTIONS_GHC -fno-float-in #-}newtypeTagged s a =Tagged{ unTagged :: a }unproxy::(Proxy s -> a)->Tagged s a
unproxy f =Tagged(f Proxy)classReifies s a | s -> a where
  reflect' ::Tagged s a

-- For conveniencereflect:: forall s a proxy .Reifies s a => proxy s -> a
reflect_= unTagged (reflect' ::Tagged s a)-- The key function--see below regarding implementationreify'::(forall s .Reifies s a =>Tagged s r)-> a -> r

-- For conveniencereify:: a ->(forall s .Reifies s a =>Proxy s -> r)-> r
reify a f = reify' (unproxy f) a
```


The key idea of `reify'` is that something of type

```
forall s .Reifies s a =>Tagged s r
```


is represented in memory exactly the same as a function of type `a -> r`.


We currently use `unsafeCoerce` to interpret one as the other. Following the general approach of the library, we can do this as such:

```
newtypeMagic a r =Magic(forall s .Reifies s a =>Tagged s r)reify'::(forall s .Reifies s a =>Tagged s r)-> a -> r
reify' f = unsafeCoerce (Magic f)
```


This certainly works. The trouble is that any knowledge about what is reflected goes unused. For instance, if I write

```
reify12$\p -> reflect p +3
```


then GHC will not see, at compile time, that the result is 15. If I write

```
reify(+1)$\p -> reflect p x
```


then GHC will never inline the application of `(+1)`. Etc. It appears that the `forall s` in `Magic` gums up the inliner somehow.

### A larger example of the problem


mpickering asked for a more substantial example, so here's one:

```
newtypeM s =M{getM ::Int}mkM::Reifies s Int=>Int->M s
mkM= normalize .M{-# INLINE mkM #-}instanceReifies s Int=>Num(M s)whereM x +M y = normalize (M(x + y))M x -M y = normalize (M(x - y))M x *M y = normalize (M(x * y))
  abs x = x
  signum x =1
  fromInteger = mkM . fromIntegral
  {-# INLINE (+) #-}normalize::Reifies s Int=>M s ->M s
normalize m@(M x)=M$ x `mod` reflect m
{-# INLINE normalize #-}unInt::Int->Int#unInt(I# x)= x
{-# INLINE unInt #-}test1::Int#->Int#->Int#->Int#test1 m x y = unInt $ reify (I# m)$\(_::Proxy s)-> getM $(mkM (I# x)+ mkM (I# y)::M s)
```


Ideally, `test1` should never have to box anything, but because nothing inlines, we get this rather unsatisfactory result:

```wiki
test1 :: Int# -> Int# -> Int# -> Int#
test1 =
  \ (m_a15J :: Int#) (x_a15K :: Int#) (y_a15L :: Int#) ->
    case ((\ (@ s_i1kJ) ($dReifies_i1kK :: Reifies s_i1kJ Int) ->
             case $dReifies_i1kK `cast` ... of _ { I# ww1_a1sH ->
             case ww1_a1sH of wild_a1sJ {
               __DEFAULT ->
                 case modInt# x_a15K wild_a1sJ of ww2_a2ov { __DEFAULT ->
                 case modInt# y_a15L wild_a1sJ of ww4_X2pz { __DEFAULT ->
                 case modInt# (+# ww2_a2ov ww4_X2pz) wild_a1sJ
                 of ww5_X2pH { __DEFAULT ->
                 I# ww5_X2pH
                 }
                 }
                 };
               -1# -> test2;
               0# -> case divZeroError of wild1_00 { }
             }
             })
          `cast` ...)
           (I# m_a15J)
    of _ { I# x1_a15I ->
    x1_a15I
    }
```

## Solutions


We basically want something that looks a fair bit like `reify'`, but that translates to safe coercions in Core.

### Class/deriving-based


Simon Peyton Jones suggests making `reify#` a class method and using the class deriving mechanism to ensure safety. His first choice thus far seems to be

```
classReifiable(a ::*)wheretypeRC a ::Constraint
  reify#::(RC a => r)-> a -> r
```


but he also mentions an alternative class design, which I'll rename for clarity:

```
classReflectableTF(c ::Constraint)wheretypeRL c ::*
  reify#::(c => r)->RL c -> r
```

`Reifiable` seems to have reasonably good ergonomics, but it seems a bit odd to me for the payload type to determine the class. Furthermore, it's not at all clear to me how the class could be *named* in the `deriving` clause. `ReflectableTF` strikes me as rather better behaved, but it has fairly bad ergonomics (it requires explicit type application to pin down `c` when calling `reify#`).


One last class-based option I think might be worthy of consideration is to use `Reflectable` with a data family:

```
classReflectableDF(c ::Constraint)wheredataRL c ::*
  reify#::(c => r)->RL c -> r
```


This offers much better inference than `ReflectableTF`, because the instance is selected by the second argument to `reify#`. The constructor name could be chosen by the deriving mechanism to match the class name, so we'd get, e.g.,

```
instanceReflectableDF(Typeable a)wherenewtypeRL(Typeable a)=Typeable(TypeRep a)
  reify# f =...
```

### Non-class-based


I'm personally reluctant to commit to any particular class-based solution before it's been tried out in user-space. None of them are perfect, and we don't know for sure which will work out best in practice. My current thinking is that perhaps the best way to *start* is to add a `reify#` function

```
reify#::(c => r)-> a -> r
```


with a special typing rule: `reify# @ c r a` will only be accepted by the type checker if `c` is a single-method class whose method has the same representation as `a`.
