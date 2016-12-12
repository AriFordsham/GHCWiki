
Using `Data.Reflection` has some runtime costs. Notably, there can be no inlining or unboxing of reified values. I think it would be nice to add a GHC special to support it. I'll get right to the point of what I want, and then give a bit of background about why.

### What I want


I propose the following magical function:

```
reify#::(forall s . c s a => t s r)-> a -> r
```

`c` is assumed to be a single-method class with no superclasses whose dictionary representation is exactly the same as the representation of `a`, and `t s r` is assumed to be a newtype wrapper around `r`. In desugaring, `reify# f` would be turned into some coercion of `f`. See below regarding specialization concerns.

### Why I want it


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


So we can currently use `unsafeCoerce` to interpret one as the other. Following the general approach of the library, we can do this as such:

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


I'd like to replace `reify'` with `reify#` to allow such optimizations. mpickering asked for a more substantial example, so here's one:

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

### Why so general

```
reify#::(forall s . c s a => t s r)-> a -> r
```


is, of course, far more polymorphic than it has any right to be. In principle, we should only need one `c`, namely `Data.Reflection.Reifies`, and one `t`, namely `Data.Tagged.Tagged`. But making it polymorphic prevents us from having to use a class and type available in `ghc-prim`.

### Specialization concerns


I've found an approach that seems to get the inlining I want in userspace, by changing `Magic`, but I'm not confident it's safe.

```
typefamilySkolem::*wherenewtypeMagic s a r =Magic(Reifies s a =>Tagged s r)sreify':: forall a r .(forall (s ::*).Reifies s a =>Tagged s r)-> a -> r
sreify' k = unsafeCoerce (Magic2(k ::ReifiesSkolem a =>TaggedSkolem r))
```


My concern with this approach is that in sufficiently complex circumstances, the specializer might be able to conflate two different reified values of the same type, as each of them will, at a certain point, look like `Reifies Skolem A`. I haven't actually found a way to make this happen yet, but it smells fishy nonetheless. I would hope GHC would be able to guarantee that the argument to `reify#` will never be specialized to a particular instance of `c` (i.e., `Reifies`).


For comparison, this produces the following (much prettier) Core for the corresponding test:

```wiki
test2 :: Int# -> Int# -> Int# -> Int#
test2 =
  \ (m_a4rI :: Int#) (x_a4rJ :: Int#) (y_a4rK :: Int#) ->
    case m_a4rI of wild_a4G4 {
      __DEFAULT ->
        case modInt# x_a4rJ wild_a4G4 of ww2_a5Bo { __DEFAULT ->
        case modInt# y_a4rK wild_a4G4 of ww1_X5Cs { __DEFAULT ->
        modInt# (+# ww2_a5Bo ww1_X5Cs) wild_a4G4
        }
        };
      -1# -> 0#;
      0# -> case divZeroError of wild1_00 { }
    }
```