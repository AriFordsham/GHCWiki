# Intrinsic Superclasses

**Note** This page is a new version of the [DefaultSuperclassInstances](default-superclass-instances) proposal, and may ultimately supplant it. For now, I'm keeping the two separate as the delta is large, superficially at least.

## The Problem


Sometimes we want to refactor the type class hierarchy, and it always hurts. The typical scenario is that we have a library class

```wiki
class C x where
  f :: ...
  g :: ...
```


but then realise that `C` is the "has `g`" special case of a useful general notion `S`, so we would really prefer to have had the library supply

```wiki
class S x where
  f :: ...
class S x => C x where
  g :: ...
```


The cost of the generalization is that all our old `C` instances must be split into a `C` and an `S`. Client code breaks all over the place and people complain bitterly. This is the reason to resist making `Functor` and `Applicative` superclasses of `Monad`. It would have been pleasant to introduce `Applicative` as a generalization of `Monad` and somehow have all our old `Monad` instances generate `Applicative` instances too.

**Requirement 1** It should somehow be possible for an instance definition in source code to generate more than one instance internally.


However, the problem deepens. What if we actually had

```wiki
class C x where
  f :: ...
  f =  ...g...
  g :: ...
```


to start with? That is, types with a `C` instance can be given an `S` instance in a "standard" way, but there are other types which have `S` instances defined differently and no `C` instance at all. Again, that is a familiar situation: every `Monad` has is `Applicative` with `(<*>) = ap`, but non-monadic `Applicative` instances have `(<*>)` defined in other ways. But now our split hits trouble. We cannot have

```wiki
class S x where
  f :: ...
  f =  ...g...
class S x => C x where
  g :: ...
```


because (technically) `g` is no longer in scope for the default `f` definition and (morally) because only the `S`s which are also `C` should have that default definition anyway: the default `f` definition rightly belongs in the declaration of `C`, but `f` is not a method of `C`.

**Requirement 2** Some subclasses should somehow be able to offer default definitions for things in some of their superclasses.
