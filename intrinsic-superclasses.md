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


If only we could write something like

```wiki
class S x where
  f :: ...
class (instance S x) => C x where
  g :: ...
  f =  ...g...
```


where the extra `instance` marking the superclass constraint makes `S` an **intrinsic** superclass of `C`, so that `f` can be treated as if it were a method of `C` for purposes of `C`'s instances and for default definition in the `C` class declaration.


If this machinery had been in place when `Applicative` was invented, we could just have given

```wiki
class (instance Functor f) => Applicative f where
  return :: x -> f x
  (<*>)  :: f (a -> b) -> f a -> f b
  fmap = (<*>) . return
class (instance Applicative m) => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  mf <*> ma = mf >>= \ f -> ma >>= \ a -> return (f a)
```


and, moreover, we could have chosen to give `Monad` its own specialized `fmap`

```wiki
  fmap f ma = ma >>= \ a -> return (f a)
```

**Requirement 3** Subclass declarations should somehow be able to override default definitions from their superclass declarations. Such overriding default definitions should be further overridable in sub-subclass declarations and in instance definitions.


Even if we can build a technology which supports such a treatment, we face further problems rolling it out across the legacy codebase. We hit some trouble if we take an existing subclass and make it intrinsic, e.g.,

```wiki
class (instance Eq x) => Ord x where
  compare :: x -> x -> Ordering
  x == y = case compare x y of {EQ -> True; _ -> False}
```


because every old `Ord` instance will now generate an `Eq` instance for which a duplicate *must* already exist. Worse is the situation with `Monad` and `Applicative` where we make an existing class into a *new* superclass and make it intrinsic: the prior constraints no longer make the whereabouts of duplicated `Applicative` instances particularly predictable.

**Requirement 4** At least transitionally, we must somehow ensure that client code which supplies explicit instances now duplicated by intrinsic superclass instances is broken as infrequently as possible.


We face not only conflicts between explicit and intrinsic instances, but also between multiple intrinsic instances. We should expect

```wiki
class (instance Functor t, instance Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  fmap f = runIdentity . traverse (Identity . f)
  foldMap f = runConst . traverse (Const . f)
```


but now if we define

```wiki
data Square x = x :& x
instance Monad Square where
  return x = x :& x
  (a :& a') >>= f = case (f a, f a') of
    (b :& _, _ :& b') -> b :& b'
instance Traversable Square where
  traverse f (a :& a') = return (:&) <*> f a <*> f a'
```


then we have silently generated duplicate instances for `Functor Square` and no particular reason to choose one over the other.

**Requirement 5** Whatever mechanism we employ for generating instances, we need an explicit means to disable it.


We might perhaps write

```wiki
data Square x = x :& x
instance Monad Square - Functor where
  ...
instance Traversable Square where
  ...
```


to inhibit the `Functor` instance arising from `Monad` but retain that from `Traversable`. It is probably a good thing in any case to be clear about which instances should be generated and which not.

**Requirement 6** Except for managing the transition of legacy code, we should ensure that the meaning of an instance definition is clear only from its class declaration (and those of its superclasses) and not deduced from the presence or absence of other instances.

## Terminology and Notation


To nail down the technicalities of the proposal, we shall need names for things, and notation to present the things thus named.


Firstly, let us talk about the stuff which gets declared in classes, defined by default in classes, and defined in instances:

- The **immediate members** of a class C are the methods and associated type and data families which are declared in the class declaration for C.
- The **members** of a class C are the methods and associated type and data families which may be defined in instance definitions for C.
- A **defaulted member** of a class C is a member with a default definition which will be added to any C instance which does not contain an overriding definition.
- An **immediately defaulted member** of a class C is a member of class C which is given a default definition in the declaration of C.


In current Haskell, all members are immediate, but this proposal seeks to change that. Similarly, the only defaulted members we currently have are immediately defaulted immediate members. Data families cannot be defaulted (because that could lead to the duplication of data constructors), but methods and type families can be defaulted.


Next, let us fix terminology for talking about the superclasses of a class.

- An **immediate superclass** S of a class C is any class which heads a constraint in the declaration of C.
- A **superclass** of C is either C or a proper superclass of C.
- A **proper superclass** of C is a superclass of an immediate superclass of C.


Note that the use of "immediate" is consistent in that it applies to things which are introduced explicitly in class declarations.


Now, let us allow some superclass constraints in class declarations to be labelled with the `instance` keyword.

*decl* ::= ...

>
> \| `class` (*sups*`=>`)? *Name**name*+ `where`*members*

*sups* ::= *sup* \| `(`*sup*,\*`)`

*atom* ::= *Name**type*+

*sup* ::= *atom* \| `instance`*atom* (`-`*Name*+)


(Grammar grammar: postfix ? for 0 or 1, postfix + for 1 or more, postfix ,\* for 0 or more comma-separated)


We can say what are the "intrinsic" superclasses of a class, with "immediate" and "proper" used as above.

- An **immediate intrinsic superclass** S of a class C is any class *Name*d in an `instance`*sup* in the declaration of C.
- An **intrinsic superclass** of C is either C or a proper intrinsic superclass of C
- A **proper intrinsic superclass** of C is an intrinsic superclass of an immediate intrinsic superclass of C.
