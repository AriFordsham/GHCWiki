# Default superclass instances


A matter of much consternation, here is a proposal to allow type class declarations to include default instance declarations for their superclasses. It's based on [ Jón Fairbairn's proposal](http://www.haskell.org//pipermail/haskell-prime/2006-August/001587.html), but it has a more explicit 'off switch' and the policy on corner-cases is rejection. Credit is due also to the [ superclass defaults proposal](http://www.haskell.org/haskellwiki/Superclass_defaults),  [ class system extension proposal](http://www.haskell.org/haskellwiki/Class_system_extension_proposal) and its ancestors, in particular, John Meacham's [ class alias](http://repetae.net/recent/out/classalias.html) proposal.


We may distinguish two uses of superclasses (not necessarily exclusive). 

- A class can *widen* its superclass, extending its interface with new functionality (e.g., adding an inverse to a monoid to obtain a group -- inversion seldom provides an implementation of composition). The subclass methods provide extra functionality, but do not induce a standard implementation of the superclass. Num provides arithmetic operations, widening Eq, but does not induce an implementation of `(==)`.
- A class can *deepen* its superclass, providing methods at least as expressive, admitting a default implementation of superclass functionality (e.g., the `compare` function from an Ord instance can be used to give an Eq instance; `traverse` from Traversable f can be instantiated to `foldMapDefault` for Foldable f and `fmapDefault` for Functor f). 


(**SLPJ** I don't understand this distinction clearly.) 


This
proposal concerns the latter phenomenon, which is currently such a
nuisance that Functor and Applicative are not superclasses of
Monad. Nobody wants to be forced to write Functor and Applicative
instances, just to access the Monad interface. Moreover, any proposal
to refine the library by splitting a type class into depth-layers is
(rightly!) greeted with howls of protest as an absence of superclass
instances gives rise to breakage of the existing codebase.


Default superclass instances are implemented in the
[ Strathclyde Haskell Enhancement](http://personal.cis.strath.ac.uk/~conor/pub/she/superclass.html). 
They should enable some tidying of
the library, with relatively few tears. Moreover, they should allow us
to deepen type class hierarchies as we learn. Retaining backward
compatibility in relative silence is the motivation for an opt-in
default.

## Design goals


The major design goal is this:

### Design goal 1: a class C can be re-factored into a class C with a superclass S, without disturbing any clients


The difficulty is that if you start with

```wiki
class C a where
   f :: ...
   g :: ...
```


and lots of clients write instances of `C` and functions that use it:

```wiki
instance C ClientType
  f = ...blah...
  g = ...blah...

foo :: C a => ...
foo = ...
```


Now you want to refactor `C` thus:

```wiki
class S a where
  f :: ...
class S a => C a where
  g :: ...
```


Design goal 1 is that this change should not force clients to change their code.  Haskell 98 does not satisfy this goal.  In Haskell 98 the client function `foo` is fine unmodified, but the instance declaration would have to be split into two.

### Design goal 2: write implementations of sub-classes that imply their superclass implementations

- Example 1: once you say

  ```wiki
  instance Ord (T a) where
    compare = ...
  ```

  then the implementation of `(==)` in `Eq` is obvious.  So, in the class decl for `Ord` we'd like to say (modulo syntax, see below)

  ```wiki
  class Eq a => Ord a where
    ...
    instance Eq a where
      (==) a b = case compare a b of { EQ -> True; _ -> False }
  ```

- Example 2: once you say `instance Monad M`, the instances for `Functor M` and `Applicative M` can be derivived from the definitions of `(>>=)` and `return`.  And similarly if you say `instance Applicative M`, the `Functor M` instance is derivable.

- Example 3: once you say `instance Traversable T` you can derive `Foldable` and `Functor`.


Obvious question: if something is both `Foldable` and `Applicative`, which `Functor` instance do you get?  Answer: the programmer must be able to control this (see "the opt-out mechanism" below).

---

## The proposal


Concretely, the proposal is as follows.

### Default superclass instances


First, we allow a class declaration to include a **default superclass instance declaration** for some, none, or all of its superclass constraints (transitively). We say that superclasses (transitively) with default implementations are **intrinsic** superclasses. Example:

```wiki
    class Functor f => Applicative f where
      return :: x -> f x
      (<*>) :: f (s -> t) -> f s -> f t

      (>>) :: f s -> f t -> f t
      fs >> ft = return (flip const) <*> fs <*> ft

      instance Functor f where
        fmap = (<*>) . return
```


Note the `instance` declaration nested inside the `class` declaration. This is the default superclass instance declaration, and `Functor` thereby becomes an intrinsic superclass of `Applicative`.  Moreover, note that the definition of `fmap` uses the `<*>` operation of `Applicative`; that is the whole point!


Here is another example:

```wiki
    class Applicative f => Monad f where
      (>>=) :: f a -> (a -> f b) -> f b
      instance Applicative f where
        ff <*> fs = ff >>= \ f -> fs >>= \ s -> return (f s)
```


Here, `Applicative` is an intrinsic superclass of `Monad`.


We might also want to give a different implementation of `fmap` for monads than the
one generated by the `Applicative` class declaration:

```wiki
    class Applicative f => Monad f where
      (>>=) :: f a -> (a -> f b) -> f b
      instance Applicative f where
        hiding instance Functor
        ff <*> fs = ff >>= \ f -> fs >>= \ s -> return (f s)
      instance Functor f where
        fmap f x = x >>= \y -> return (f y)
```


(In fact this `fmap` would, after optimisation of a particular instance and if enough inlining too place, generate the same code, but that might not always be the case.)

### Instance declarations


A default superclass instance in a class declaration for class C
has an effect on the instance declarations for C.


Specifically:

- An instance declaration 

  ```wiki
  instance Q => C ty where ...defs...
  ```

  for class C generates an extra instance 
  declaration

  ```wiki
  instance Q => Si ty where ....
  ```

  for each intrinsic superclass Si of C

- The method definitions in `...defs...` are distributed to the 
  appropriate instance declaration, according to which class
  the method belongs to.

- Any methods that are not specified explicitly are "filled in"
  from the default definition given in the default superclass instance.
  (If there is no default definition, then a warning is produced,
  and a definition that calls `error` is used instead.)


For example, assume the class declaration for `Monad` above. Then
this instance declaration:

```wiki
  instance Monad m where
    (>>=) = ...blah...
    (<*)  = ...bleh...
```


would generate an extra instance declaration for the instrinsic superclass `Applicative`,
with the methods distributed appropriately:

```wiki
  instance Monad m where
    (>>=) = ...blah...

  instance Applicative m where
    (<*) = ...bleh...  -- Programmer specified

    ff <*> fs = ff >>= \ f -> fs >>= \ s -> return (f s)
                       -- From the default superclass instance
```


We call these extra instance declarations an **intrinsic instance declaration**.
(The term "derived instance" is already taken!)


This process is recursive.  Since `Functor` is an intrinsic superclass of `Applicative`,
the intrinsic instance for `Applicative` recursively 
generates an intrinsic instance for `Functor`:

```wiki
  instance Functor m where
    fmap = (<*>) . return	-- From default superclass instance
```

## The opt-out mechanism


Just because you *can* make default instances, they are not always the instances you *want*. A key example is

```wiki
    instance Monad m => Monad (ReaderT r m) where ...
```


which would give us by default the intrinsic instance

```wiki
    instance Monad m => Applicative (ReaderT r m) where ...
```


thus preventing us adding the more general

```wiki
    instance Applicative m => Applicative (ReaderT r m) where ...
```


To inhibit the generation of an intrinsic instance declaration, one can use a
`hiding` clause in the instance declaration:

```wiki
    instance Sub x where
      ...
      hiding instance Super
```


which acts to prevent the generation of instances for Super and all of
Super's intrinsic superclasses in turn. For example:
write

```wiki
    instance Monad m => Monad (ReaderT r m) where ...
      return x = ...
      ba >>= bf = ...
      hiding instance Applicative
```


The `hiding` clause applies to all the intrinsic instances generated
from an instance declaration.  For example, we might write

```wiki
    instance Monad T where
      return x = ...
      ba >>= bf = ...
      hiding instance Functor
```


Note that `Functor` is only an indirect intrinsic superclass of `Monad`, via `Applicative`.
So the above instance would generate an intrinsic instance for `Applicative` but not for `Functor`.


See below for more about the opt-out mechanism.

### Details


Each default superclass instance declaration in a `class` declaration must be for
a distinct class.  So one of these is OK and the other is not:

```wiki
    -- This is ILLEGAL
    class (Tweedle dum, Tweedle dee) => Rum dum dee where
      instance Tweedle dum where ...
      instance Tweedle dee where ...

    -- But this is OK 
    class (Tweedle dum, Tweedle dee) => Rum dum dee where
      instance Tweedle dee where ...
```


By requiring that intrinsic superclasses be class-distinct, we ensure that the distribution of methods to spawned instances is unambiguous. 

### Flags


The declaration of a class with a default superclass instance would require a language extension flag; but the *instances* of such a class would not.  Again Design Goal 1 means that we want to impact client code as little as possible.

---

## Possible variations

### The design of the opt-out mechanism


Jón's proposal had a more subtle opt-out policy, namely that an
intrinsic superclass can be **silently pre-empted** by an instance for the
superclass from a prior or the present module. 
For example, instead of 

```wiki
  instance Functor T where ...

  instance Monad T where
    return x = ...
    ba >>= bf = ...
    hiding instance Functor
```


you would simply say

```wiki
  instance Functor T where ...

  instance Monad T where
    return x = ...
    ba >>= bf = ...
```


Of course, the instance of `Functor T` might be in a different module entirely.
Note that to declare an
instance of the subclass, one must produce an instance of the
superclass by the same module at the latest. 


This quiet exclusion
policy is not enough to handle the case of multiple candidate
intrinsic instances arising from multiple intrinsic superclasses (e.g.,
`Traversable` and `Monad` giving competing `Functor` instances), so some
explicit `hiding` form is required even under the "silent pre-emption" plan. 


The question for us, then, is what should
happen if an intrinsic superclass not explicitly hidden were to clash
with an explicit instance from the same or a prior module. We could

1. **Reject this as a duplicate instance declaration**, which indeed it is.  We acknowledge that it creates an issue with legacy code --- that is, it contradicts Design Goal 1 --- precisely because there are plenty of places where we have written the full stack of instances, often just doing the obvious default thing.

1. **Allow the explicit to supersede the intrinsic default, but issue a warning** suggesting to either remove the explicit instance or add an explicit opt-out.

1. **Allow the explicit to supersede the intrinsic default silently**. This fits with Design Goal 1, but risks perplexity: if I make use of some cool package which introduces some `Foo :: * -> *`, I might notice that `Foo` is a monad and add a `Monad Foo` instance in my own code, expecting the `Applicative Foo` instance to be generated in concert; to my horror, I find my code has subtle bugs because the package introduced a different, non-monadic, `Applicative Foo` instance which I'm accidentally using instead. 


There is considerable support in the [ email discussion thread](http://www.haskell.org/pipermail/glasgow-haskell-users/2011-August/020730.html) for Option 2 or 3, on the grounds that Option 1 contradicts Design Goal 1.  


Perhaps Option 2 is the pragmatic choice.

### Opting in rather than out


The \[ [ http://www.haskell.org/haskellwiki/Superclass_defaults](http://www.haskell.org/haskellwiki/Superclass_defaults) superclass default proposal\] deals with the question of opt-outs by intead requiring you to opt *in*.  A `Monad` instance would look like

```wiki
  instance (Functor T, Applicative T, Monad T) where
    (>>=) = ...blah...
    return = ...bleh...
```


where we explicitly ask the compiler to generate an instance of `Applicative T` and `Functor T`.   The disadvantage is that you have to know to do so, which contradicts Design Goal 1.

## Multi-headed instance declarations


While we're about it, to allow multi-headed instance declarations for class-disjoint conjunctions, with the same semantics for constraint duplication and method distribution as for the defaults, so

```wiki
    instance S => (C x, C' x) where
      methodOfC  = ...
      methodOfC' = ...
```


is short for

```wiki
    instance S => C x where
      methodOfC  = ...
    instance S => C' x where
      methodOfC' = ...
```


This proposal fits handily with the [kind Fact proposal](kind-fact), 
which allows multiple constraints to be abbreviated by
ordinary type synonyms.  So we might write

```wiki
  type Stringy x = (Read x, Show s)
  instance Stringy Int where
    read = ...
    show = ...
```


The common factor is that one instance declaration is expanded into
several with the method definitions distributed appropriately among
them.

## Feature interactions


What about interaction with other features?

- `deriving`.  If you say `deriving(Ord)` do you get the `Eq` by default?  What if you want to specify a manual instance for `Eq`?  Ditto standalone deriving. (**pigworker** We use the same logic as if the derived instances were written by hand in the same module as the datatype (or standalone declaration). If you say `deriving(Ord)`, you get the default. If you say `deriving(Ord, Eq)` under option 2, the derived `Eq` preempts the default one. But we *do* then need to be able to say `deriving (Ord hiding Eq)` to allow explicit opt-out if another default superclass offers an `Eq` which we prefer.)
- Generic default methods. (**pigworker** Default methods in default superclass instances override class default methods and are overridden by specific implementations in subclass instances. So I can define `Applicative` to have a default class method `<*>` which fails with a specific message; I can make `class Monad` give a default implementation `(<*>) = ap`; I can give a specific `instance Monad` which overrides `<*>`, perhaps for improved efficiency.)
- Associated types, and default type synonyms.  Presumably they obey the same rules as for default methods. (**pigworker** Yes. You can put any declaration into a default superclass instance, and the same sorts of definitions as you can with default methods in class declarations. Notably, `data instance`s are forbidden, because they could result in multiple declarations of the same constructor. (Must get around to an 'overloaded constructors' proposal.))

---

# Applications

*If you want superclass instances, add yourself below with a quick sketch of what your application is*


For years, I have been pretending that Monads are not Functors. When applicative functors showed up, I could accept them as functors, but my internal contradictions prevented me to accept them as monads. This is a cry for help! 

>
> -- Haskell 98
