# Roles

**Update** (March, 2014): There is a draft paper describing roles, including much discussion of design decisions, [ here](http://www.cis.upenn.edu/~eir/papers/2014/coercible/coercible-ext.pdf).


The idea of *roles* comes from the paper [ Generative Type Abstraction and Type-level Computation](http://www.seas.upenn.edu/~sweirich/papers/popl163af-weirich.pdf), published at POPL 2011. The implementation of roles in GHC, however, is somewhat different than stated in that paper. This page focuses on the user-visible features of roles. [RolesImplementation](roles-implementation) talks about the implementation in GHC. See also [ Richard's blog post about roles](http://typesandkinds.wordpress.com/2013/08/15/roles-a-new-feature-of-ghc/). (Note: some aspects of that blog post are out of date, as of December 17, 2013.)


See also this email thread: [ More GND + role inference woes](http://www.haskell.org/pipermail/ghc-devs/2013-October/003003.html).

## The problem we wish to solve


GHC has had a hole in its type system for several years, as documented in [\#1496](https://gitlab.haskell.org//ghc/ghc/issues/1496), [\#4846](https://gitlab.haskell.org//ghc/ghc/issues/4846), [\#5498](https://gitlab.haskell.org//ghc/ghc/issues/5498), and [\#7148](https://gitlab.haskell.org//ghc/ghc/issues/7148). The common cause behind all of this is the magic behind `-XGeneralizedNewtypeDeriving` (GND). Here is an example:

```wiki
newtype Age = MkAge { unAge :: Int }

type family Inspect x
type instance Inspect Age = Int
type instance Inspect Int = Bool

class BadIdea a where
  bad :: a -> Inspect a

instance BadIdea Int where
  bad = (> 0)

deriving instance BadIdea Age
```


This code is accepted by GHC 7.6.3. Yet, it goes wrong when you say `bad (MkAge 5)` -- we see the internal encoding of `Bool`! Let's trace what is happening here.


A newtype is a new algebraic datatype that wraps up exactly one field (in our example, of type `Int`). Yet, the semantics of Haskell makes a guarantee that wrapping and unwrapping a value (with `MkAge` or `unAge`) has no runtime cost. Thus, internally, we must consider `Age` to be wholly equivalent to `Int`.


The problem with this idea comes with type families. (There are other ways to tickle the bug, but one example is enough here.) A type family can branch on *Haskell* type, and of course, in Haskell (unlike in the internals of a compiler), `Age` is most certainly *not*`Int`. (If it were, newtypes would be useless for controlling instance selection, a very popular use case.) So, in our example, we see that `Inspect Age` is `Int`, but `Inspect Int` is `Bool`. Now, note the type of `bad`, the method in class `BadIdea`. When passed an `Int`, `bad` will return a `Bool`. When passed an `Age`, `bad` will return an `Int`. What happens on the last line above, when we use GND? Internally, we take the existing instance for `Int` and just transmogrify it into an instance for `Age`. But, this transformation is very dumb -- because `Age` and `Int` are the same, internally, the code for the `Age` instance and the code for the `Int` instance are the same. This means that when we call `bad (MkAge 5)`, we run `5` through the existing implementation for `bad`, which produces a `Bool`. But, of course, the type of `bad (MkAge 5)` is `Int`, and so we have effectively converted a `Bool` to an `Int`. Yuck.

## The solution


What to do? It turns out we need a subtler definition of type equality than what we have had. Specifically, we must differentiate between *nominal* equality and *representational* equality. Nominal equality (called C in the paper cited above) is the Haskell equality we all know and love. If two types have the same name, they are nominally equal. If they don't have the same name (expanding type synonyms), they are not nominally equal. Representational equality, on the other hand, shows that two types have the same *representation*. This is the equality that newtypes produce -- `Age` is representationally equal to `Int`, but they are not nominally equal.


Datatypes, classes, and type synonyms can be parametric in their type arguments or not. By "parametric", I mean that they do not *inspect* the type argument. A non-parametric type variable is inspect. Here are some examples:

```wiki
data List a = Nil | Cons a (List a)    -- parametric
data GADT a where                      -- non-parametric
  GAge :: GADT Age
  GInt :: GADT Int

class C1 a where                       -- parametric
  foo :: a -> List a

class C2 a where                       -- non-parametric
  bar :: a -> GADT a

class BadIdea a where                  -- non-parametric
  bad :: a -> Inspect a
```


In the terminology here, non-parametric types and classes care, in some fundamental way, what type parameter they are given. Parametric ones don't. We can generalize this idea a bit further to label each type variable as either parametric or not. For example,

```wiki
data Mixed a b where
  MInt :: a -> Mixed a Int
  MAge :: a -> Mixed a Age
```


is parametric in its first parameter but not its second. We say that a parametric type variable has a representational role and a non-parametric one has a nominal role.

## `Coercible`


The libraries with GHC 7.8 offer a new class

```wiki
class Coercible a b where
  coerce :: a -> b
```


The idea is that a `Coercible` instance exists allowing coercions between any two types that are representationally equal. A programmer can then use `coerce` to get between the types. The instances themselves are magically generated as necessary; it is not allowed for programmers to declare their own `Coercible` instances. So, we have `Coercible Age Int` but never `Coercible Bool Int`.


The reason we need roles is to describe how these representational equalities (or, equivalently, `Coercible` instances) "lift" through other types. For example, is `[Age]` representationally equal to `[Int]`? Sure. But, is `GADT Age` representationally equal to `GADT Int`? I hope not!


The rule is this: we have `instance Coercible a b => Coercible (T a) (T b)` if and only if the first parameter has a representational role. Thus, we have `instance Coercible a b => Coercible [a] [b]` but not `instance Coercible a b => Coercible (GADT a) (GADT b)`. This generalizes straightforwardly when there are multiple parameters, and it's worth noting that `Coercible` is always reflexive, even when nominal roles are involved.

## GeneralizedNewtypeDeriving implemented using `coerce`


Now that we have all of this `Coercible` machinery, we can define the behavior of GND in terms of it -- we simply `coerce` each method of the derived class. For example:

```wiki
newtype RestrictedIO a = MkRIO { unRIO :: IO a }
  deriving Monad
```


generates

```wiki
instance Monad RestrictedIO where
  return = coerce (return :: a -> IO a) :: forall a. a -> RestrictedIO a
  (>>=) = coerce ((>>=) :: IO a -> (a -> IO b) -> IO b) :: forall a b. RestrictedIO a -> (a -> RestrictedIO b) -> RestrictedIO b
  fail = coerce (fail :: String -> IO a) :: forall a. String -> RestrictedIO a
```


Note that each of these is just a call to `coerce` over the method in the instance for the newtype's representation type (in this case, `IO a`). All those type annotations are necessary to make sure that the type checker does the right conversion (and that scoped type variables are bound appropriately).


Putting all of this together, GND works exactly when each of the methods being derived is `Coercible` into the new type.

## Phantom parameters


It turns out that a third role is also useful (though unnecessary for type soundness): the phantom role. It is often the case that programmers use type variables simply to constrain the type checker, not to make any statement about the runtime representation of a type. For example `data Phant a = MkPhant Int`. Because `a` doesn't appear on the right-hand side, we say that `a` is at role phantom. Why is this nice? Because it allows us to say that, say, `Phant Int` and `Phant Bool` are representationally equal, because they really do have the same representation. Thus, there would be `instance Coercible (Phant a) (Phant b)` for any `a` and `b`.

## Role inference


How do we know what role a type parameter should have? We use role inference! We start with a few base facts: `(->)` has two representational parameters; `(~)` has two nominal parameters; and all type families' parameters are nominal. Then, we just propagate the information. By defaulting parameters to role phantom, any parameters unused in the right-hand side (or used only in other types in phantom positions) will be phantom. Whenever a parameter is used in a representational position (that is, used as a type argument to a constructor whose corresponding variable is at role representational), we raise its role from phantom to representational. Similarly, when a parameter is used in a nominal position, its role is upgraded to nominal. We never downgrade a role from nominal to phantom or representational, or from representational to phantom. In this way, we infer the most-general role for each parameter.


The exception to the above algorithm is for classes: all parameters for a class default to a nominal role. This is because we generally consider, say, `Ord Age` and `Ord Int` to be quite distinct, even if their representation is the same under the hood. Changing the behavior of type classes is a major use case for newtypes, and we wouldn't want to subvert that!

## Role annotations


As we have learned with type and kind inference, sometimes the programmer wants to constrain the inference process. For example, the base library contains the following definition:

```wiki
data Ptr a = Ptr Addr#
```


The idea is that `a` should really be a representational parameter, but role inference assigns it to phantom. This makes some level of sense: a pointer to an `Int` really *is* representationally the same as a pointer to a `Bool`. But, that's not at all how we want to use `Ptr`s! So, we want to be able to say

```wiki
type role Ptr representational
data Ptr a = Ptr Addr#
```


The `type role` annotation forces the parameter `a` to be at role representational, not role phantom. We, then, of course, need to *check* the user-supplied roles to make sure they don't break any promises. It would be bad if the user could make `BadIdea`'s role be representational!


If `Ptr` were to have multiple type parameter we would have used multiple `nominal`/`representational` annotations 

```wiki
type role Foo representational representational
data Foo a b = Foo Int
```


The other place where role annotations may be necessary are in .hs-boot files, where the right-hand sides of definitions can be omitted. As usual, the types/classes declared in an .hs-boot file must match up with the definitions in the .hs file, including down to the roles. The default role is representational in hs-boot files, corresponding to the common use case. Note that this **will break code**. But, the change is necessary to close the type-safety hole discussed above.


Role annotations are allowed on type variables in `data`, `newtype`, and `class`, declarations. They will not be allowed on type/data family declarations or in explicit `forall`s in function type signatures.

## Roles and `Traversable`


Though a minor issue in the overall scheme, work on Roles had led to an interesting interaction with `Traversable`, excerpted here:

```wiki
class Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
```


According to the rules for roles, the parameter `t` must be at role nominal, as it is used as a parameter to the type variable `f`. We must account for the possibility that `f` will be instantiated with a type whose last parameter is at role nominal, so we force `t` to be at role nominal as well.


This means that GND no longer works with Traversable. But, DeriveTraversable *does* still work. However, GHC previously preferred using GND over DeriveTraversable when both were available, which assumedly produced the same code. How is this all possible? If GND doesn't work anymore, is there something wrong with DeriveTraversable? The answer is that GND and DeriveTraversable make *different* instances, contrary to expectations. The reason is that DeriveTraversable has to use `fmap` to cast the result of `traverse` from the representation type back to the newtype. According to the Functor laws, `fmap`ping this cast should be a no-op (the law of interest is `fmap id == id`). But, if that law is not obeyed, `fmap`ping the cast may change the result of the `traverse`. Contrast this with a GND instance, which magically casts the result, without using `fmap`. If the Functor law is not obeyed, these two instances have different behavior.


Despite this, I believe that using GND with `Traversable` is indeed type-safe. Why? Because of the parametricity guaranteed in `Functor` and `Applicative`. The reason GND is prohibited with `Traversable` is that we are worried `f`'s last parameter will be at role nominal. While it is possible to write `Functor` and `Applicative` instances for such a type, the methods of those classes can't really use the any constructors that force the role to be nominal. For example, consider this:

```wiki
data G a where
  GInt :: a -> G Int
  Ga   :: a -> G a

instance Functor G where
  fmap f (GInt _) = error "urk"  -- no way out here
  fmap f (Ga a)   = Ga (f a)

instance Applicative G where
  pure a = Ga a
  (Ga f) <*> (Ga a) = Ga (f a)
  _ <*> _ = error "urk" -- no way out here, either
```


There's no way to usefully interact with the `GInt` constructor and get the code to type-check. Thus, I believe (but haven't yet proved), that using GND with `Traversable` is safe, because the `f` in `traverse` can't ever do bad things with its argument. If you, the reader, have more insight into this (or a counterexample!), please write!
