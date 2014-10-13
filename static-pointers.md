# Design notes for static pointers


These notes discuss the design of the language extension for static
pointers as proposed in
[ Towards Haskell in the Cloud](http://research.microsoft.com/en-us/um/people/simonpj/papers/parallel/remote.pdf) (Epstein et al, 2011) (called “static
values” there). This language extension is useful for remoting
computations to a distant machine. This wiki page motivates use cases
for a language extension and proposes a design for an implementation.
Much of the implementation is done in GHC "userland" so to speak, that
is, in the form of libraries.


The corresponding Trac ticket to track progress is [\#7015](https://gitlab.haskell.org//ghc/ghc/issues/7015).


See also [Simon PJ's long blog post](/trac/ghc/blog/simonpj/StaticPointers).

## Introduction


In distributed programming, processes on different nodes exchange data
by asynchronously sending messages to each other. It is useful to go
beyond this model, and allow processes to send other processes to
other nodes, not just first-order data. For instance, an extremely
useful feature of distributed frameworks in Hasell (e.g.
\[[ https://hackage.haskell.org/package/distributed-process](https://hackage.haskell.org/package/distributed-process)
distributed-process\], [ HdpH](https://hackage.haskell.org/package/hdph))
and other languages (Erlang, Scala), is the ability for a process on
one node to *spawn* a process on another node.


For example, consider a simple calculator-as-a-service. It is
a process living on some node B, accepting requests of some type
`ArithRequest`, allowing to express simple arithmetic expressions.
Given a request, the calculator-as-a-service must decode it, interpret
the arithmetic expression, and return the result. But ideally, one
would like a more direct way of performing computations remotely. As
a client, a process on some node A, we would like to be able to do
something like the following instead:

```wiki
client = do
    spawn nodeB $ plus 10 2
    spawn nodeB $ mult (2^10) (3^10)
    spawn nodeB $ neg 1
```


This avoids the need for effectively defining a new DSL, and avoids
the need for an interpreter for this DSL on the other end. Expressing
computations as straight Haskell expressions allows us to reuse GHC's
syntax and type checking at little cost. The above code is similar to
what one would write in a concurrent but single-node setting, using
`forkIO` instead of spawn. Except that the above snippet implies that
`spawn` is able to serialize arbitrary Haskell values (or *closures*).
This is undesirable, because in general closures might capture all
manner of system and local resources (e.g. sockets, locks, file
descriptors) that it makes no sense to send on the wire. We instead
want to limit what can be spawned in this manner to so-called *static
closures*: values expressed using only top-level identifiers,
literals, and *serializable* locally-bound variables.


With this extension, one can write:

```wiki
client = do
    spawn nodeB $ closure $ static (plus 10 2)
    spawn nodeB $ closure $ static (mult (2^10) (3^10))
    spawn nodeB $ closure $ static (neg 1)
```

## The `-XStaticPointers` language extension


The proposed `StaticPointers` language extension adds a new syntactic
construct to Haskell expressions:

```wiki
E ::= ... | static E
```

`E` is any Haskell expression, but restricted as follows: it must
contain no free variables (module-level identifiers are ok). For
technical reasons, the body `E` of a static form is further restricted
to be of unqualified type. In other words, `E` is allowed to be of
polymorphic type, but no unresolved type class or equality constraints
of any kind are allowed.


An expression of the form `static E` has type `StaticPtr T` if `E` has
type `T`. Any value of type `StaticPtr T` can be "resolved" to a value
of type `T` by the following new primitive, which can be brought into
scope by importing `GHC.StaticPtr` (so-named by symmetry with
`StablePtr`, `ForeignPtr`, etc):

```wiki
unstatic :: StaticPtr a -> a
```


This is the full extent of the impact on GHC. The above isn't
a standalone solution for remoting arbitrary computations across a set
of nodes, but the remaining support can be implemented in userland
libraries.

## Library support for static pointers


The \[[ https://hackage.haskell.org/package/distributed-process](https://hackage.haskell.org/package/distributed-process)
distributed-process package\] implements a framework for distributed
programming *à la* Erlang. Support for static closures is implemented
in a separate package called
\[[ https://hackage.haskell.org/package/distributed-process](https://hackage.haskell.org/package/distributed-process)
distributed-static package\]. We propose to patch this library in the
following way, and rename it to `distributed-closure`. Ultimately,
distributed-closure should be the one-stop shop for all distributed
frameworks that wish allow users to program with static closures.

`distributed-closure` will define the following datatype:

```wiki
data Closure a
```

`Closure` is the type of *static closures*. Morally, it contains some
pointer to a static expression, paired with an environment of only
serializable values.


Why do we need `Closure`? `Closure` is strictly more expressive than
`StaticPtr`. `StaticPtr` can only be constructed from *closed* expressions
(no free variables). `Closure` is built on top of `StaticPtr`. It allows
encoding *serializable expressions*. That is, expressions formed of
only top-level identifiers, literals, and serializable free variables.
for example, using `Closure`, one can write:

```wiki
f :: Int -> Int -> ...
f x y = ... closure (static (+)) `closureAp` closurePure x `closureAp` closurePure y ...
```


We introduce the following library functions on `Closure`:

```wiki
closure :: StaticPtr a -> Closure a
unclosure :: Closure a -> a

closurePure :: Serializable a => a -> Closure a
closureAp :: Closure (a -> b) -> Closure a -> Closure b
```


The signature of `closure` mentions `Serializable`, which is a class
defined as follows:

```wiki
data Dict c = c => Dict

class (Binary a, Typeable a) => Serializable a
  serializableDict :: forall a proxy. proxy a -> StaticPtr (Dict (Serializable a))
```


In words, a *serializable value* is a value for which we have
a `Binary` instance and a `Typeable` instance, but moreover for which
we can obtain a `StaticPtr` referencing a reification of the
`Serializable` dictionary for type `a`. (The `Dict` datatype can be
obtained from the \[[ http://hackage.haskell.org/package/constraints](http://hackage.haskell.org/package/constraints)
constraints package\] on Hackage).

## Implementation

### Implementation in GHC

TODO See [ old proposal](https://ghc.haskell.org/trac/ghc/wiki/StaticPointers/Old) and [ blog post](https://ghc.haskell.org/trac/ghc/blog/simonpj/StaticPointers) by Simon PJ.

### Implementation of `distributed-closure`


The definition of `Closure a` is as follows:

```wiki
data Closure a where
  StaticPtr :: StaticPtr a -> Closure a
  Encoded :: ByteString -> Closure ByteString
  Ap :: Closure (a -> b) -> Closure a -> Closure b
```


This definition permits an efficient implementation: there is no need
to reserialize the environment everytime one composes two `Closures`s.
The definition in the Cloud Haskell paper is as follows:

```wiki
data Closure' a where
  Closure' :: StaticPtr (ByteString -> a) -> ByteString -> Closure a
```


Note that the `Closure'` constructor can be simulated:

```wiki
Closure cf env <=> Ap (StaticPtr cf) (Encoded env)
```


One can even add the following constructor for better efficiency:

```wiki
data Closure a where
  ...
  Closure :: Closure a -> a -> Closure a
```


Any `StaticPtr` can be lifted to a `Closure`, and so can any
serializable value:

```wiki
closure :: StaticPtr a -> Closure a
closure x = StaticPtr 

closurePure :: Serializable a => a -> Closure a
closurePure x =
    StaticPtr (static decodeD) `closureAp`
    serializableDict Proxy `closureAp`
    Encoded (encode x)
  where
    decodeD :: Dict (Serializable a) -> ByteString -> a
    decodeD Dict = decode
```


Given any two `Closure`s with compatible types, they can be combined
using `closureAp`:

```wiki
closureAp :: Closure (a -> b) -> Closure a -> Closure b
closureAp = Ap
```


Closure serialization is straightforward, but closure deserialization
is tricky. See
\[[ https://ghc.haskell.org/trac/ghc/blog/simonpj/StaticPointers\#Serialisingstaticpointers](https://ghc.haskell.org/trac/ghc/blog/simonpj/StaticPointers#Serialisingstaticpointers)
this blog post section\] from Simon PJ as to why. The issue is that
when deserializing from a bytestring to target type `Closure b`, one
needs to ensure that the target type matches the type of the closure
before it was serialized, lest *bad things happen*. We need to impose
that `Typeable b` when deserializing to `Closure b`, but that doesn't
help us for all closures. Consider in particular the type of `Ap`:

```wiki
Ap :: Closure (a -> b) -> Closure a -> Closure b
```


Notice that the type `a` is not mentioned in the return type of the
constructor. We need to know `Typeable (a -> b)` and `Typeable a` in
order to recursively deserialize the subclosures, but we can't infer
either from the context `Typeable b`. The trick is to introduce
`ApDyn` and redefine `closureAp`:

```wiki
newtype DynClosure = DynClosure Dynamic

data Closure a where
  ...
  ApDyn :: DynClosure -> DynClosure -> Closure b

closureAp :: (Typeable a, Typeable b) => Closure (a -> b) -> Closure a -> Closure b
closureAp cf cx = ApDyn (DynClosure (toDynamic cf)) (DynClosure (toDynamic cx))
```

`DynClosure` is *not* a public type so we can assume whatever
invariants we like: the user can't build any values of this type
directly. One can serialize/deserialize a `DynClosure` quite easily:

```wiki
instance Binary DynClosure where
  put (DynClosure (Dynamic typerep x)) =
      -- XXX Can't use Any because no Typeable Any.
      let clos :: Closure () = unsafeCoerce x
      in encode clos
  get bs = do
    typerep <- get
    clos :: Closure () <- get
    return $ DynClosure $ Dynamic typerep x
```


From whence we can have

```wiki
instance Typeable a => Binary (Closure a) where
  put = ... -- Does not use the ambient Typeable constraint.
  get = ... -- Uses ambient Typeable constraint to check we are
            -- deserializing against the right type.
```


We only need the `Typeable` constraint when deserializing, but not
during deserialization, because the smart constructors `closurePure`,
`closureAp` etc enforce that any `Closure a` has `Typeable a` by
construction.


The occurrence of `unsafeCoerce` above is quite ok: it is only used to
recover structure from the wrapped `Dynamic`: that the type of the
object stored in the `Dynamic` is in fact always of the form \`Closure
b` for some `b`. We are allowed to pretend `b == ()\` always, just as
`Dynamic` internally pretends that its content is of type `Any`. This
structure is an invariant that we make sure to have the pubic API of
our module enforce.


All that remains is to implement `unclosure`:


unclosure :: Typeabe a =\> Closure a -\> a
unclosure (StaticPtr sptr) = unstatic sptr
unclosure (Encoded x) = x
unclosure (Ap cf cx) = (unstatic cf) (unstatic cx)
unclosure (ApDyn (DynClosure dyncf) (DynClosure dyncx)) = dynApply dyncf dyncx
unclosure (Closure cx x) = x

### About performance


We anticipate that the dynamic type checks associated with the use of
`Dynamic` may have a substantial impact on performance. Not only that,
the presence of these `Dynamic`s bloats the size of the messages that
are sent over the wire. But one nice property of this approach is that
we can always keep *both*`Ap` and `ApDyn` constructors, and define
`unsafeClosureAp` as: 

```wiki
unsafeClosureAp :: Typeable a => Closure (a -> b) -> Closure a -> Closure b
unsafeClosureAp = Ap
```

`unsafeClosureAp` is used to send composite `Closure`s over the wire
*without* dynamic type checks. This in general may allow crafting
messages that cause the remote side to segfault, but that's what the
name is all about. And the remote side is free to refuse processing
`Closure`s built with `unsafeClosureAp` if it doesn't trust the
sender.

## Conclusion


It appears possible to implement a language extension first proposed
in the original Cloud Haskell paper in a way that supports polymorphic
types - a feature that was not considered in the paper. Furthermore,
the proposal in the original Cloud Haskell paper compromised type
safety since it allowed deserializing `Closure`s at arbitrary type,
while this proposal adds extra safety yet still making it possible to
use a backdoor for performance.


What's the trusted code base (TCB) that you need to trust in order to
guarantee type safety? GHC of course, but not only. This language
extension adds one new primitive function to GHC. But one needs to
also trust `dynamic-closure`, since it uses `unsafeCoerce`. Ideally
one would only have to trust GHC and its standard libraries, and have
`dynamic-closure` be part of the standard library. But in any case
`dynamic-closure` depends on at least `binary` in order to do its
work, which it would be undesirable to pull into `base`, so is best
kept separate from GHC.
