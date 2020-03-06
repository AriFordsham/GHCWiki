# Adding linear types to Haskell


This page contains information on the Linear Type Proposal.

## Discussion


The Linear Type Proposal is being discussed [on the ghc proposal repository](https://github.com/ghc-proposals/ghc-proposals/pull/111). It had previously received [ a first round of review](https://github.com/ghc-proposals/ghc-proposals/pull/91).

## Source material


The motivations, technical details and examples on the proposal are described in the article [Linear Haskell: practical linearity in a higher-order polymorphic language](https://arxiv.org/abs/1710.09756) published at POPL 2018.


Description of the implementation strategy: [LinearTypes/Implementation](linear-types/implementation)


Some motivating examples in the wiki: [LinearTypes/Examples](linear-types/examples)


Talks:

- [Arnaud Spiwack at Haskell Exchange 2017](https://skillsmatter.com/skillscasts/10637-distributed-programming-with-linear-types), speaking about some of the goals and vision for Linear Types in Haskell
- [Arnaud Spiwack at Popl 2018](https://www.youtube.com/watch?v=o0z-qlb5xbI), laying out the theory of Linear Haskell
- [Simon Peyton Jones at Curry On 2018](https://www.youtube.com/watch?v=t0mhvd3-60Y), longer but covers more ground.
- [Simon Peyton Jones at Haskell Exchange 2018](https://skillsmatter.com/skillscasts/11067-keynote-linear-haskell-practical-linearity-in-a-higher-order-polymorphic-language), similar to the Curry On talk, but Arnaud speaks inaudibly in the background from time to time

## Implementation


The implementation in progress can be found at [https://github.com/tweag/ghc/tree/linear-types](https://github.com/tweag/ghc/tree/linear-types) .


A library to serve as a central library for linearly typed program can be found at [https://github.com/tweag/linear-base/](https://github.com/tweag/linear-base/)

## Faq

# Does this proposal improve performance


No.


This proposal focuses on one thing and one thing only: letting users ascribe more precise types to functions. More precise types can mean more safety, which in turn means things that were dangerous to do before can now be viable (such as optimizations to your code). Three examples:

- Allocating long-lived objects in the C heap instead of the Haskell heap. This can ease pressure on the garbage collector (GC) and therefore improve tail latencies and potentially throughput as well. Difficult to guard against use-after-free or double-free errors without linear types.
- Zero-copy serialization. This is the largest example in the [POPL'18](https://arxiv.org/abs/1710.09756) paper and is the topic of a [ blog post](http://www.tweag.io/posts/2017-08-24-linear-types-packed-data.html) as well. The idea is to skip deserialization altogether when receiving structured message types. This is very error prone in general, but the paper explains how this can be done safely with linear types.
- Safe mutable arrays. Mutable arrays are typically implemented in the `ST` monad. But using a monad for locally impure code over-sequentializes computations, so opportunities for parallelization are lost. Linear types let us recover the lost parallelism, allow local mutation and still guarantee that referential transparency won't be broken.

# Does this proposal change the RTS?


No.


This proposal only affects the syntax of types, the type checker and GHC's Core language.

# Why does this proposal need to change Core?


There are two reasons:

- In GHC, the type `Type` is shared between the surface language and Core. So modifying the types implies that Core is modified as well.
- More importantly, because Core is a good check that our implementation works as intended. That is, a linearly typed Core will ensure that linearly-typed programs are indeed desugared to linearly-typed programs in Core. And that optimisations do not destroy linearity.

# Isn't a linear function that diverges unsound?



Our proposed type system allows you to give a linear type to list concatenation:


```
(++) :: [a] ->. [a] ->. [a]
```


So we can write


```
strange :: Int ->. [Int]
strange x = (repeat 1) ++ [x]
```


This sounds like it's breaking the promise made by `strange` that it will consume its argument exactly once. Since `x` will *never* be consumed!


But there is no cause for panic: `strange` only promises to consume `x` if its result is consumed. Which will never happen since the result of `strange` is infinite. The properties of linear types which we need are not compromised by non-termination.

# In the motivations of the proposal, there is a linear IO monad. Isn't linear IO unsound in presence of exception?



It is not, but as always, we need to careful about how we type primitives of the `IO` monad. For example, [catch](https://github.com/tweag/linear-base/blob/007b884ebb0e3182ea73e450683f9660b7a92f40/src/System/IO/Linear.hs#L144-L146) should not have a linear type. It should be typed as follows:


```
catch
  :: Exception e
  => IO (Unrestricted a) -> (e -> IO (Unrestricted a)) -> IO (Unrestricted a)
```


This means that neither the body nor the handler of `catch` can capture linear variables. Otherwise, you could lose track of them.



If, say, the body of `catch` was linear, we could write:


```
oops :: a ->. IO (Unrestricted ())
oops x =
  catch
    (throwIO "This is bad" >> oops a)
    (\_ -> return $ Unrestricted ())
```

See also [this blog post](https://www.tweag.io/posts/2020-02-19-linear-type-exception.html) which elaborates on the logical foundations of a linear type system with exceptions.

# Don't linear guarantees degrade to affine in the presence of exceptions?


When an exception is raised during the consumption of `f u`, `u` may not have been fully consumed. Functions whose argument is consumed *at most* once when their result is consumed exactly once are called affine.


Linear functions do have the property that: _if their result is consumed at most once, then their argument is consumed at most once_. This is a more relevant phenomenon in case exceptions are raised: exceptions interrupt the consumption of the result, which is only partial.



In fact, linear functions have the property that if their result is consumed with multiplicity X, then their argument is also consumed with multiplicity X. Whatever we choose X to mean. Which can be internalised in the language as follows


```
data Mult (p :: Multiplicity) (a :: *) where
  Mult :: a :p-> Multp p a

multMap :: (a ->. b) -> Mult p a -> Mult p b
multMap f (Mult x) = Mult (f x)
```

# Wouldn't it be just as good to have affine types, since they are simpler?



Having affine types would makes some things easier. For instance `catch` is allowed to capture affine variables:


```
-- Writing A for the multiplicity of affine functions
catch
  :: Exception e
  => IO (Unrestricted a) :'A-> (e -> IO (Unrestricted a)) :'A-> IO (Unrestricted a)
```


However, the Rust programming language, with its (essentially) affine language has shunned exceptions for a reason: they are still quite a complication.


Affine types are sufficient for abstractions based on ownership, as long as you don't care about prompt deallocation (mutable arrays on the GC heap are a good example where you don't).


To recover prompt deallocation, Rust relies on a bespoke mechanism (lifetime analysis) and code generation to an essentially linear language. This is something which is not reasonable to hope for in Haskell at the moment.


Use-cases which do not rely on uniqueness, such as Samuel Gelinaux's [3D-printable model example](https://www.spiria.com/en/blog/desktop-software/making-non-manifold-models-unrepresentable) (which he also implemented with our prototype [ here](https://github.com/gelisam/linear-examples)) may not accommodate affinity so well.


Offering affine types in addition to linear types is possible. In fact no choice that we make now compromises the ability to add them later, because our linear types system was specifically designed to allow all manner of other multiplicities as extensions. So we propose to keep an already large proposal as small as possible, and postpone adding affine types to a future proposal, if and when the need arises.

# Do linear types guarantee resource safety?


No. Linear types only gives a type to functions that consume their argument exactly once when their result is consumed exactly once.


Having linear types is powerful, and makes it possible to write *resource-safe abstractions*. But this is not an intrinsic quality of the type system.

# Will base be modified to use linear types?


Not at first. When it happens it will be as a separate proposal to the Core library Committee (CLC), if at all.


Before we are ready to make a proposal, users should put linear types to work and share idioms via a [linearly typed library](https://github.com/tweag/linear-base/), uploaded to Hackage, and driven by the ecosystem. Anyone could, in a future proposal to the CLC, propose for some of these idioms to be standardized in base.

# Will adding linear types fragment the libraries ecosystem?


The centrepiece of our design is to avoid code duplication. Crucially, the same types can be used in linear and non-linear contexts. For example, the [linear-base](https://github.com/tweag/linear-base/) library uses the same types as `base`. So libraries developed with `linear-base` will be compatible with libraries developed with `base`.

# Is this type for monads in the paper correct?



The linear types proposal features monads with the following interface


```
return :: a ->. m a
(>>=) :: m a ->. (a ->. m b) ->. m b
```


But unfolding the definitions of the Kleisli extension and unit of a monad you would get instead


```
return :: a -> m a
extend :: (a ->. m b) -> (m a ->. m b)
```


Which is right? The latter type is actually not very useful, but it would feel uncomfortable if the former type was not backed by well-known mathematics.


Fortunately, it is: it is the type of an enriched monad over the self-enrichment of the base category. Briefly:

- An [enriched category](https://en.wikipedia.org/wiki/Enriched_category) is a category whose hom-sets are taken to be objects in another category (which must be monoidal).
- Any closed symmetric monoidal category is enriched over itself.
- The category of Haskell types and linear functions is closed symmetric monoidal (with the usual [provisos](http://math.andrej.com/2016/08/06/hask-is-not-a-category/)). Let's call it LHask.
- Monads of LHask for which the unit and join are maps in LHask are called enriched monads.


This is related to the often overlooked fact that monads must be [strong](https://en.wikipedia.org/wiki/Strong_monad), both in Moggi's theory of effects, and as a programming language construct. It is easy to overlook because (self-)enriched monads are necessarily strong. And all monads in Hask are, naturally, self-enriched. See [ this discussion](https://ncatlab.org/nlab/show/tensorial+strength) for more details.
