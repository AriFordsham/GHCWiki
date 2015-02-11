## Impredicative polymorphism


GHC does not (yet) support impredicative polymorphism, but it's a topic that comes up regularly, so this page collects the state of play.

### What is impredicative polymorphism?


Consider this

```wiki
foo :: (forall a. a -> a) -> Int
bar :: forall b. Bool -> b -> b

test1 :: Bool -> Int
test1 x = foo (bar x)

test2 :: Bool -> Int
test2 = foo . bar
```


Should `test1` typecheck?  Yes: GHC can see that `foo`'s argument should have type `forall a. a->a`, and indeed `bar x` has that type.  It involves higher-rank type inference (see \[[ http://research.microsoft.com/en-us/um/people/simonpj/papers/higher-rank/index.htm](http://research.microsoft.com/en-us/um/people/simonpj/papers/higher-rank/index.htm) Practical type inference for higher rank types), but GHC has supported this for ages.


What about `test2`?  After all, it's just an eta-abstracted version of `test1`.  No, `test2` is rejected.  Remember the type of `(.)`:

```wiki
(.) :: forall p q r. (q -> r) -> (p -> q) -> p -> r
```


To make this work in `test2` we must instantiate `q := forall a. a->a`, to make the type of `(.)`'s first argument match `foo`'s type.  **So we have to instantiate a polymorphic type variable `q` with a polymorphic type**.  This is called *impredicative polymorphism*, and GHC's type inference engine simply does not support it.

### Special case for `($)`


Consider

```wiki
runST :: (forall s. ST s a) -> a

foo = runST $ do { ...blah... }
```


Here again we need impredicative polymorphism, to instantiate `($)`'s type, very much like `(.)` above. So `foo` would be rejected.  But Haskell programmers use `($)` so much, to avoid writing parentheses, that GHC's type inference has an ad-hoc special case for `($)` that allows it to do type inference for `(e1 $ e2)`, even when impredicative polymorphism is needed.

### The internal language


All of this concerns the *source* language.  GHC's *internal language*, System FC, is fully impredicative.  This works find becuase there is no type inference in System FC.  It's only type *inference* that is the problem with impredicativity.

### What about `-XImpredicativeTypes`?


We've made various attempts to support impredicativity, so there is a flag `-XImpredicativeTypes`.  But it doesn't work, and is absolutely unsupported.  If you use it, you are on your own; I make no promises about what will happen.

### Reading


Here are some useful papers about type inference for impredicative polymorphism;
  

- [ FPH : First-class Polymorphism for Haskell (2008)](http://research.microsoft.com/en-us/um/people/simonpj/papers/boxy/)
- [ Boxy types: type inference for higher rank and impredicativity (2006)](http://research.microsoft.com/en-us/um/people/simonpj/papers/boxy/)  We implemented this in GHC, but it was Just Too Complicated.
- [ QML: Explicit first-class polymorphism for ML (2009)](http://research.microsoft.com/en-us/um/people/crusso/qml/) A much simpler, and less ambitious, approach.
- [ MLF: Raising ML to the power of System F (2003)](http://gallium.inria.fr/~remy/publications.html) The other end of the spectrum from QML: a very sophisticated approach.

### Tickets


There are lots of tickets in GHC's Trac that boil down to impredicativity.  Here is a non-exhaustive list (please add to it):

- [\#4295](https://gitlab.haskell.org//ghc/ghc/issues/4295)