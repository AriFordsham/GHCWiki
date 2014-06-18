# Kind inference for types and classes


This page summarises several alternative designs, which are debated on [\#9200](https://gitlab.haskell.org//ghc/ghc/issues/9200).  (See also [\#9201](https://gitlab.haskell.org//ghc/ghc/issues/9201).)

## Baseline strategy (BASELINE)


This plan, originally due to Mark Jones, is the strategy that GHC 7.8 follows for ordinary, recursive term-level functions, and for recursive data types.  I'll describe it for data types, with this example:

```wiki
   data SS f a b = MkSS (TT a f) (SS f a b)
   data TT (a::k) (f::k -> *) :: * where
      MkTT :: f a -> SS f a Maybe -> SS f a Int -> TT a f
```

1. Identify which type constructors have Complete User Type Signatures (CUSK).  In this example, `TT` does. Extend the environment with these, fixed, kinds:

  ```wiki
         TT :: forall k. k -> (k->*) -> *
  ```
1. Perform strongly-connected component (SCC) analysis on the non-CUSK decls, *ignoring* dependencies on a type constructor with a CUSK.  In our example, we get a single recursive SCC, containing `SS`.

1. For each SCC in turn:

  - Bind the type constructor to a fresh meta-kind variable:

    ```wiki
            SS :: kappa0
    ```
  - Kind-check all the declarations of the SCC in this environment.  This will generate some unifications, so in the end we get

    ```wiki
            kappa0 ~ (kappa1 -> *) -> kappa1 -> kappa2 -> *
    ```

    The `kappa1` arises from instantiating `TT` at its call site in `SS`
  - Generalise.  So we get

    ```wiki
            SS :: forall k1 k2. (k1->*) -> k1 -> k2 -> *
    ```
1. Extend the environment with these generalised kind bindings, and kind-check the CUSK declarations.


The Key Point is that we can kind-check `SS`*without looking at `TT`'s definition at all*, because we completely know `TT`'s kind.  That in turn means that we can exploit *inferred* polymorphism for `SS` when kind-checking `TT`.  As we do here: `TT` uses `SS` in two different ways `(SS f a Maybe)` and `(SS f a Int)`.


Note that for a *non-recursive* type or class declaration, (BASELINE) always works fine.

## Partial kind signature strategy (PARTIAL)


The key idea is that *all polymorphism is declared*, so nothing gets to be kind-polymorphic unless you say so.  But the payoff is that you can give partial kind signatures.  Here's the strategy.

1. Sort the declarations into SCCs.  No special treatment for CUSKs.

1. For each declaration, extend the environment with a kind binding that has a forall for each *explicit* user-written kind variable, but meta-kind variables otherwise.  These kind annotations amount to partial kind signatures.  For example

  ```wiki
        data Foo (a :: k1 -> k1) b c = ...
  ```

  would get a kind binding

  ```wiki
        Foo :: forall k1. (k1->k1) -> kappa1 -> kappa2 -> *
  ```

  Our earlier example would give

  ```wiki
        T :: forall k. k -> (k->*) -> *
        S :: kappa3 -> kappa4 -> kappa5 -> *
  ```

1. Kind-check the declartions in this environment.  At a call of `Foo`, say, we'd instantiate the `forall k1` with a fresh meta-kind variable, but would share `kappa1`, `kappa2` among all calls to `Foo`.

1. Default any unconstrained meta kind variables to `*`


That's it!   No generalisation step.  The *only* polymorphism is that declared by the user.


So our earlier `SS`/`TT` example would be rejected because it relies on S being polymorphic in its third parameter. If you want the `SS`/`TT` example to work you could write

```wiki
   data SS (f::k1->*) (a::k1) (b::k2) = MkSS (TT a f) (SS f a b)
   data TT (a::k) (f::k->*) where
      MkTT :: f a -> SS f a Maybe -> SS f a Int -> TT a f
```

### Declarative typing rules for (PARTIAL)


I think that (PARTIAL) has a nice declarative typing rule.


Here is what the conventional declarative typing rule, *in the absence of polymorphism* for a single self-recursive function looks like.  (I'm using the term language but the same thing happens at the type level.)

```wiki
        G, f:t |- e:t
        G, f:t |- b:t'
      ---------------------------
        G |- letrec f = e in b : t'
```


Here the "t" is a monotype (no foralls) that the declarative typing rules clairvoyantly conjures up out of thin air.


Once you add Hindley-Milner style polymorphism, the rule gets a bit more complicated

```wiki
        G, f:t |- e:t
        G, f:gen(G,t) |- b:t'
      ---------------------------
        G |- letrec f = e in b : t'
```


where 'gen' is generalising.


The (PARTIAL) rule might look like this:

```wiki
        t = forall vs. sig[t1..tn/_]
        vs \not\in ti
        G, f : t |- e : forall vs.t
        G, f : t |- b:t'
      --------------------------- (T-PARTIAL)
        G |- letrec f :: forall vs. sig; f = e in b : t'
```


Here I'm expressing the user-specified knowledge as a signature `forall vs.sig`, with '_' for bits you don't want to specify.

```wiki
       f :: forall a. _ -> a -> _
```


Then the rule intantiates each '_' independently with 
a clairvoyantly guessed monotype (provided it does not mention
the 'vs', or 'a' in this example), and off you go.

### A tricky point about (PARTIAL)


Notice that in this typing rule I say `vs \not\in ti`.  If you don't have that side condition I think
complete inference becomes very hard.  Suppose `MT :: (*->*) -> *`, and consider

```wiki
   data Foo f (a::k) = MkFoo (Foo Maybe Int) (Foo MT Maybe)
```


Because of the partial kind signature we'll kind-check `Foo`'s RHS with this kind signature for `Foo`:

```wiki
   Foo :: forall k. kappa1 -> k -> *
```


using the unification variable `kapp1` for `f`.  Now, if we clairvoyantly decide `kappa1 := k->*`, as would be allowed by (T-PARTIAL), then indeed the definition if well-kinded.  So we'd better infer that, if we are to be complete wrt (T-PARTIAL).  But the algorithm will share `kappa1` among both calls to `Foo`, and will therefore unify `Maybe` with `MT` and fail.


To gain completeness we need to be less ambitious; hence the side condition in (T-PARTIAL) `vs \not\in ti`.


But that side condition, in turn, means that this will fail:

```wiki
  data Foo f (a::k) = MkFoo (f a) (Foo f a)
```


because here `kappa1` must be unified with `k->*`, which isn't allowed by (T-PARTIAL).
Maybe that is acceptable; you can always decorate both of `Foo`'s arguments.

## Generalised partial kind signature strategy (PARGEN)


The (PARGEN) strategy is exactly like (PARTIAL) except that step 4 is different:

1. Generalise over any unconstrained meta kind variable, rather than defaulting to `*`.  Since we are operating at top level, there are no kind variables mentioned in the environment, so no need for the ususal "not free in the environment" check.


So we use the partial kind signatures to express any polymorphism necessary for recursion *inside* the SCC,
but perhaps infer yet more polymorphism that can be used *after* the SCC.  Thus:

```wiki
data T1 f a = MkT1 (f a) (T f a)
  -- Success:  T1 :: forall k. (k->*) -> k -> *

data T1a f (a::k) = MkT1a (f a) (T f a)
  -- Failure:  f's kind is unified with skolem k
  -- See "tricky point" above

data T2 f a = MkT2 (f a) (T2 Maybe Int) (T2 Monad Maybe)
  -- Failure: needs polymorphic recursion

data T3 (f::k->*) (a::k) = MkT3 (f a) (T3 Maybe Int) (T3 Monad Maybe)
  -- Success: polymorphic recursion declared

data T4 (f::k->*) a = MkT4 (f a) (T4 Maybe Int) (T4 Monad Maybe)
  -- Failure: not all polymorphism in k is declared
  -- See "tricky point" above
```

### Declarative typing rules for (PARGEN)


The declarative rule for (PARGEN) is a combination of the one for (PARTIAL) with 
the standard generalisation:

```wiki
        t = forall vs. sig[t1..tn/_]
        vs \not\in ti
        G, f : t |- e : forall vs.t
        G, f : gen(G,t) |- b:t'
      --------------------------- (T-PARGEN)
        G |- letrec f :: forall vs. sig; f = e in b : t'
```


The difference from (PARTIAL) is that before type-checking `b` we generalise `t`.

### How does (PARGEN) differ from (BASELINE)?


(PARGEN) and (BASELINE) are incomparable.

- The `SS/TT` example under (BASELINE) will be rejected by (PARGEN) becuase `SS` will get kind `kappa1 -> kappa2 -> kappa3 -> *` when kind-checking the `SS/TT` strongly connected component.  But (BASELINE) accepts it by breaking the SCC into two.
- There are obviously examples that are accepted by (PARGEN) but not (BASELINE).


So moving from (BASELINE) to (PARGEN) would be a breaking change, but only in rather obscure circumstances.  I am intensely relaxed about that particular backward-compatibility problem!

## All of the above (ALL)


Combine (BASELINE), for the CUSK stuff, with (PARGEN) for type with partial kind signatures.  This would type the
most programs, but is the most complicated.

## Type signatures


Another place that we currently (i.e. using (BASELINE)) do kind generalisation is in *type signatures*. If you write

```wiki
f :: m a -> m a 
f = ...
```


then the type signature is kind-generalised thus:

```wiki
This user-written signature 
  f :: m a -> m a 
means this (BASELINE)
  f :: forall k (a:k) (m:k->*). m a -> m a
```


And f's RHS had better *be* that polymorphic.  


However (PARTIAL) does no kind generalisation, and it would be consistent to cease doing so for type signatures too.  so:

```wiki
This user-written signature 
  f :: m a -> m a 
means this (PARTIAL)
  f :: forall (a:*) (m:k->*). m a -> m a
```


If you want the kind-polymorphic one, you'd have to write thus

```wiki
This user-written signature 
  f :: forall k (a:k) (m:k->*). m a -> m a
means this (PARTIAL)
  f :: forall k (a:k) (m:k->*). m a -> m a
```

## Reflection


I think we could reasonably switch to (PARTIAL) throughout.


As Richard's comments in `TcHsType` point out, we don't want maximal polymorphism.  His example is:

```wiki
    type family F a where
      F Int = Bool
      F Bool = Char
```


We could generate  

```wiki
   F :: forall k1 k2. k1 -> k2
```


so that `(F Maybe)` is well-kinded, but stuck. But that's probably not what we want. It would be better to get `F :: * -> *`


But what about

```wiki
    type family G a f b where
      G Int  f b = f b
      G Bool f b = Char -> f b
```


You could just about argue that the programmer intends

```wiki
   F :: forall k. * -> (k->*) -> k -> *
```


It's quite similar to this:

```wiki
  data PT f a = MkPT (f a)
```


which today, using (BASELINE), we infer to have kind

```wiki
  PT :: forall k. (k->*) -> k -> *
```


But I'd be perfectly happy if PT got a *monomorphic* inferred kind,
which is what (PARTIAL) would do:

```wiki
  PT :: (*->*) -> * -> *
```


If you want the poly-kinded PT, use a signature:

```wiki
  -- Any of these would do
  data PT f             (a :: k) = MkPT (f a)
  data PT (f :: k -> *) a        = MkPT (f a)
  data PT (f :: k -> *) (a :: k) = MkPT (f a)
```


One oddity is that we'd do (BASELINE) for terms and (PARTIAL) for types.  But perhaps that's ok.  They are different.

- Terms ought to be as polymorphic as possible but arguably not types. Examples above.  Also, since kind polymorphism is still in its infancy, maybe it's no bad thing if all kind polymorphism is explicitly signalled every time a kind-polymorphic binder is introduced.

- Terms have well-established separate type signatures, but we don't have a syntax for separate kind signatures of types and classes.


If we moved from (BASELINE) to (PARTIAL), some programs that work now would fail:

- the original S/T example above
- a data type like `PT` where the user did actually want the kind-polymorphic version.


But that might be a price worth paying for the simplicity, uniformity, and predictability you'd get in exchange.

**Richard:** I think changing to (PARTIAL) throughout would be a mistake, as lots of code would fail to compile. Kind polymorphism by default in datatypes and classes has been around since 7.4, and I suspect there is quite a bit of code that such a change would disrupt.


On the other hand, I think changing to (PARGEN) throughout would work nicely. I believe that it would allow all current code to type-check (except for the weird example that probably should be rejected in [\#9201](https://gitlab.haskell.org//ghc/ghc/issues/9201)). If we were to choose (PARGEN) over (ALL), it's possible that some code would become *more* polymorphic, as (PARGEN) is more polymorphic than (BASELINE) in the presence of a CUSK. However, I don't believe that this could be a *breaking* change, and I would prefer going with (PARGEN) over (ALL) for the sake of simplicity -- no need to have two systems around.


I can't figure out a way that (BASELINE) and (PARGEN) are different in type signatures for terms. This version doesn't have quite as nice a declarative typing rule because the type is generalized over kind variables that go completely unmentioned in the type -- a straightforward `forall ftv(t). t` doesn't quite do it. We need to generalize over seen variables, infer kinds, and then generalize over meta-kind variables. But, this is what is done today.


(Because open type families do not have a body, they *would* still be considered to have a CUSK, where un-annotated type variables default to have kind `*`.)


In [comment:5:ticket:9200](https://gitlab.haskell.org//ghc/ghc/issues/9200), I discuss "good" polymorphism and "bad" polymorphism. This discussion, in retrospect, seems tangential at this point. It really only makes sense when discussing closed type families, which aren't at the heart of the problems here. **End Richard**