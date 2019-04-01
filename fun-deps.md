# Functional Dependencies


This is the root page for material about functional dependencies in GHC.

## Tickets


See the ~FunctionalDependencies label.

## Consistency of Functional Dependencies


(This section was written by Iavor; I am unsure of its current status. SLPJ Apr 17.)


The functional dependencies on a class restrict the instances that may
be declared for a given class.  The instances of a class:

```haskell
    class C a b c | b -> c where f :: ...
```


are consistent with its functional dependency if the following invariant holds:

```wiki
    byFD(C,1): forall a1 a2 b c1 c2. (C a1 b c1, C a2 b c2) => (c1 ~ c2)
```


Please note that the question of FD-consistency is orthogonal to
instance coherence (i.e, uniqueness of evidence, overlapping instances,
etc.), and the decidability of type-checking of terms---for examples
of their independence, please see the `Examples` at the end of the document.


If we check that all instances in scope are consistent with their FDs,
then we can use the FD invariant `byFD` during type inference to
infer more precise types, report errors involving unsolvable contexts,
or accept programs that would be rejected without the invariant.


For example, if we have an instance:

```wiki
  I: instance F Int Int Char
```


and we have a constraint `C: F Int Int a`, then we can use the
FD-invariant to prove that `a` must be `Char`:

```wiki
    byFD(C,1) (C,I) :: a ~ Char
```

### Checking FD-Consistency


To ensure FD-consistency, before accepting an instance we need to check
that it is compatible with all other instances that are already in
scope.  Note that we also need to perform the same check when combining
imported instances.  Consider adding a new instance to an FD-consistent
set of instances:

```wiki
    I: instance P(as,bs) => C t1(as,bs) t2(as) t3(as,bs)
```


The notation `t(as)` states the variables in `t` are a subset of `as`.

1. Check that `I` is self-consistent (i.e., we can't use different
  instantiations of `I` to violate FD-consistency).  Self consistency
  follows if we can prove the following theorem:

  ```wiki
           forall as bs cs. (P, P[cs/bs]) => t3[cs/bs] ~ t3[cs/bs]
  ```
1. Check that `I` is FD-consistent with all existing instances of the class.
  So, for each existing instance, `J`:

  ```wiki
           J: instance Q(xs) => C s1(xs) s2(xs) s3(xs)
  ```

  we need to show that:

  ```wiki
           forall as bs xs. (P,Q,s2 ~ t2) => (s3 ~ t3)
  ```

  Assuming no type-functions in instance heads, the equality
  assumption is equivalent to stating that `s2` and `t2` may be
  unified, so another way to state our goal is:

  ```wiki
           forall as bs xs. (P[su], Q[su]) => (s3[su] ~ t3[su])
  ```

  where `su` is the most general unifier of `s2` and `t2`.


Proving these two goals before accepting an instance is similar to
the process of finding evidence for super-classes before accepting
and instance.  Also, note that while proving (2), it is not a problem
if we find that we have assumed a contradiction:  this simply means
that the two instances can never be used at the same time, so
the FD-consistency follows trivially.


### Examples


- FD-consistency is orthogonal to instance coherence.

>
> >
> >
> > FD-consistent but not coherent:
> >
> >
> > ```wiki
> >          instance C Int Int Int where f = definition_1
> >          instance C Int Int Int where f = definition_2
> > ```
> >
> >
> > Coherent but not FD-consistent:
> >
> >
> > ```wiki
> >          instance C Int  Int Char where ...
> >          instance C Char Int Bool where ...
> > ```
>
>

- FD-consistency is orthogonal to termination of instances.

>
> >
> > >
> > >
> > > FD-consistent but "non-terminating":
> > >
> > >
> > > ```wiki
> > >          instance C a b c => C a b c
> > > ```
> > >
> > >
> > > Terminating but not FD-consistent:
> > >
> > >
> > > ```wiki
> > >          instance C Int  Int Char where ...
> > >          instance C Char Int Bool where ...
> > > ```
> >
> >
>

