
This page is to document various decisions / important emails / conversations that have come up in the course of implementing [DependentHaskell](dependent-haskell). There is no effort at making the material here understandable to those not in the conversations. (Some of these conversations are with myself \[= Richard Eisenberg\], making the situation even worse.) There is also no concerted effort at a logical organization of this page. 

## Roles, Coherence, and Kinds

### Email sent from RAE to SCW on 7/14/14


I've hit an unfortunate conflict between roles and "nokinds" and want some advice on how to merge them. I have a potential answer, but it involves introduce a second, symmetric form of coherence (but with different roles), and that feels wrong.


The problem all starts with casts:

```wiki
t : k1
g : k1 ~ k2
---------- CastTy_NoRoles
t |> g : k2
```


Uniformity with the term level suggests the following:

```wiki
t : k1
g : k1 ~R k2
------------- CastTy
t |> g : k2
```


Notice that g is representational.


Now, we must consider how coherence should work. Here is an attempt:

```wiki
g : t1 ~r t2
t1 |> h : k          -- that is, (t1 |> h) is well-formed
-------- Coherence1
g |> h : (t1 |> h) ~r t2
```


Note that `r` is a role metavariable; it is NOT (necessarily) "representational". The role of (g \|\> h) is the same as the role of g. Now, suppose we have (axAge : Age \~R Nat). Then, (\<Zero\> \|\> axAge) : (Zero \|\> axAge) \~N Zero. In other words, (Zero \|\> axAge) and Zero are nominally equal. This is disconcerting, though, because nominal equality should be the equality that the type-checker infers... and yet, casting by axAge is not inferrable; the user must put in a `coerce`. So, rule Coherence1 seems wrong.

```wiki
g : t1 ~r t2
t1 |> h : k
------------- Coherence2
g |> h : (t1 |> h) ~R t2
```


The Coherence2 rule allows g to have any role, but it restricts (g \|\> h) to be representational. Coherence2 would mean that nominal equality is not coherent! This seems troubling, too. (It would also break the lifting lemma, which crucially relies on proper coherence of all three equalities.)


My current adopted rules are these:

```wiki
g : t1 ~N t2
t1 |> (sub h) : k
--------------------- Coherence_Nom
g |> h : (t1 |> sub h) ~N t2

g : t1 ~R t2
t1 |> h : k
------------------- Coherence_Rep
g |> h : (t1 |> h) ~R t2
```


These rules require that g and h have the same role, which is also the role of (g \|\> h). The problem here is that the lifting lemma breaks. Recall that the lifting lemma works with a substitution S from type variables to coercions. (It's slightly more involved, but this is a convenient simplification.) Let SL be the type-variable-to-type substitution that maps the domain of S to the left-hand types of the range of S. Define SR similarly, on the right. Then, then lifting lemma states that S(t) : SL(t) \~ SR(t). In the presence of roles, the lifting operation must also take an input role, making the lemma look like: S(t)_r : SL(t) \~r SR(t). See pages 19-20 of the extended version of the Coercible paper for the details.


We must choose an implementation for S(t \|\> h)_r. We know, by induction, that S(t)_r : SL(t) \~r SR(t). We need a coercion of type (SL(t) \|\> SL(h)) \~r (SR(t) \|\> SR(h)). In the "nokinds" paper, we achieved this by using coherence on S(t) -- no problem. But, with the last two rules I wrote above, this doesn't work! `h` has the wrong role to use in a coherence, if the desired role is nominal, because `h` is representational, as we can see by its use in the cast (t \|\> h). We're stuck.


The solution I have in mind is represented by this rule:

```wiki
g : t1 ~N t2
h : k1 ~N k2
t1 |> h1 : k1
t2 |> h2 : k2
------------ NomCoherence
g |>>_h (h1, h2) : (t1 |> h1) ~N (t2 |> h2)
```


Here, I've defined a new form. This is quite like a symmetric form of Coherence1, but it requires the target kinds to be \*nominally\* equal, which is a stronger requirement. This form would work in the lifting lemma fine: S(t \|\> h)_N = S(t)_N \|\>\>_(S(k)_N) (SL(h), SR(h)), where k is the kind of (t \|\> h). Note that we still need the asymmetric coherence to deal with lifting context extension for existential variables (Def'n 5.6 of p. 9 of the "nokinds" paper).


I don't see any real problems with introducing the new form, except for the extra weight it causes. And, having two very similar, but subtly different coercion forms just smells like there is a better design out there.

### Result of ensuing discussion


Go with Coherence1. No actual problems are caused by it, and we don't have a semantics of nominal equality that we're aiming for, so there's nothing terribly wrong here.

### Conversation on 11/7/14


SCW said that Coherence_Nom + Coherence_Rep + NomCoherence was better. NomCoherence is actually more like the other congruence forms that we have in coercions. The only reason it's missing from the "nokinds" paper is that we can derive it from the other coherence rule. And, if we did have a semantics for nominal equality, Coherence1 would be questionable.


RAE's decision after conversation: Continuing with Coherence1, noting SCW's disagreement. Reasons for this decision:

1. The decision to go with Coherence1 was a result of another (less well-documented) conversation with SCW. Thus, there's not a clear, unchanging stance of the person with more experience.
1. The cost of changing from Coherence1 to the 3 other rules is not likely to change if I continue more work based on Coherence1.
1. There's a good chance that only one of these two possibilities is "Right". We'll know more later.
1. I'm eager to make forward progress and not spin around this wheel again.

## Lifted vs. Unlifted equality


GHC happily keeps a distinction between lifted and unlifted equality, but this is causing me grief. The primary cause of the grief is that I need to cast types by unlifted equality (casting by lifted equality is bogus because the evidence might be bottom), and there is no type-level construct to unpack a lifted equality. Here is the canonical example:

```wiki
data SameKind (k :: *) (a :: k) (b :: k)
challenge :: forall (k1 :: *) (k2 :: *) (a :: k1) (b :: k2). (k1 ~ k2) => SameKind k1 a b
challenge = ...
```


During translation to FC, we have to give a name to `k1 ~ k2` (easy) but then somehow use it to cast `b` to have kind `k1`.

### Equality today


This problem provoked me (and SCW) to think about why we need lifted equality at all. Currently (7.8.3), this is the story:

1. The user writes lifted equalities only.

1. Lifted equality is defined like

```wiki
data a ~ b :: Constraint where
  Eq# :: a ~# b -> a ~ b
```

1. The solver thinks in terms of lifted equalities only.

1. Casts (only in terms, of course!) use unlifted equalities.

1. The desugarer uses the result of the solver (the various `EvTerm`s and `EvBinds`s) to create and use lifted equalities. To use the lifted equalities in casts, the desugarer inserts `case` statements to unbox the lifted equalities. This is in `dsTcCoercion`.

1. GADTs are rejigged in terms of lifted equality for the wrapper, but the worker is in terms of unlifted equality. This is important, because we don't want to make space to store the equalities! The wrapper unboxes before calling the worker.

### Recent developments


In recent Skype calls, I've discussed the issue with SPJ and DV, among others. Here are some the points that came out of these conversations:

1. Once upon a time, equality evidence was all unlifted and considered to be a type variable. This caused endless havoc, because all other evidence (dictionaries, implicit parameters) are term variables.

1. A refactoring made equality just like other evidence: lifted and term-level.

1. Looking back, we questioned whether making equality lifted was necessary, while agreeing that it fits much better at the term level.

1. The solver could just as easily work over unlifted equality. It would be a simplification, actually, in that the desugarer would no longer have to box and unbox. (The unnecessary boxing/unboxing is later removed by the simplifier.)

1. A potential snafu is that the solver can spit out mutually-recursive groups of bindings. We, of course, cannot put an unlifted binding in a recursive group. So, the plan is to push the unlifted bindings into the RHSs of the letrec group, and use non-recursive lets. For example:

```wiki
let x :: C a = ... z ...
    y :: t ~ s = ... x ...   -- class C could have an equality superclass
    z :: t ~# s = unbox y
```

>
> becomes

```wiki
let x :: C a = let z :: t ~# s = unbox y in ... z ...
    y :: t ~ s = ... x ...
```

>
> Simon has spotted a tricky bit here, but I can't recall it. Simon?

1. Another small annoyance with unlifted equality is deferred type errors. This can likely be mitigated by floating in the (now-strict) calls to `error`.

1. The bigger problem with unlifted equality is that unlifted things -- for good reasons -- can't be abstracted over. For example, if we have

```wiki
data Dict c where
  Dict :: c => Dict c
```

>
> it would be disastrous if `c` could become `a ~# b`. So, users still need to work in terms of lifted equality.

1. I proposed a plan of redefining lifted equality thusly:

```wiki
class a ~# b => a ~ b
instance a ~# b => a ~ b
```

>
> Let's call this redefined lifted equality `~2`. Although the internal representation of `~2` is the same as that of `~`, the treatment in the solver would differ significantly. When the solver sees `~2`, it would just access the superclass (in the case of a given) or the one instance (in the case of a wanted), and then the real equality solving would happen over `~#`. This has the advantage of simplifying the desugarer, but still requiring the let-pushing in point 5, above. But, this is a big win for me because I need the solver to work over unlifted equality for kind casts, so using `~2` instead of `~` would mean that the solver works over only 1 equality type. There were no objections to this plan, as of Nov. 8, 2014.


The point articulated directly above seems to be a nice resting place for this discussion, and it is my plan of record going forward. However, because my current hybrid solver (that works over both `~` and `~#`) is chugging along, doing this redesign is not a high priority. Furthermore, this plan does **not** fix [my original problem](dependent-haskell/internal#), of needing unlifted equality in types.

### A simple solution the `SameKind` problem

**When users write `~`, it means lifted equality, unless unlifted equality is necessary. In the latter case, `~` means unlifted equality.**


For easy reference, here is the challenge:

```wiki
data SameKind (k :: *) (a :: k) (b :: k)
challenge :: forall (k1 :: *) (k2 :: *) (a :: k1) (b :: k2). (k1 ~ k2) => SameKind k1 a b
```


When type-checking and desugaring the type of `challenge`, above, we can discover that the `k1 ~ k2` equality must be used "right away". Under my proposal, this means that the `~` would simply be interpreted as unlifted. This is possible at all because we can discover -- crucially, in the same type -- the need for unlifted equality quite easily. Thus, `challenge`'s type is translated to FC like this:

```wiki
challenge :: forall (k1 :: *) (k2 :: *) (a :: k1) (b :: k2) (c :: k1 ~# k2). SameKind k1 a (b |> sym c)
```


How can this be implemented? That's a little more challenging because type-checking and desugaring happen in the same pass (unlike for terms). By the time we've discovered that we need unlifted equality, the equality constraint has already been desugared from `HsType` to `Type`. There's a great trick we can pull here, though: parameterize `~` by a levity variable! (See [NoSubKinds](no-sub-kinds).) This unifies the types `~` and `~#`.


We create a new type

```wiki
EQ# ::  forall (v :: Levity) (k1 :: *) (k2 :: *) (a :: k1) (b :: k2). TYPE v
```


That is, `EQ# Lifted` is good ol' `~` and `EQ# Unlifted` is just `~#`. When a user writes `~`, it desugars to `EQ# alpha`, where `alpha` is a levity metavariable. If this variable is still unconstrained when we're done type-checking the type, it defaults to `Lifted`. If an equality is used within the type, the type-checker will set `alpha` to `Unlifted`. This is very similar to the treatment of levity metavariables that arise when type-checking `->` or `error`. This idea also has the nice advantage of getting rid of `~` and `~#` as separate tycons within GHC -- we can now just talk in terms of `EQ#`.


This is all really quite pleasing to me, as it seems to simplify things somewhat, rather than complicate them. That's a great thing when it happens.

### Some non-solutions to the `SameKind` problem


It seems worthwhile to jot down some failed ideas of how to solve `SameKind`:

1. Write a type family `Cast`:

```wiki
type family Cast (k1 :: *) (k2 :: *) (a :: k1) (g :: k1 ~ k2) :: k2 where
  Cast k1 k2 a (Eq# g#) = a |> g#
```

>
> This type family does the unboxing and then casts. If the supplied lifted equality evidence is not `Eq#`, then the type family application is stuck, as it should be.

>
> This doesn't work because it's not compositional. If we have an equality for `Maybe a ~ Maybe b`, we can't case from `a` to `b`. This is because we can't use the coercion formers anywhere. A solution here is to just write type families that lift each coercion former into lifted equality. This works in every case but the `forall` coercion former, which can't be written as a type family because it binds a variable locally. If we had type-level lambdas, this approach could work.

1. Have `~` always mean unlifted equality.

>
> This doesn't work because we then can't abstract over equality predicates.

1. Write a type family `Unbox`:

```wiki
type family Unbox (k1 :: *) (k2 :: *) (g :: k1 ~ k2) :: k1 ~# k2 where
  Unbox k1 k2 (Eq# g#) = g#
```

>
> The problem here is that we certainly can't call a type family from within the coercion grammar. There's no way of using `Unbox`! Even if we made space somehow for just this one type family, when would it get "evaluated"? Never, so the system is broken.

1. Introduce `case` into types.

>
> This actually works (I think) and shouldn't cause any undue wrinkles. The `case` would simplify through iota-reduction axioms that get applied (still no computation within types), but I think this could work. But, it's decidedly **not** simple.
