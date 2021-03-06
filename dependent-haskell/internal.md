
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

### New idea on 2/2/15


This whole coherence thing is a mess. Specifically, with Coherence1, we have to require that (kind g) always be representational, regardless of the role on g. This is because Coherence1 allows for nominal equality between types of kinds that are only representationally equal. This, in turn, causes problems in the solver, because homogenizing equalities spits out representational kind coercions, which then can't be solved by unification. (Right now, I have a hack to unify these. But I hate it.)


So, new, drastic proposal: Have `eqType` ignore casts and coercion arguments, as long as the two types have the same kind. This means that `t` and `t |> <k>` are definitionally equal. This is actually a nice simplification, because it means that instance lookup, etc., properly ignores irrelevant bits. (The fact that my current implementation doesn't ignore these bits causes real problems.) A further consequence of this decision is that `tcUnifyTys` should work only up to this new definition of equality, which will require some re-engineering.


I thought for a while that the change would mean that we can drop coherence entirely. But this is wrong. Suppose we have `g :: t1 ~ t2`. We need a way to say `g' :: t1 |> h ~ t2`, when `h` is not reflexive. For non-reflexive `h`, `t1` and `t1 |> h` are *not* definitionally equal. Thus, we need to be able to build `g'` from `g` somehow. This is necessary in, say, `canEqCast` in the canonicalizer.


But the change does mean that we can use a modified `NomCoherence` rule, and drop the asymmetrical versions. We never need an asymmetrical coherence rule because we can always slot in a reflexive coercion where we wish to omit one -- reflexive coercions are always ignored! Here's the rule

```wiki
g : t1 ~r t2
h : k1 ~r k2
t1 |> h1 : k1
t1 |> h2 : k2
------------------------------------------- Coherence*
g |>_h (h1, h2) : (t1 |> h1) ~r (t2 |> h2)
```


This is just like `NomCoherence` above, but `Coherence*` allows `g` and `h` to have any (same) role. The `h` coercion is redundant if `r` is representational, but it is critically necessary if `r` is nominal. After all, we want `kind g` to have the same role as `g` in the end!


Going ahead with this idea now...

### It's all gone pear-shaped!


So, the above idea doesn't quite work. Say we have

```wiki
 [G] co: t1 |> g ~N t2
   where
  t1 :: k1
  g  :: k1 ~R k1'
  t2 :: k2
```


How should we canonicalize? In practice, it's very important to be able to deal with casted types, as they come up a lot as we have only partial information about kinds. (Often, we have a levity metavariable that we don't yet know is `Lifted`, yielding quite a few coercions in types.) A new principle is that, if `s1 ~N s2` then `s1`'s kind is N-equal to `s2`'s kind. This is how `kind` of an N coercion yields an N coercion. We know from the given `co` above that `k1'` is N-equal to `k2`. However, if we strip off the cast, then we know only that `k1` is **R**-equal to `k2`. We can't then reduce `co` to something involving `t1 ~N t2`!


It's possible that we could come up with a new way to solve in the presence of casted types without stripping off casts. (For example, this system allows for moving casts from one side of an equality to the other.) But it's hard to know how to proceed.

### A way forward?


What if `kind` always produced a **nominal** coercion? That solves the problem immediately above. And, with the right coherence rule, this might work out. Let me try.

**Edit:** This was quickly a terrible idea -- it doesn't make any sense at all.


Current plan (2/6/15): Just keep the old coherence rule, along with the added insight that whenever the type-checker needs to equate `(t1 :: k1)` and `(t2 :: k2)`, it produces `[W] t1 ~N t2` and `[W] k1 ~N k2`. If we're consistent about requiring **both** of these every time, it should all be OK.

### Status update, Aug 2015


The last point above -- the "Current plan (2/6/15)" -- is implemented and is working, generally. But it's dissatisfyingly baroque. A particular thorny point, not covered in detail here, is that it all requires a change to `AppCo`. The naive definition for `AppCo` takes two `Coercion`s, representing the lifted version of applied types. Suppose:

```wiki
t1 :: k1 -> k2
t2 :: k1
t1' :: k1' -> k2'
t2' :: k1'
c1 :: t1 t2 ~N t1' t2'
c2 :: k2 ~N k2' 
```


We have now followed the advice above, by having `k2 ~N k2'` alongside `t1 t2 ~N t1' t2'`. But what if we decompose the `AppCo`? We get, on the right, `t2 ~N t2'`, but we **don't** get `k1 ~N k1'`. This is problematic, as it violates our desire stated above. The solution is to put that second equality in `AppCo` as a third argument. This solution isn't actually terrible in the implementation, but it's an odd wrinkle in the theory, to be sure.


Simon has strongly entreated me to back off this stance. Instead, he proposes having all equalities among kinds be nominal. In particular, he wants this casting rule:

```wiki
t :: k1
c :: k1 ~N k2
------------- CastTy
t |> c :: k2 
```


Note that the coercion is nominal. By choosing this `CastTy`, all of the problems above are avoided. The considerable cost to this approach is that it prevents the straightforward promotion of expressions to types, as those expressions will have representational casts in them. But that's OK, for now, as we're not planning on doing any promotion *of Core* from expressions to types until we have Pi. And then, maybe we'll have a better idea.


I plan to make this change, and use the above `CastTy` rule.

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
>
> becomes
>
>

```wiki
let x :: C a = let z :: t ~# s = unbox y in ... z ...
    y :: t ~ s = ... x ...
```

>
>
> Simon has spotted a tricky bit here, but I can't recall it. Simon?
>
>

1. Another small annoyance with unlifted equality is deferred type errors. This can likely be mitigated by floating in the (now-strict) calls to `error`.

1. The bigger problem with unlifted equality is that unlifted things -- for good reasons -- can't be abstracted over. For example, if we have

```wiki
data Dict c where
  Dict :: c => Dict c
```

>
>
> it would be disastrous if `c` could become `a ~# b`. So, users still need to work in terms of lifted equality.
>
>

1. I proposed a plan of redefining lifted equality thusly:

```wiki
class a ~# b => a ~ b
instance a ~# b => a ~ b
```

>
>
> Let's call this redefined lifted equality `~2`. Although the internal representation of `~2` is the same as that of `~`, the treatment in the solver would differ significantly. When the solver sees `~2`, it would just access the superclass (in the case of a given) or the one instance (in the case of a wanted), and then the real equality solving would happen over `~#`. This has the advantage of simplifying the desugarer, but still requiring the let-pushing in point 5, above. But, this is a big win for me because I need the solver to work over unlifted equality for kind casts, so using `~2` instead of `~` would mean that the solver works over only 1 equality type. There were no objections to this plan, as of Nov. 8, 2014.
>
>


The point articulated directly above seems to be a nice resting place for this discussion, and it is my plan of record going forward. However, because my current hybrid solver (that works over both `~` and `~#`) is chugging along, doing this redesign is not a high priority. Furthermore, this plan does **not** fix [my original problem](dependent-haskell/internal#), of needing unlifted equality in types.

### A simple solution to the `SameKind` problem

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


Simon has pointed out that users may still eventually want control over this inference. So, here is the concrete plan: lifted equality is denoted by `Equal` in user code; unlifted equality is denoted by `Equal#`. The notation `~` means, essentially, `EQ#` -- that is, infer whether to use `Equal` or `Equal#`. This should be fully backward-compatible.


This is all really quite pleasing to me, as it seems to simplify things somewhat, rather than complicate them. That's a great thing when it happens.


Question: What is the kind of `Equal#`? I propose `forall k1 k2. k1 -> k2 -> Constraint#`.

### Some non-solutions to the `SameKind` problem


It seems worthwhile to jot down some failed ideas of how to solve `SameKind`:

1. Write a type family `Cast`:

```wiki
type family Cast (k1 :: *) (k2 :: *) (a :: k1) (g :: k1 ~ k2) :: k2 where
  Cast k1 k2 a (Eq# g#) = a |> g#
```

>
>
> This type family does the unboxing and then casts. If the supplied lifted equality evidence is not `Eq#`, then the type family application is stuck, as it should be.
>
>

>
>
> This doesn't work because it's not compositional. If we have an equality for `Maybe a ~ Maybe b`, we can't case from `a` to `b`. This is because we can't use the coercion formers anywhere. A solution here is to just write type families that lift each coercion former into lifted equality. This works in every case but the `forall` coercion former, which can't be written as a type family because it binds a variable locally. If we had type-level lambdas, this approach could work.
>
>

1. Have `~` always mean unlifted equality.

>
>
> This doesn't work because we then can't abstract over equality predicates.
>
>

1. Write a type family `Unbox`:

```wiki
type family Unbox (k1 :: *) (k2 :: *) (g :: k1 ~ k2) :: k1 ~# k2 where
  Unbox k1 k2 (Eq# g#) = g#
```

>
>
> The problem here is that we certainly can't call a type family from within the coercion grammar. There's no way of using `Unbox`! Even if we made space somehow for just this one type family, when would it get "evaluated"? Never, so the system is broken.
>
>

1. Introduce `case` into types.

>
>
> This actually works (I think) and shouldn't cause any undue wrinkles. The `case` would simplify through iota-reduction axioms that get applied (still no computation within types), but I think this could work. But, it's decidedly **not** simple.
>
>

## Open questions

1. What is the kind of `Equal#`? I propose `forall k1 k2. k1 -> k2 -> Constraint#` for a new kind `Constraint#`.

1. What are the restrictions on the use of `Constraint#`s in tuples, superclasses, and such?

1. What checks should Core Lint do about levity polymorphism? Where is it allowed and where is it not allowed?

# `TcTyVars`


This is from emails with SPJ & DV on 12/31/14:

**RAE:**
1) Is there a reason to have `SkolemTv False` TcTyVars separate from
proper, zonked TyVars? There is some effort at switching back and
forth between proper TyVars and skolems, but I'm not sure why. I even
recall finding a case (we talked about it -- I forget the Trac \#)
where a skolem tyvar got past zonking and into Core, with no ill
effect. Conversely, TyVars are seen during kind unification (see
comments toward end of TcUnify), also to no ill effect.

**SPJ:**
Well my original invariant was supposed to be:


>
>
> the typechecker sees only TcTyVars (except perhaps as the 
> quantifier of a forall)
>
>

>
>
> the rest of the compiler never sees a TcTyVar
>
>


The SkolemTv False thing was just to ensure this. You'll see lots of ASSERTs for isTcTyVar which would fail if we used a TyVar. But I think nothing would actually go wrong if we allowed TyVars.  And as you observe, the invariant is not rigorously observed.


Originally SkolemTvs had lots of SkolemInfo in them, but they don't any more.

**RAE:**
In my branch, this change may be somewhat forced, for exactly the same reason that sometimes TyVars appear during kind unification. Of course, with type=kind, then the TyVars can appear anywhere! I won't actively actually remove SkolemTv for now (too much is broken at the moment, and this would only break more things!), but I won't care too much about the distinction, with a note to myself to clean this up later.

**RAE:**
2) In the TcS monad, there are two facilities that accomplish the same
goal but with very different mechanisms: meta-tyvars and EvBindsVar
stuff. Both allow in-place updating of types or coercions, but one
does it in a distributed manner (meta-tyvars) and one via a central
master list of bindings (EvBindsVar). Is there an advantage to having
these different mechanisms?

**SPJ:**
The original idea was that the *solver* would be pure.  It would have no side effects, and so in principle you could do backtracking search without having to undo side effects.  The TcS monad even carried a TvSubst which we used *instead* of side-effecting updates of the meta-tyvars.  When exiting the TcS monad we applied the substitution in a side-effecting way.


But (a) there was no powerful motivation for the goal of purity.  The only place we try search is in TcSimplify.disambigGroup when we see if we can solve things by filling in a meta-tyvar with Int, or Integer, or Float, etc.  But that's now done another way.


Moreover (b) the goal of purity was never achieved. Reason: the EvBindsVar stuff.  The solver solves an entire tree of constraints, side-effecting bindings in the EvBindVars kept at various points in the tree.   These EvBindsVars are *also* referred to from the HsSyn tree.  Now, we could have maintained a substitution from EvBindsVar -\> EvBinds, and applied that when exiting the substitution, but I never tried that.


Finally (c) if you *don't* need to undo things, then side-effecting substitution is very efficient!  (At least I think so; I have not actually measured it.)  Building a functional substitution and subsequently applying it by side effect seems necessarily slower.


So I removed the functional TvSubst and do both the EvBinds and meta-tyavar by side effect.

**RAE:** Is there something to be gained by
unifying these approaches?

**SPJ:**
Actually they *are* the same already.  In both cases we update a mutable variable.  In the EvBinds case it's not a single central "master" list; each Implication has its own EvBindsVar, which becomes the ambient one when we walk inside the Implication.

**RAE:**
But, with this approach, zonking becomes simply applying a substitution,
no? Continuing on from my first point, above, it might be possible to
eliminate TcTyVars entirely. This would seem to be a nice
simplification: no more zonking as a separate algorithm (2,
actually!), and no more instSkolTyVars. (Zonking inside the knot would
perhaps need to remain, as applying a subst can be strict in
TyCons...)

**SPJ:**
You seem to be suggesting extending the functional-substitution idea outside TcS to TcM.  Yes, we could do that.  But in effect the mutable store \*IS\* a single, global, ambient substitution!  But one that is implemented by the memory system.  So what would be gained, really?

**RAE:**
3) Part of recent refactoring allows the TcS monad to update a meta-
tyvar directly, with setWantedTyBind. In some places, the code that
calls setWantedTyBind then kicks out relevant bits of the inert set.
In other places, there is no kick-out. I would think we would *always*
want to kick out after setWantedTyBind, but perhaps I'm missing the
subtleties of fmvs and such.

**SPJ:**
The kick-out stuff is necessary only when we have an active InertCans and work-list; ie during TcInteract. Outside that, it doesn't matter.

**RAE:** Specifically, I'm worried about
defaultTyVar, which is misbehaving on me. Relatedly, does Note
\[DefaultTyVar\] still apply when there is no sub-kinding?

**SPJ:**
When there is no sub-kinding I think we can get rid of defaultTyVar *altogether*.  But maybe something different happens instead? What happens with the example in the Note?

```wiki
    instance Show (a->b)
    foo x = show (\_ -> True)
```


What kind do you get for `p` in the `Show (p -> q)` constraint?

**RAE:**
This doesn't really change.


We infer `(\_ -> True) :: (c -> Bool)`, where

```wiki
 c :: TYPE v
 v :: Levity
```


and `c` and `v` are meta-vars. The instance doesn't apply because the kind of `a` in the instance is \*, distinct from the kind of `c`. Defaulting (in my branch) simply writes `v := Lifted` using setWantedTyBind. So we have all the same issues at play, just with a different underlying mechanism.

# Separating `TcType` from `Type`


Also from emails 12/31/14.

**SPJ:**
On this question, I had been thinking about a more radical solution: make TcType and Type into distinct types, just as TcCoercion and Coercion are different.


A powerful reason for doing this is that TcTypes could have location information at every node in the tree, which is good for error reporting; Lennart gave a great talk at the Haskell Implementors Meeting about this.  Another reason is that we'd have a static guarantee that there were no meta-tyvars surviving after typechecking.


There are minor things too. For example, we could have a special constructor for type-function applications.


How hard would this be?  For much of the code it'd be easy.  E.g. a global Id contains a Type, which must be instantiated to a TcType even if it has no quantifiers.  A local Id would be a TcId, not an Id, with no IdInfo but with other info currently stored in the ATcId structure.


I'm quite sure there would be some dark and tricky corners, perhaps around the places where our current invariant doesn't quite hold.  But sorting them out would make everything cleaner and more robust.


It's a big, unforced change.  But I think it would be a jolly good thing.


Any opinions?

**RAE:**
Funny -- I just ran across a different place (in my branch) where this was called for.


My need is that the type-checker thinks in terms of TcCoercions, and occasionally needs to cast a type by a TcCoercion. Of course, Type has no room for such a cast. I've carefully avoided making this a problem, using two tricks:

- I've rejigged TcUnify.uType & friends to produce a Coercion, not a TcCoercion. This was straightforward, now that we can inject a Coercion into a TcCoercion (unifyType, the exported function, still produces a TcCoercion). But, now uType can deal with the heterogeneous case.

- In the solver, the same problem happens, but the solution isn't so easy. Say we have a Type s and a TcCoercion tco. We wish to write (s \|\> tco), but this is ill-formed. So, I create a new covar c of the same type as tco, bound to tco. I can then use CoVarCo to put c in a Coercion, and make (s \|\> c). When desugaring, this all works out, because any lifted coercion variables in a TcCoercion get unboxed, so all is good in the end. But, this all smells wrong.


And, it's always possible I'll come across a case where I need a type casted by a TcCoercion and no trick works.


With this change, it also opens up the door to separating desugaring of types from type-checking. The TcType could support wrappers, like HsExprs do. I currently don't think we'll need wrappers in types to implement dependent types, but I could very well be wrong.


While we're at it, we could be more general in the form of annotation. The flattener would awfully like to know whether a type is fully normalised w.r.t. type families, but that would have to be stored at every node.


I'm generally of the opinion that more types are better, unsurprisingly. (I've wanted to separate out Id, CoVar, TyVar, and TcTyVar for some time, along similar lines.)


My guess is that this change would decrease performance in places, with the extra conversions. Would this be significant? Probably not, but worth noting as a "con".

# Separating type signatures from definitions


In order to support induction recursion, we'll need to typecheck type-level signatures separately from definitions. For example, in

```wiki
data G a where
  MkG :: forall (g :: G *). Proxy g -> G Int
```


we need to kind-check and desugar `G` before working on `MkG`. This is because `G` is used as a kind in the type of `MkG`.


Right now, this would be impossible, because `G` is knot-tied when processing `MkG`, and kind-checking that type would surely loop. The only real way forward is to treat the header of `G` separate from the body. This would, of course, only be possible with a CUSK:

```wiki
data G (a :: *) where ...
```


(Without the CUSK, we'd need to kind-check the datacons before being able to deal with the header, and that's loopy... I'll leave it as an exercise to the reader to determine if there is a better way.)


Implementation plan: don't store tycon RHSs in the tycon itself, but in the environment. Here are the fields that have to move:


From `AlgTyCon`:

- `algTcRhs`: This is the big change. Although, it may be best to keep the nature of the RHS (family vs. data vs. newtype) in the `TyCon`. This won't pose a challenge.
  The challenge is that we sometimes use `tyConDataCons` away from `TcM`.

  - To create the `tag` in `mkDataCon`. Seems easy enough to re-plumb.
  - Sometimes with a class (`mkDictSelId`, `mkDictSelRhs`). May need to add the datacon to the `Class` object. Shouldn't be too bad, as classes are never loopy in this way. Actually, when we promote classes, they **can** be loopy in this way, but we're not going to worry about that now.
  - Some code-gen stuff. In a monad, but one with an env?
  - Core Lint. Could add the env to the linter monad. But the use is just a warning.
  - Producing worker ids in `CorePrep.mkDataConWorkers`. New plumbing around here?
  - Getting from one datacon to another (`DsUtils.mkDataConCase`). Perhaps the datacons could all refer to each other.
  - And a bunch of other places. Might not be so easy.

- `algTcRec`: No problem here. In one place (`checkRecTc` for avoiding recursive-newtype loops) it's accessed far from `TcM`, but it's just an optimization there. Otherwise, we have `TcM` to hand.


From `SynonymTyCon`: Nothing. Synonyms can't be loopy!


From `FamilyTyCon`: Closed type family RHSs.

---


Alternate plan: Make a new type `TcTyCon` that is missing the RHS and then zonk it to a `TyCon`. But this is terrible because we'd then need a `TcType` that stores `TcTyCon`s. And this change might not be enough, by itself, as we might need the RHS of `TcTyCon`s in places.

---


Other alternate plan: Be really loopy and dance among black holes, keeping the existing structure but putting everything into a bigger knot. This might actually not be so terrible.

# `tcUnifyTys` vs `tcMatchTys`


Email exchange with SPJ, early Jan. 2015:


Hi Simon,


The `Unify` module exports `tcMatchTys` for template-against-target matching and `tcUnifyTys` for full unification. However, the latter is parameterized by a function that informs it which variables it can add to its substitution. Is there a reason we can't implement `tcMatchTys` in terms of `tcUnifyTys`? When I looked at this once before, the answer was "sub-kinds", which matching respected but unification didn't bother about. Now that I don't have sub-kinds, do you see any reason not to make this drastic simplification?


Background info: I ask because I need to do somewhat delicate surgery to these functions, and would prefer to do it only once! Here's the scenario:

```wiki
class C k1 k2 (a :: k1 -> k2) (b :: k1)
data A = MkA
data Foo :: A -> *
type family FA where FA = A   -- thus, axFA :: FA ~ A
forall (b :: FA). instance C A * Foo (b |> axFA)   -- call this (1)
```


Now, I need an instance for

```wiki
target: C A * Foo MkA    -- call this (T)
```


But, just using `tcMatchTys` on (1) won't yield a match for (T). We need to look through casts, smartly, producing `(b |-> (MkA |> sym axFA))`. I think I know how to do such a match, but it's delicate, as you might imagine. Incidentally, I know this means that `lookupInstEnv` will now produce instances that might be slightly different than requested. In the example, `lookupInstEnv` would produce `(instance C A * Foo (MkA |> sym axFA |> axFA))`, instead of exactly (T). It turns out that this is OK -- callers to `lookupInstEnv` just need to take a bit more care to call a new function `buildCoherenceCo`, which will find the coercion between two types that are identical modulo casts.


In any case, I'm feeling pretty good about all of the above, except that I wanted a double-check before erasing the implementation of `tcMatchTys`.


Thanks!
Richard

**SPJ:**

- `tcMatchTy` observes alpha-equivalence for the template.  In particular the template variables can in principle occur in the type it is matching against have the same variables.  E.g.

```wiki
   Template        [x].  x->x
   Type to match   Maybe x -> Maybe x

   Result: success with x -> Maybe x
```


This obviously doesn't work for unification.  


I'm not sure we *use* this, but it's one less thing to worry about.

- `tcUnifyTy` doesn't work for foralls (**RAE:** It does now!), whereas `tcMatchTy` does. Maybe that doesn't matter.

- `tcMatchTy` is a lot simpler, and hence perhaps more efficient.


I didn't follow the "background info" I'm afraid; I didn't even understand instance declaration (1).  We don't usually get casts in an instance head.  Do we really need them?

# Definitional equality wrinkle


I've decided that it's a good idea for `t` and `t |> refl` to be equal according to `eqType`. In fact, I want to make any arrangement of coercions within a type irrelevant, as long as the kinds of the types are the same. This decision has pervasive effects, mostly good. We can now worry a lot less about coherence coercions, which won't exist between types of the same kind. But there is a wrinkle:


Suppose we have

```wiki
a :: Raining -> *
T :: Bool -> *

b :: Raining
```


and we're matching `a b` with `T True`. According to a definitional equality that ignores casts, this match should succeed, with `a |-> T |> sym axRaining -> <*>` and `b |-> sym axRaining`. This is success because `(T |> sym axRaining -> <*>) (True |> sym axRaining)` is well-typed and `eqType` to `T True`. But, we've hit a major problem, in that `axRaining` isn't mentioned anywhere in the query! There's no way for the unifier to produce it. Yet, failing on this match would be **wrong**, as it wouldn't respect definitional equality.


Proposed solution: "tag" each application with the kind of its argument. These tags remain relevant during the equality check. According to this slightly more restricted (finer) definitional equality, `a b` doesn't match with `T True`, because the kind tags are different! Problem solved.

### Alternate solution


The plan above is a little dissatisfying, because it means that matching is less eager than a user might reasonably expect... I can see answering a bug report as not-a-bug in some time over this issue.


An alternate plan would be to leave definitional equality intact, but to "invent" coercions during matching to fill the void. These could be `UnivCo`s with a distinguished provenance. When matching is all done, we could subst into the template, add some magic to `mkAppTy` so that casts on both types can cancel each other, and then check to see if any of the invented coercions is still present. We'd have to be careful, because the substs produced during matching/unifying are applied to more than just the template. In every case I could find, one of these two tricks would work:

1. Pass the match/unify function a list (possibly long) of places the subst will be used. The end-of-algorithm check for invented coercions would look throughout this list.
1. Emit the invented coercions as wanted during type-checking.


I can't think of any property that *guarantees* that approach (1) or (2) would always work, but searching through the codebase suggests that one of these options is always available.

### An easier approach


We have no guarantee that the matcher/unifier is complete. Just fail (with `MaybeApart`) in the tricky case. No need to complicate definitional equality.

# Matching axioms


Axioms may have casts in their LHS. This poses a problem for matching. So the matcher has been made more clever and essentially ignores coercions. This is a little thorny, but not terrible. The problem is that ignoring coercions means that the matcher has no way of discovering the value of coercion variables. But this makes some good sense, as coercion variables may be arbitrarily buried within coercions, and there's no reason at all that matching two coercions should reveal the same structure.


So: the matcher cannot find the value of coercion variables.


However, covars **can** appear in axiom RHSs. So this is a bit of a sad state of affairs, because there seems to be no pure way to simplify type family applications. When we can emit constraints, it's all OK, because we can just emit the covars as wanted and let the solver deal with it.


Further thought led to this observation: this is exactly the same situation as with class instances. There is no pure way to answer the question "Is there an instance for `C` at some type `t`?" because instances can be constrained. What makes type families harder is that type instances have an RHS; class instances essentially don't (as far as the type system is concerned). Because of this observation, I'm less bothered by the fact that we cannot reduce type families purely. Furthermore, the ability to reduce a family in a pure situation seems never to be absolutely necessary: it's done for optimizations and such.


The plan going forward is as described above: emit constraints when reducing a type family, and be incomplete when in pure code.

### A red herring


I thought for some time that axiom LHSs would have to be devoid of coercions, replacing any coercion with a bare variable to facilitate matching. This is wrong, precisely because the matcher ignores coercions. So, much of the time, coercion variables in axioms are unnecessary.

### An example


Here is an example of where we want a coercion variable in an axiom:

```wiki

type family F a

type family Blah (x :: k) :: F k

data Foo :: forall k. k -> F k -> * -> *

type family G a
type instance G (Foo @k a (Blah a) (Blah a)) = Int
```


The instance will infer `c :: F k ~ *`.


It's possible that we will want such things to be available only when a user declares:

```wiki
type instance (F k ~ *) => G (Foo @k ...) = ...
```


Emitting constraints when reducing type families also opens the door to arbitrary constraints that control instance selection. However, these constraints would be just like class instance constraints, in that they would be emitted only *after* matching the types. This behavior is probably not what folks want when they ask for constrained type instances.

### Possible improvement


It seems that, whenever a target type is well-kinded, there is *some* solution for the coercion variables in an axiom LHS. Since this is the case, I can imagine building machinery that does indeed solve for coercion variables in a pure manner. But this problem is left for another day.

### User-facing design issues


It seems much better from a user standpoint to make these constraints be explicit, like the rephrasing at the end of the example, above. Thus, **design decision:** inferred unsolved equality constraints when checking type family equations are always errors, regardless of what `quantifyPred` thinks. The user can write a context (containing only equality constraints, for now) if they want other behavior.


In data families, it's a little subtler, because the RHS is not just a type. In particular, tycons can't have covars. If they did, the covars would have to act quite like stupid thetas, and we don't want that. Instead, every data constructor must also share the constraint(s) of the instance head. This is necessary because data family instances are injective: we must be able to get the LHS from the RHS. This is needed, specifically, in implementing datacon wrappers. Consider this, following from the example above:

```wiki
data family D a
data instance D (Foo @k a (Blah k a) (Blah k a)) = MkD (Blah k a)

-->
[k :: *, a :: k; c :: F k ~ *]. D (Foo k a (Blah k a) (Blah k a |> c)) ~ DCon k a
data DCon k a where
  MkD :: forall k (a :: k). (F k ~ *) => Blah k a -> DCon k a
```


We need a wrapper for `MkD`:

```wiki
wrapMkD :: forall k (a :: k). (F k ~ *) => Blah k a -> D (Foo @k a (Blah k a) (Blah k a))
```

**There's that darned pear again! ** This doesn't work because of the "SameKind" problem: we need to use the `F k ~ *` constraint in the type. We're not implementing this until we have Pi. So this is out of reach. Conclusion: reject constrained data family instances, for now.

### Sometimes, we want to infer constraints


Take this new example:

```wiki
type family F a

data G a where
  MkG :: F a ~ Bool => G a

foo :: G a -> F a
foo MkG = False

type family Foo (x :: G a) :: F a
type instance Foo MkG = False
```


Without the type family, this compiles on GHC 7.8. We thus desperately want the type family to compile with our extension. But it won't, if all type instance constraints must be declared. So we make an exception for equalities brought into scope by a GADT pattern-match. Should be easy enough to implement and specify.

# User-facing design issues

- A redditor suggested replacing `*` with `type`. Note that `type` is lowercase! I think this is a good idea. Replacing `*` with something gets strong support on reddit. See the [page of interest](https://www.reddit.com/r/haskell/comments/3hlck0/planned_change_to_ghc_merging_types_and_kinds/).

- It was also pointed out that `StarInStar` is wrong. Because `* :: *` is always true but the ability to use this, in some scenarios, is limited. It was suggested to use `KindTypes`, but I find that extension name totally opaque.
