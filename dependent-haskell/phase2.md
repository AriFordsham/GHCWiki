# Implementing Dependent Haskell, Phase 2


This page is to track design ideas and questions in the second phase of implementing Dependent Haskell, which is the development of a dependently typed Core. Phase 3 will include modifying surface Haskell and implementing type inference.

## Phase 2a: Homogeneous equality


The "System FC with Explicit Kind Equality" paper (ICFP'13) describes a *heterogeneous* equality: that is, we can form `a ~# b` even when `a :: k1` and `b :: k2`. A proof of `a ~# b` implies both that `k1` equals `k2` and that `a` equals `b`. This choice was necessitated by, among other things, the unusual binding structure in that paper's coercion between forall-types.


During the implementation of `TypeInType` (directly based on that paper), we realized several simplifications:

1. We do not need to make the binding structure of forall-coercions so strange. Instead, we can use an asymmetrical rule:

```wiki
G |- g1 : t1 ~ t3
G,a:k1 |- g2 : t2 ~ t4
------------------------------------------------------------------------
G |- forall a:g1.g2 : (forall a:t1.t2) ~ (forall b:t3.t4[b |> sym g1/a])
```

>
> Though it's asymmetrical, it's far simpler than the rule in the ICFP'13 paper. This is the rule implemented in GHC 8.x.

1. The ICFP'13 paper allows the binding of coercion variables in types. That is, we can have `forall c:phi.t` as a type. However, the need for this in practice was slight, and so it was removed from the implementation.


With the simpler (asymmetrical) forall-coercion rule above, one of the primary motivations for heterogeneous equality was removed. And so, in "A Specification for Dependent Types in Haskell" (ICFP'17), we use more of a mixed economy of heterogeneity: a coercion can still related two types of different kinds, but coercion *variables* must be homogeneous. That is, if `c :: t1 ~# t2`, then `t1` and `t2` have equal (that is, alpha-equivalent) kinds. But if a coercion `g` relates `t1` and `t2`, then `t1` and `t2` might have different kinds `k1` and `k2`. However, we can always extract a proof that `k1 ~# k2` from `g`.


We propose to make this change to GHC too.

### Why homogeneous equality is good


Homogeneous equality is simpler than heterogeneous equality so, all else being equal, it's better to have homogeneous equality. However, even beyond simplicity, we are able to prove **congruence** only with the homogeneous variant. Richard's thesis uses heterogeneous equality, and he was unable to prove congruence there. The ICFP'17 paper, with homogeneous equality, proves congruence. (It's called substitutivity there.) So this seems like a nice step forward. 


Congruence means that if a type `t` has a free variable `a`, and we have a coercion `co` proving `s1` equals `s2` (and `s1` and `s2` have the same kind), then we can always find a proof that `t[s1/a]` equals `t[s2/a]`. The lack of congruence has no effect in the current (GHC 8.6) implementation because it bites only in the presence of coercion quantification in types. See Section 5.8.5.4 of Richard's thesis. Given that we will need coercion quantification in types in order to have full dependent types \[example needed\], we will want congruence, too.

### What would homogeneous equality look like in GHC?


How should this homogeneous equality take form? It's simple: make `~#`, the type of primitive equality, homogeneous. That is, we want

```
(~#):: forall k. k -> k ->TYPE(TupleRep'[])
```


(The return kind says that equality proofs take up 0 bits at runtime.) All coercion variables must have a kind headed by `~#`. With the new kind of `~#`, then we effectively make all assumptions homogeneous. (Axioms were already, and have always been, homogeneous.) Coercions themselves can remain heterogeneous, but we can write definitions like this:

```
coercionKind::Coercion->PairType-- the two types might have different kinds-- if Pair t1 t2 = coercionKind co, k1 = typeKind t1, and k2 = typeKind t2, then-- Pair k1 k2 = corcionKind (promoteCoercion co)promoteCoercion::Coercion->Coercion
```

`promoteCoercion` is a function that transforms one coercion (tree) into another; it is no longer a coercion constructor (i.e. the existing `KindCo` vanishes).

### A small wrinkle: we need coercion quantification back


If `~#` is homogeneous in Core, then how do we support heterogeneous equality in Haskell? Heterogeneous equality is important in Haskell to support, for example, the new `TypeRep` (see [ the paper](https://repository.brynmawr.edu/cgi/viewcontent.cgi?article=1002&context=compsci_pubs)).   Easy: just use an equality between the kinds and then one between the types. But it's not so easy in practice. Examine the definition of `~~`, written as a Core datatype, even though it's really a class:

```
-- this version is wrong!data(~~):: forall k1 k2. k1 -> k2 ->ConstraintwhereMkHEq:: forall k1 k2 (a :: k1)(b :: k2).(k1 ~# k2)->(a ~# b)-> a ~~ b
```


Sadly, this is ill-kinded: we're using `a ~# b` even though `a` and `b` have different kinds. Of course, we know that `k1` and `k2` are the same, but that doesn't quite help us here. Instead, we need to *name* the coercion between `k1` and `k2`, thus:

```
data(~~):: forall k1 k2. k1 -> k2 ->ConstraintwhereMkHEq:: forall k1 k2 (a :: k1)(b :: k2). forall (co :: k1 ~# k2)->((a |> co)~# b)-> a ~~ b
```


This version names the kind coercion `co` so it can be used in the type proposition. All is well again. Sadly, the implementation does not support coercion quantification in types like this. 


Bottom line: a consequence of switching to homogeneous equality is that we need coercion quantification in types.  So it's time to implement it.

### Coercions are quantified both relevantly and dependently


Up until now, GHC has kept two ideas separate: relevance and dependence. 

- A variable quantified *relevantly* is preserved until runtime
- A variable quantified *dependently* is available for use in a type.


Term variables are relevantly quantified (with `FunTy`), while type variables are dependently quantified (with `ForAllTy`) and are irrelevant. 


Coercion variables today are relevant but not dependent.  (Even though they have 0 bits, a coercion variable is treated very much like a normal term-level variable.)
Now, though, need to be both dependent *and* relevant.  We thus have a problem:

```
typeKind(\(x ::Nat)....)=FunTyNat...typeKind(\(a ::Type)....)=ForAllTy(a::Type)...typeKind(\(c :: a ~# b)....)=????????
```


This choice is important, because we will need to check the type of an expression by comparing it to some other type with `eqType`. We cannot make arbitrary decisions between `ForAllTy` and `FunTy` here. Here is the design Simon and Richard (with Ningning's input) have come up with:

- The type of a coercion-lambda shall be a `ForAllTy` iff the coercion variable is mentioned in the result type. It shall be a `FunTy` iff the coercion variable is not mentioned in the result type.


Equivalently:

- INVARIANT: If a `ForAllTy` quantifies over a coercion variable, that variable *must* be mentioned later in the type.


With these choices, we never have to equate `ForAllTy`s with `FunTy`s in, say, `eqType`. The downside to this design is that it is non-performant: we must do a free-variable check when building `ForAllTy`s to maintain the invariant. However, the key observation here is that we need to do this check only when building a `ForAllTy` with a coercion variable -- *something we never do today*. So it will be rare in the near future. And, we should be able to easily distinguish when we're about to quantify over a coercion, so the normal `mkForAllTy` can just `ASSERT` that its variable is a tyvar. We'll have a new `mkCoVarForAllTy` that quantifies over coercions and does the free-variable check.


We believe that, in the future, `FunTy` and `ForAllTy` may merge into `PiTy`. That future is not yet here, and there's no need to rush it. 

### Coercion holes


As the constraint solver solves constraints, it generates *evidence*. For class constraints, this evidence takes the form of dictionaries that package the implementations of class methods. For equality constraints, this evidence takes the form of coercions. Because sometimes the solver has no place to bind evidence (like when we are kind-checking types, so there is no `let` or `case`), it stores coercion evidence in *coercion holes*. A coercion hole is a mutable cell, of type `IORef (Maybe Coercion)`. The cell starts out empty (`Nothing`) and then is filled in when the solver know how to build the coercion. 


These holes appear in coercions during type inference. When we're done type checking, coercion holes are zonked to be replaced by the coercion in the hole. All holes will be filled by the end -- otherwise, the program has a type error that we will have reported to the user. (There is special allowance for deferred type errors, which I won't describe here.)


None of the above changes. However, currently, coercion holes are implemented with this definition, in TyCoRep:

```
dataCoercion=...|CoHoleCoercionHoledataCoercionHole=CoercionHole{ ch_co_var ::CoVar, ch_ref    ::IORef(MaybeCoercion)}
```


By pairing the mutable cell with a `CoVar`, we get several benefits:

- The `CoVar` has a name and unique, so we can print it during debugging to track the coercion holes.
- The `CoVar` has a kind, so we know the two types that the coercion hole relates. This is necessary to be able to write `coercionKind`.
- The `CoVar` has a role (buried in its kind), necessary to implement `coercionRole`.
- The `CoVar` can be included in sets of free variables in a coercion. This is necessary so that we do not float a coercion with a hole out from an implication invalidly. See `Note [CoercionHoles and coercion free variables]` in TyConRep.


In our new world with homogeneous equality, we have a problem, though: a `CoVar` must be homogeneous. Yet, the solver will sometimes have to work with heterogeneous equality (more on that later). We thus have to remove the `CoVar`. Thus, `CoercionHole` becomes

```
dataCoercionHole=CoercionHole{ ch_types ::PairType, ch_role  ::Role, ch_name  ::Name-- for debugging only, ch_level ::TcLevel-- more on this below, ch_ref   ::IORef(MaybeCoercion)}
```


These new coercion holes are *not* returned as free variables.

### Preventing floating


If coercion holes are no longer returned as free variables, how do we prevent bad floating? (Here, "floating" refers to the process by which a constraint inside an implication is floated out of that implication -- that is, the constraint is attempted absent any of the assumptions of the implication. This can be done only when the constraint mentions no variables bound by the implication.)  By tracking levels. The type checker manages a `TcLevel`, a natural number that starts at 0 and is incremented as the type checker enters scopes. Essentially, the `TcLevel` is the count of how many local scopes the type checker has entered. All type variables are assigned a `TcLevel` saying what scope they belong to. Note that there are no global type variables, so these levels start at 1. To prevent floating, all we have to do is to make sure that the maximum level of any variable in a type is not equal to (or greater than) the level of the implication. (All implications have levels, too, because implication constraints correspond to local scopes. The level in the implication is the level of the variables in the implication.) If the maximum level of any variable in a type is less than the level of the implication, floating is fine.


We already have the maximum-level checker: `TcType.tcTypeLevel`. All we need to do is add levels to coercion holes (we can use the level from `getTcLevel :: TcM TcLevel`) in the `ch_level` field and then incorporate that into `tcTypeLevel`. Then, we modify the floating-out mechanism to do a level-check instead of a free-variable check. This is done in `TcSimplify.floatEqualities`.

**RAE Question:** Currently, `floatEqualities` does a free-variable check and then *promotes* levels (reducing level numbers) of some variables. If we use the level numbers to decide what to float, when will we ever promote? This seems like it might not work. **End RAE**

**Answer from Simon**
Consider this implication constraint

```wiki
forall[2] b[2].
   forall a[3].
      ([W] alpha[2] ~ a[3],
       [W] beta[2] ~ (b[2], gamma[3]))
```


It is nested somewhere inside a constraint tree; hence this implication
has level 3.  I have put level numbers on every variable.  The greek
ones are unification variables.


This implication has two nested equalities:

- We *cannot* float `alpha[2] ~ a[3]` because it mentions the skolem `a`, which is bound by the implication.
- We *can* float `beta[2] ~ (b, gamma[2])` because it does not mention a skolem bound by this implication.


So we float out the second equality (but not the first), thus:

```wiki
forall[2] b[2].
   [W] beta[2] ~ (b[2], gamma[3]),
   forall[3] a[3]. ([W] alpha[2] ~ a[3])
```


But now that `gamma[3]` has too large a level number.  It doesn't follow the `(WantedInv)`
in `Note [TcLevel and untouchable type variables]` in `TcType`.


So we promote, creating a fresh unification variable `gamma2[2]` and setting `gamma := gamma2`.
Now we have

```wiki
forall[2] b[2].
   [W] beta[2] ~ (b[2], gamma2[2]),
   forall[3] a[3]. ([W] alpha[2] ~ a[3])
```


Currently we decide which constraints we can float by

1. Finding which variables are *bound* by the implication (see `Note [What prevents a constraint from floating]` in `TcSimplify`).
1. Finding which variables are *mentioned* by the constraint.  
1. Seeing if the two sets have an empty intersection.  If so, float.


We are considering using level numbers (which all type variables now have) to simplify this.  Thus:

1. Find the level number of the implication.  That's easy: the implication records it directly `ic_tclvl`!  In the above it's denoted `forall[lvl] skols. body`.
1. Find the maximum level number mentioned in the constraint.  Here we have to be a bit more careful.   As you can see from the example, we want to consider the level numbers of the *skolems* but totally ignore the *unification variables*.
1. See if (2) \< (1).


No sets, no intersection, so easy.


Incidentally, this level-number business would dramatically simplify this code in \`TcSimplify.floatEqualities

```wiki
       ; let seed_skols = mkVarSet skols     `unionVarSet`
                          mkVarSet given_ids `unionVarSet`
                          foldEvBindMap add_one emptyVarSet binds
             add_one bind acc = extendVarSet acc (evBindVar bind)
             -- seed_skols: See Note [What prevents a constraint from floating] (1,2,3)

             extended_skols        = transCloVarSet (extra_skols eqs) seed_skols
```


Gah!  Looking at the bindings, transitive closure... horrible.  If every coercion variable had a level number indicating which level it is bound at, we could throw all this away.

**End of answer from Simon**

**RAE:** To summarize, you propose to ignore unification variables when doing the floating-out level-check. (Presumably, we won't ignore unification variables' kinds.) I'm still bothered though: we're worried about having coercion holes prevent floating. Coercion holes are very much like unification variables. If we ignore unification variables (and, by consequence, coercion holes), then do we have [\#14584](https://gitlab.haskell.org//ghc/ghc/issues/14584) again? If we don't ignore coercion holes, then when will coercion holes ever get floated? I'm still very unconvinced here. **End RAE**

### Heterogeneity in the solver


While we'd like to remove heterogeneous coercion variables from Core, they are useful in the solver.
Specifically, when we're analyzing an equality like `ty1 ~ (ty2 |> co)`, it\[\['s helpful to strip off the `co` and look at `ty1 ~ ty2`. This might discover similarities between `ty1` and `ty2` that can move solving forward. One of `ty1 ~ (ty2 |> co)` or `ty1 ~ ty2` is heterogeneous.


Currently, all constraints in the solver have types. For example, a constraint might have a type of `Eq [a]`. Equality constraints have types, too. Currently, these types use `~#`. But if we have a homogeneous `~#`, then we won't be able to express a heterogeneous equality constraint using `~#`.  In fact, a heterogeneous equality constraint won't have a type at all.   Remember the "mixed economy" of "A specification of dependent types for Haskell":

- A coercion *variable* (which has a type `t1 ~# t2`) must be homogeneous
- A *coercion* can be heterogeneous.  It does not have a type.


Equality constraints in the solver are witnessed by coercions and therefore may not have a type.


The types in the solver are useful because we make evidence bindings with those types. However, all equality Wanted constraints use coercion holes for their evidence (more on Givens later), so no binding is needed. We can thus store a heterogeneous equality constraint simply by storing a pair of types.


Note that the `TcEvDest` type stores either an evidence binding or a coercion hole. The new form of constraint (the pair of types) will always go hand-in-hand with the `HoleDest` constructor of `TcEvDest`.

### Heterogeneous given equalities


Even though we do not use evidence bindings for equality Wanteds, we still do use bindings for equality Givens. For example:

```wiki
class a ~# b => C a b where ...
f :: C a b => b -> a
```


We'll typecheck this to get

```wiki
f = /\a \(d :: C a b). \(x::b).
   let co :: a ~# b = sc_sel_1 d
   in x |> sym co
```


The superclass selection can only be done in term-land.  We cannot inline it to get

```wiki
f = /\a \(d :: C a b). \(x::b).
   x |> sym (sc_sel_1 d)
```


So at least some bindings for Given equalities must generate a real binding; we cannot use coercion holes for them. We thus need a dual of `TcEvDest`:

```
dataCtEvidence=CtGiven-- Truly given, not depending on subgoals{ ctev_pred ::TcPredType, ctev_evar ::TcEvSource<------NEW!WasEvVar!, ctev_loc  ::CtLoc}|CtWanted-- Wanted goal{ ctev_pred ::TcPredType, ctev_dest ::TcEvDest, ctev_nosh ::ShadowInfo, ctev_loc  ::CtLoc}dataTcEvSource=EvVarSourceEvVar|CoercionSourceCoercion
```


Just as a wanted constraint carries with it a `TcEvDest`, a given constraint will have to carry a `TcEvSource`. Unlike wanteds, though, *sometimes* an equality given will be an `EvVarSource`, if that equality given arises from, say, a superclass selector or GADT pattern match or some such. When a `CoercionSource` given is used, we just substitute the given into the coercion we are building (in a `CoercionHole` value). This will happen in `TcRnTypes.ctEvCoercion`, and possibly elsewhere.

### Some other details

- The three primitive equality tycons (`eqPrimTyCon`, `eqReprPrimTyCon`, and `eqPhantPrimTyCon`) all get a homogeneous kind.

- `coercionKind` does not need to change.

- Remove the now-redundant `KindCo` constructor for coercions.

- `coercionType` now works only over homogeneous coercions. We will have to audit usages of this function to make sure it doesn't get called on something heterogeneous.

- The core-spec will have to be updated.

- `~~` will have to be updated to use two `~#`s, as demonstrated above.

- Simon suggests that it is easier to have `~` refer directly to `~#`, instead of the current setup where it is defined in terms of `~~`. This is an unnecessary refactoring, but it might lead to a small performance win as there is one fewer indirection.

### Open questions

- What is the concrete design (type definitions) for the types in the solver to deal with heterogeneous equality constraints without using `~#`?

- At some point, GHC must assume that `ForAllTy`s are irrelevant. Now, however, a `ForAllTy` over a coercion variable is relevant, and must make a proper runtime function. Where is the code that has to change?
