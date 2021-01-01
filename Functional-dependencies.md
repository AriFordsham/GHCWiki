This page summarises examples and issues about functional dependencies in GHC.

[[_TOC_]]

## 1. Key papers

* [Understanding functional dependencies via constraint handling rules](https://www.microsoft.com/en-us/research/publication/understanding-functional-dependencies-via-constraint-handling-rules/), Sulzmann et al, JFP 2006.
* [Type classes with functional dependencies](https://web.cecs.pdx.edu/~mpj/pubs/fundeps.html), Mark Jones, ESOP 2000.

We will call the JFP paper (Sulzmann et al) the **JFP-paper**.

-------------------------------

## 2. Terminology

### Confluence and termination

**Confluence** means that:

* A program will either typecheck or not; it can't typecheck one day and fail the next day.
* If it typechecks, it'll have the same meaning.  (Exception: with overlapping instances and different instances in scope in different modules.)
* You can re-order the constraints in a signature without affecting whether the program typechecks, or what it means
  ```
  f :: (C a, D a b) => blah   -- These two should
  f :: (D a b, C a) => blah   -- behave the same
  ```
* You can re-order instance declarations without affecting whether the program typechecks, or what it means

**Termination** means that type inference terminates.

Generally,
* We'd like to be able to guarantee both termination and confluence.
* We are happy to risk non-termination when we ask for it; insisting on guranteed termination is very restrictive
* We are extremely reluctant to risk non-confluence.

### The Paterson conditions

The Paterson conditions try to ensure that, when you use an instance decl, the sub-goals are "smaller" than the head. E.g.
```
instance Eq a => Eq [a]
```
If you are trying to solve `Eq [Maybe Int]`, you can use the instance decl to get the smaller goal `Eq (Maybe Int`.

The Paterson conditions are described in the user manual under [Instance termination rules](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html?highlight=undecidable#instance-termination-rules).

Note: the Paterson conditions subsume the Bound Varible Condition (Defn 8) of the JFP-paper.

### Strict coverage condition (SCC)

The **(strict) coverage condition** is given in the user manual under [Instance termination rules](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html?highlight=undecidable#instance-termination-rules) and in Defn 7 of of the JFP-paper.  Consider
```
class C2 a b | a -> b
instance C2 [p] p     -- Satisfies strict coverage
instance C2 [p] [q]   -- Does not satisfy strict coverage
```
From the class decl for `C2` the first paramters should fix the second; that is true of the first instance here, but not of the second, becuase `q` is not fixed when you fix `p`.

**Liberal coverage condition (LCC)**

The **liberal coverage condition** is more liberal because it takes into account the context of the instance.  For example
```
instance {-# LIBERAL #-} C2 p q => C2 [p] [q]
```
I'm using `{-# LIBERAL #-}` to signal that the instance only satisfies the liberal coverage condition, not the strict one.
The intuition is that since `p` fixes `q` in the context, so `[p]` indirectly fixes `[q]`.

See Defn 12 of the JFP-paper, which calls it the "weak coverage condition".

### Strict instance consistency condition (SICC)

Consider these two instances
```
class C2 a b | a -> b
instance C2 Int Bool
instance C2 Int Char
```
They are mutually inconsistent. The fundep on `C2` says that `a` determines `b`; but if `a`=`Int`, then `b` can be both `Bool` and `Char`.

The instance consistency condition ensures that instances are pair-wise consistent. It appears not to be documented in GHC's user manual, but is is Defn 6 in the JFP-paper.

** (Strict) instance consistency condition **.  Consider a declaration for class TC, and any pair of instance declarations for that class:
```
class blah => TC a1 ... an | fd1; ...; fdm
instance D1 => TC t1...tn
instance D2 => TC s1...sn
```
Then, for each functional dependency `fdi`, of form `ai1; ...; aik -> ai0`, the following
condition must hold: for any substitution S such that S(ti1; ..., tik) = S(si1; ..., sik)
we must have that S(ti0) = S(si0).

### Liberal instance consistency condition (LICC)

Consider
```
class D p q | p -> q
class C a b c | b -> c
instance {-# LIBERAL #-} D p q => C Int  p [q]
instance {-# LIBERAL #-} D r s => C Bool r [s]
```
These instances do not satisfy the (strict) instance consistency condition.
Yet if they are not allowed, then the liberal coverage condition is not very useful.
That motivates the definition of liberal instance consistency:

** Liberal instance consistency condition **.  Consider a declaration for class TC, and any pair of instance declarations for that class:
```
class blah => TC a1 ... an | fd1; ...; fdm
instance D1 => TC t1...tn
instance D2 => TC s1...sn
```
Then, for each functional dependency `fdi`, of form `ai1; ...; aik -> ai0`, the following
condition must hold: for any substitution S such that S(ti1; ..., tik) = S(si1; ..., sik)
there must exist a substitution T such that
we must have that T(S(ti0)) = T(S(si0)); or equivalently S(ti0) and S(si0) are unifiable.


-----------------------

## 3. GHC today

GHC today does this:

* If `UndecidableInstances` is on, GHC
  * uses the liberal (not strict) coverage condition
  * lifts the Paterson conditions

* Always: GHC implements liberal instance consistency.  See `Note [Bogus consistency check]` in `GHC.Tc.Instance.Fundeps`.

-------------------------------

## 4. Some key examples

### Example 1: liberal coverage breaks termination

The liberal coverage condition means that type inference can diverge.
Examle from 5.2 of the JFP-paper:
```
class Mul a b c | a b -> c
instance {-# LIBERAL #-} Mul a b c => Mul a (Vec b) (Vec c)
```
This satisfies Paterson and LIBERAL.
Now suppose we are solving the constraint `[W] Mul alpha (Vec beta) beta`
* Fundeps give us `beta := Vec delta`
* Substituting we have `[W] Mul alpha (Vec (Vec delta)) (Vec delta)`
* That matches the instance decl, giving `[W] Mul alpha (Vec delta) delta`
* And now we are back where we began.

### Example 2: LCC and LICC do weird improvement (#10675 OP)

Consider
```
class CX x a b | a -> b where
  op :: x -> a -> b
instance                                CX Bool [x] [x]
instance {-# LIBERAL #-} CX Char x y => CX Char [x] [Maybe y]

f x = op True [x]
```
The instance decls require LICC.  But notice that they do not overlap, because of the first parameter.

From `f` we get `[W] CX Bool [alpha] beta`.
* Now GHC takes fundeps from *both* instances, giving `beta ~ [alpha]` and `beta ~ [Maybe gamma]`
* That leaves us with `CX Bool [Maybe gamma] [Maybe gamma]`
* We can solve that from the first instance decl.
* So we infer `f :: Maybe g -> [Maybe g]`.
  Bizarre.  Where did that `Maybe` come from?  It's nothing to do with it.

### Example 3: LCC and LICC threaten confluence

Consider:
```
class D a b c | b -> c
instance {-# LIBERAL #-} (q ~ Int)  => D Int  p (Int,q)
instance {-# LIBERAL #-} (s ~ Bool) => D Bool r (s,Bool)
```
These instances satisfy the Liberal Coverage and Liberal Instance Consistency conditions.

Now suppose we are trying to solve a Wanted constraint `[W] C alpha beta (gamma, delta)`.
* We'll get fundeps from both instances, yielding `gamma ~ Int` and `delta ~ Bool`.
* But if `alpha` later turns out to be `Int`, we'll select the first instance decl, getting `delta ~ Int`, resulting in a contradiction.
* If, on the other hand, we learned `alpha := Int` and `gamma := Int` earlier, we'd have picked the first instance immediately, and succeeded.

This reflects a loss of confluence.

### Example 4: even LICC is too strong

Consider (assuming overlapping instances):
```
class TypeEq a b (res :: Bool)  | a b -> res
instance TypeEq a a True
instance TypeEq a b False
```
These instances satisfy SCC, but not SICC.
And even the LICC would reject the program, because True and False are not unifiable!  But imagine we rewrote it like this:
```
instance r ~ True  => TypeEq a a r
instance r ~ False => TypeEq a b r
```
Now the fundep is effectively vacuous, but if it remains we'd need LCC and LICC.  But the program works fine: the overlapping-instance technology will pick an instance only when it is the unique one, and that will fix `r`.

But it's a bit un-satisfying to have to encode our desired behaviour like this.

### Example 5: Even LCC is too restrictive

We can use fundeps to support record selection in records with polymorphic fields (#18759).  Consider
```
class HasField (name :: Symbol) s a | name s -> a where
  getField :: s -> a

data T = MkT { fld :: forall a. [a] -> [a] }

instance {-# DYSFUNCTIONAL #-} HasField "fld" T ([p] -> [p])
  getField (MkT f) = f

f x = (getField @"fld" x, True)
```
Here the instance doesn't even satisfy the LCC, so I've marked it DYSFUNCTIONAL.  And yet it is very useful!
* From `f` we get `[W] HasField "fld" T alpha`.
* Using the fundep we can get `alpha ~ ([beta] -> [beta])`, which is just what we want.

In effect, the fundep gives the *shape* of `alpha` but not its complete type.  This is a pretty compelling example.

### Example 6: LIBERAL can get you DYSFUNCTIONAL

It turns out that with LIBERAL and UNDECIDABLE you can trick GHC into lifting the coverage condition algotether, effectively achieving DYSFUNCTIONAL.  Consider, this variant of Example 5:
```
instance {-# LIBERAL, UNDECIDABLE #-}
         HasField "fld" T ([p] -> [p])
         => HasField "fld" T ([p] -> [p])
  getField (MkT f) = f
```
We have added a strange context to the instance declaration, equal to itself!  Now the LCC is satisfied.  You might think that the instance is now non-terminating, because solving `HasField "fld" T ([p]->[p])` via the intance gives us a new sub-goal `HasField "fld" T ([p]->[p])`, and so on.

But GHC's type-class constraint solver has a long-standing trick whereby it solves goals co-inductively. I think it was first documented in [Scrap your boilerplate with class](https://www.microsoft.com/en-us/research/publication/scrap-your-boilerplate-with-class/), where it is *essential* to allow SYB-with-class to work at all.  You might enjoy the paper; the coinductive part is discussed in Section 5.   Coinduction is switched on all the time, but it only has an effect when you have `UndecidableInstances`, which allows instance declarations that don't provably terminate.

So in priciple, LIBERAL+UNDECIDABLE lets you express DYSFUNCTIONAL (no coverage condition at all).  But it's a weird coding trick, and so we leave DYSFUNCTIONAL in our vocabulary, for now anyway, to mean "lift coverage condition".


----------------


## 5. Exploring the unique-unifiable-instance idea

Here is a concrete idea, triggered by Examples 2, 3, and 4:

* Abandon the LICC altogether. It is too weak (Examples 2,3) and too strong (Example 4).
* Instead, when considering improvement of a Wanted constraint against the global instances, do the following:
  * Look up the constraint in the instances.
  * If at most one can possibly match (i.e. at most one instance unifies with the constraint) then, and only then, add the fundeps from that instance.
  * "Add fundeps from instance" means (precisely as now): for each fundep, if the LHS tys match, then generate an equality with the instantiated RHS tys.

This "if at most one can possibly match) is very like the [rule for overlapping instances](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html?highlight=overlapping%20instances#overlapping-instances).

#9210 is a very relevant ticket, with interesting discussion.

### Examples of how it works

Now consider Example 2:
* Only one instance unifies with `[W] C Bool [alpha] beta`
* So we take fudeps from that constraint alone, giving `beta ~ [alpha]`
* Now we can solve `[W] C Bool [alpha] [alpha]`
* Yielding `f :: a -> [a]` as desired

Now Example 3.  `[W] C alpha beta (gamma,delta)` unifies with both instances, so we get no fundeps at all.
If `alpha` gets unified with, say, `Int` by some other constraint, then it'll unify with just one instance and we can use the fundeps.

Example 4.  If we have `[W] TypeEq Int Int r`, that unifies with only one instance, so we'll get a fundep `r ~ True` as desired. Similarly if the first two arguments are apart.  All is good.

In the OP from #9210 we have
```
class Foo s t a b | a b s -> t where
instance Foo (x, a) (y, a) x y where
instance Foo (a, x) (a, y) x y where
```
We want to solve `[W] Foo (Int,Int) alpha Int String`.  This unifies with both instances, so we will not use either fundep.  We need more information to disambiguate.

### Less completeness

Because this new rule is a bit less aggressive on using fundeps, it may fail to solve some constraints that we can solve today.
```
class CX x a b | a -> b
instance CX Int Int Bool
instance CX Bool Int Bool

class C2 a b | a -> b
instance C2 Bool Bool
```
Now suppose we are solving `[W] CX alpha Int beta, [W] C2 beta alpha`.
With our new rule, both instances unify, so no fundeps are used.  We are stuck.

But in GHC today we take fundeps from both instances, giving `beta ~ Bool`.  Then we can get fundeps from `[W] C2 Bool alpha`, giving `alpha ~ Bool`.  Now we can solve the `CX` constraint.

But this is an extremely delicate setup.

### Can we do *any* instance consistency checks at all?

We want to weaken instance consistency, but can we do *any* consistency checking on instances?  For example, these ones look pretty suspicious.
```
class C2 a b | a -> b
instance C2 Int Bool
instance C2 Int Char
```
But what about this:
```
instance C2 Int [Int]
instance C2 Int (Mabye Bool)
```
Here if we have `[W] C Int [alpha]` only one instance matches and perhaps we can improve `alpah` to `Int`.

Be careful: we want to allow Example 4.

----------------

## 6. A concrete proposal

To have something concrete to discuss, here's a proposal:

* Abandon instance consistency altogether, except perhaps some super-liberal instance consistency check.
* For coverage, with no modifiers use SCC; with LIBERAL use LCC.
* When doing improvement between a constraint and an instance, do so only if only one instance can possibly match
