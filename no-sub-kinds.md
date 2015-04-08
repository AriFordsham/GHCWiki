
This page is to track the design of a plan, originally Simon PJ's idea, to get rid of GHC's sub-kinding story. Richard E. has volunteered to implement.

## The current story


Right now, there are several "base" kinds:

- `*`: The kind of lifted, boxed types. That is, types of kind `*` are represented with a pointer and capable of being bottom.

- `#`: The kind of unlifted types. Types of this kind cannot be bottom. Most unlifted types are unboxed, e.g. `Int#`, `Double#`; but some are boxed (represented with a heap pointer), e.g. `Array#`.

- `Constraint`: The kind of Haskell constraints. Rather annoyingly,

  - During type checking `*` and `Constraint` must be distinct kinds; so that a signature `f :: Int => Int` is rejected.
  - In Core, it is important to treat `Constraint` and `*` as indistinguishable.   **SLPJ** why?  It used to be the case because GND cast dictionaries, but we don't do that any more. **RAE** We still use this trick when converting a 1-member class into just a coercion, right? And, if `*` and `Constraint` were different, we would need multiple different arrows. In any case, this issue is completely orthogonal to this proposal.

>
> So, `tcEqType` considers `Constraint` and `*` distinct (as they are distinct in Haskell) but `eqType` considers them to be equal.

- `OpenKind`: The superkind of `*` and `#`. The existence of `OpenKind` is necessary for several reasons

  - To give a kind to `(->)`, which is `OpenKind -> OpenKind -> *`.   **SLPJ** False.  We actually give `(->)` kind `*->*->*`, but have a special kinding rule for saturated applications of `(->)`. **RAE** Right. But the points I've made below still stand, no?
  - To give a type to `error :: forall (a :: OpenKind). String -> a` and `undefined :: forall (a :: OpenKind). a`.  We need to allow `error Int# "foo" :: Int#`.
  - During inference, to give a kind to lambda-bound variables.  E.g.  `\x -> 3# +# x`.  When we encounter the lambda we give it a type of `alpha`, a unification variable. But what kind does `alpha` have?  Not `*`, else this lambda would be rejected.  So we give it `OpenKind`.

- `BOX`: This classifies kinds. Thus, we have `* :: BOX` and `# :: BOX`. Somewhat cheekily, `BOX :: BOX`.


The entire handling of `OpenKind` is unsatisfactory. For example, it is not abstractable:

```wiki
myError s = error ("Blah" ++ s)
```


Currently `myError` cannot be used at type `Int#`.

## Down with kinds


The proposed remedy below would muck about somewhat with the upper echelons of this hierarchy (i.e. `BOX`). So, instead of trying to implement this in today's GHC, Richard is implementing in his branch, which has eliminated `BOX` entirely, instead favoring `* :: *` and dropping any distinction (in Core) between types and kinds. See [ the paper](http://www.cis.upenn.edu/~eir/papers/2013/fckinds/fckinds.pdf) for more info.


All further commentary on this page assumes a merged type/kind language and semantics.

## Proposed remedy


We define an ordinary datatype `Levity`:

```wiki
data Levity = Lifted | Unlifted
```


Then, we create a new magical constant `TYPE`, of type `Levity -> TYPE Lifted`. The idea is that `TYPE Lifted` replaces the old `*` and `TYPE Unlifted` replaces the old `#`. Accordingly, if we have `* :: *`, that would mean that `TYPE Lifted` classifies *kinds* as well, thus giving the rather strange type to `TYPE`. Now, `OpenKind` can become something like `forall (l :: Levity). TYPE l`. Specifically, we would get the following facts:
 

```wiki
(->) :: forall (l1 :: Levity) (l2 :: Levity). TYPE l1 -> TYPE l2 -> TYPE Lifted
error :: forall (l :: Levity) (a :: TYPE l). String -> a
undefined :: forall (l :: Levity) (a :: TYPE l). a
```


The whole sub-kinding story goes out the window in favor of much-more-nicely-behaved kind polymorphism. To make this palatable to users, we then define

```wiki
type * = TYPE Lifted
type # = TYPE Unlifted
```


and hope that GHC's existing preserve-type-synonyms-wherever-possible machinery keeps messages reasonable.


Note that this proposal does not address the `Constraint` / `*` infelicity -- that is a separate problem.

### Levity polymorphism


With the changes outlined here, the type system would support *levity polymorphism*. That is, the type system supports something like `\x -> x :: forall (v :: Levity) (a :: TYPE v). a -> a`. But, this is hogwash -- the code generator needs to know whether to deal with pointers or not! So, in general, we wish to prohibit levity polymorphism.


But, *sometimes*, we indeed want a little levity polymorphism. For example:

```wiki
myError :: forall (v :: Levity) (a :: TYPE v). String -> a
myError s = error ("Me" ++ s)
```


The above declaration should be acceptable, but is quite clearly levity-polymorphic.


Simon and Richard believe that the following rule might work: levity polymorphism is acceptable only if the following holds:

- The user has specifically requested levity polymorphism via a type signature.

- Levity-polymorphic type variables may appear *only* to the right of arrows.


The first point is just so that GHC doesn't do unexpected things. For example, the following is quite reasonable:

```wiki
f :: forall (v :: Levity) (a :: *) (b :: TYPE v). (a -> b) -> a -> b
f g x = g x
```


but if a user wants a type inferred, we shouldn't produce such an exotic specimen.


The second point seems to be the right way to allow levity polymorphism. If the levity-polymorphic variables are mentioned only on the right of arrows, then parametricity tells us that no values can exist for these variables. Thus, the functions must diverge (or call `error` or `undefined`, the two primitive sources for levity-polymorphic type variables). This deserves More Thought before becoming implemented.


The actual implementation should be easy: add parsing support for TYPE and then check for levity-polymorphism in `checkValidType`. The inference algorithm should remain unchanged, as a levity-polymorphic type would never be inferred, just checked. This extension is left as future work.

### Issues from Dimitrios


(Apr 15: These issues are absolutely present in Richard's current implementation.)


You make it sound as if the only problem is defaulting. But I do not think it is enough. For instance, what prevents a user from writing:

```wiki
	f :: forall v. forall (a :: TYPE v). a -> a 
	f x = x
```


The only plausible explanation from that subsection is that TYPE and levities are not exposed to programmers, so they can't shoot themselves in the foot. \*However\* there are cases where you \*must\* expose levities to programmers, for instance to give a signature to an eta-expansion of "error". Example:

```wiki
  g :: forall (a:*). forall v. forall (b :: TYPE v). Show a => a -> b
  g x = error (show x) 
```


So, the statement that "the only complication for GHC is defaulting" is a pretty big understatement. How will you prevent the bad "f" above but allow the good "g"?


Now, I agree with you that defaulting is part of the solution but, what we really would like is to ensure that levities on the left of an arrow cannot be abstracted over. For instance, imagine the following hypothetical typing of (-\>):

```wiki
	(->) :: forall v1,v2, FIXED v1 => TYPE v1 -> TYPE v2 -> *
```


Where the `FIXED v1` is like a derived kind class constraint emitted during kind inference (but of course there's no associated evidence with it), which prevents a levity variable from getting generalized. Hence, during type inference:

```wiki
	FIXED Lifted  => is just discharged successfully
	FIXED Unlifted => is just discharged successfully
```


However, in the end of type inference we will be left with constraints of the form:

```wiki
	FIXED levity_unification_variable  ==> can be defaulted to Lifted
                                               (this will allow us to infer sound types)

	FIXED levity_skolem_variable       ==> are genuine errors! 
```


I don't think it would be that hard to generate kind constraints like the `FIXED` kind class, and treat them specially during defaulting, would it? In fact this might be something we want to introduce to GHC \*today\* even without sucking in the fully glory of your kind equalities, is my guess. 
