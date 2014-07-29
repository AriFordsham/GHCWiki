
This page is to track the design of a plan, originally Simon PJ's idea, to get rid of GHC's sub-kinding story. Richard E. has volunteered to implement.

## The current story


Right now, there are several "base" kinds:

- `*`: The kind of lifted, boxed types. That is, types of kind `*` are represented with a pointer and capable of being bottom.

- `#`: The kind of unlifted types. Types of this kind cannot be bottom. Most unlifted types are unboxed, e.g. `Int#`, `Double#`; but some are boxed (represented with a heap pointer), e.g. `Array#`.

- `Constraint`: The kind of Haskell constraints. Rather annoyingly,

  - During type checking `*` and `Constraint` must be distinct kinds; so that a signature `f :: Int => Int` is rejected.
  - In Core, it is important to treat `Constraint` and `*` as indistinguishable.   **SLPJ** why?  It used to be the case because GND cast dictionaries, but we don't do that any more.

>
> So, `tcEqType` considers `Constraint` and `*` distinct (as they are distinct in Haskell) but `eqType` considers them to be equal.

- `OpenKind`: The superkind of `*` and `#`. The existence of `OpenKind` is necessary for several reasons

  - To give a kind to `(->)`, which is `OpenKind -> OpenKind -> *`.   **SLPJ** False.  We actually give `(->)` kind `*->*->*`, but have a special kinding rule for saturated applications of `(->)`.
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


Note that `myError` would get a natural inferred type

```wiki
myError :: forall (l :: Levity) (a :: TYPE l). String -> a
```


Whether users could write down such a signature is another matter.  (They probably should be able to.)


Note that this proposal does not address the `Constraint` / `*` infelicity -- that is a separate problem.
