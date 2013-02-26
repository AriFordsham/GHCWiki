
This page collects ideas about definitions to support type-level (propositional) reasoning in Haskell programs. Much of the initial content comes from the thread "RFC: Singleton equality witnesses" on ghc-devs. Currently (Feb 2013), these ideas are implemented in `GHC.TypeLits`.


See also [TypeNats](type-nats).


Gabor Greif's proposal:

```wiki
class (kparam ~ KindParam, SingE (kparam :: OfKind k)) => SingEquality (kparam :: OfKind k) where
  type SameSing kparam :: k -> k -> *
  type SameSing kparam = (:~:)
  sameSing :: Sing a -> Sing b -> Maybe (SameSing kparam a b)

instance SingEquality (KindParam :: OfKind Nat) where
  sameSing = eqSingNat

instance SingEquality (KindParam :: OfKind Symbol) where
  sameSing = eqSingSym
```


Richard Eisenberg's proposal:

```wiki
-- Void and absurd are borrowed from Edward Kmett's 'void' package:
data Void
absurd :: Void -> a
absurd x = case x of {}

type PropNot a = a -> Void

type Decision a = Either a (PropNot a)

class SingE kparam => SingEquality (kparam :: OfKind k) where
  sameSing :: forall (a :: k) (b :: k). Sing a -> Sing b -> Decision (a :~: b)
```


I (Richard) think that using `Decision` instead of `Maybe` allows tighter programs to be written, because programmers can escape from impossible situations using `absurd`. If `sameSing` returns only a `Maybe`, then a programmer gets no information usable at the type level when the two singletons do not equal.


Other variations:

- In Edward Kmett's *void* package, he uses a different definition of `Void`:

  ```wiki
  newtype Void = Void Void
  ```


I'm not sure of the advantages/disadvantages of the choice of representation here. My guess is that Edward did this so his absurd function could spin infinitely. With empty case, we don't need that.

- I'm not at all happy with the name `PropNot`, but I wanted to reserve `Not` for Boolean equality, as a parallel to `not`. Please suggest something better. Gabor suggests `Falsified`, or even better `Refuted`.

## Other thoughts (Richard)

- The *singletons* package contains these definitions:

```wiki
data SingInstance (a :: k) where
  SingInstance :: SingRep a => SingInstance a
class (kparam ~ KindParam) => SingKind (kparam :: OfKind k) where
  singInstance :: forall (a :: k). Sing a -> SingInstance a
```

`SingKind (KindParam :: OfKind k)` states that the kind `k` has an associated singleton. I think that's a better superclass for `SingEquality` than just `SingE`.

- I don't love having the functions `unsafeSingNat` and `unsafeSingSymbol` in `GHC.TypeLits`. I envision a future where, some day, a programmer could statically declare that they avoid the partial features of Haskell (along the lines of Safe Haskell, but stricter). Having these functions here means that this module would not be safe. (I am not bothered by various uses of `unsafeCoerce` within the module -- those uses certainly seem safe to me.) Instead, I propose that we move them to `GHC.TypeLits.Unsafe`.

- Perhaps we should move some of what we're discussing out of `GHC.TypeLits`. After all, `(:~:)` does not interact directly with singletons, and neither do some of the definitions I mentioned above. I'm at a bit of a loss for a name, though...

## Other thoughts (Gabor)

- Ultimately we want GHC to derive the `SingEquality` instance (`sameSing`, `decideSing` methods) for any singleton instance. GHC libraries should be laid out in a way that GHC's deriving engine can access all vital parts. IISC, this criterion rules out a completely detached library.

## Course of Implementation


Gabor: I have created a new branch `type-reasoning` and pushed everything I have so far to the `libraries/base` repo. [ Richard's mail](http://www.haskell.org/pipermail/ghc-devs/2013-February/000304.html) summarizes what still needs to be done.


Richard (11 Feb 2013): I've just pushed a commit to the type-reasoning branch with a strawman proposal of a reorganization of these definitions. Specifically, this commit breaks TypeLits into the following five files:

- GHC.TypeEq, which contains the definitions for (:\~:), Void, Refuted, etc.
- GHC.Singletons, which contains the definitions about singletons in general, such as SingI and SingEquality
- GHC.TypeLits.Unsafe, which contains just unsafeSingNat and unsafeSingSymbol
- GHC.TypeLits.Internals, which is necessary to get GHC.TypeLits.Unsafe to have access to the right internals; this module is not exported from the 'base' package
- GHC.TypeLits, which contains the definitions specific to type-level literals.


Some thoughts on this design:

- First off, why is TypeEq part of GHC?? Because we wish to write eqSingNat and eqSingSym in GHC.TypeLits, and that module rightly deserves to be part of GHC. I'm quite uncomfortable with this decision, and I even created a new git repo at \[github.com/goldfirere/type-reasoning\] to hold the definitions that eventually ended up in GHC.TypeEq. (The repo has nothing in it, now.) Perhaps the best resolution is to move eqSingNat and eqSingSym out of GHC.TypeLits and into an external package, but that seems silly in a different direction. (It is fully technically feasible, as those functions don't depend on any internals.) I would love some feedback here.
- Why is Singletons broken off? No strong reason here, but it seemed that the singletons-oriented definitions weren't solely related to type-level literals, so it seemed more natural this way.
- Making the Unsafe module was a little more principled, because those functions really are unsafe! They are quite useful, though, and should be available somewhere.
- Currently, the internals of GHC assign types like "0" the kind GHC.TypeLits.Nat, so Nat and Symbol **must** remain in the GHC.TypeLits module. Unfortunately, the plumbing around GHC.TypeLits.Unsafe want Nat and Symbol to be defined in GHC.TypeLits.Internals. So, I created a TypeLits.hs-boot file to fix the problem. This is highly unsatisfactory, and if something like what I've done here sticks around, we should change the internals of GHC to use GHC.TypeLits.Internals.Nat, getting rid of the import cycle.
- I've put in the decideSing function as discussed further up in this thread. Its implementation for Nat and Symbol must use unsafeCoerce, but that shouldn't be a surprise.


Unfortunately, the code doesn't compile now. This is because it needs SingI instances for, say, Sing 0. For a reason I have not explored, these instances are not available here, though they seem to be for code written outside of GHC. Iavor, any thoughts on this?


Please tear any of these ideas (or my whole commit) to shreds! It really is meant to be a strawman proposal, but committing these changes seemed the best way of communicating on possible set of design decisions.
