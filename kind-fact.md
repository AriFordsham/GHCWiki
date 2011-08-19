# Adding Kind Fact


Proposal: extend the kind system with a kind **Fact** to cover constraints as well as types, in order to reuse existing abstraction mechanisms, notably **type synonyms**, in the constraint language.


Much of the motivation for this proposal can be found in [ Haskell Type Constraints Unleashed](http://www.cs.kuleuven.be/%7Etoms/Research/papers/constraint_families.pdf) which identifies the shortage of abstraction mechanisms for constraints relative to types. See ticket [\#788](https://gitlab.haskell.org//ghc/ghc/issues/788) for the resulting **constraint synonym** proposal, which seeks to fill some of the gaps with new declaration forms. Here, however, the plan is to extend the kind system, empowering the existing mechanisms to work with constraints. [ Max Bolingbroke](http://blog.omega-prime.co.uk/?p=61), commenting on [ context aliases](http://www.haskell.org/haskellwiki/Context_alias) (in turn based on John Meacham's [ class alias](http://repetae.net/recent/out/classalias.html) proposal) makes a similar suggestion, remarking that a new kind would probably help. The claim here is that the new kind obviates the need for other new syntax.

## The proposal

- Add a kind `Fact` for constraints, so that, e.g. `Monad :: (* -> *) -> Fact`.

- Close `Fact` under tuples, so `(F1, .. Fn) :: Fact` iff each `Fi :: Fact`.

- Allow (rather, neglect to forbid) the use of `type` to introduce synonyms for Fact(-constructing) things.  Thus one might say

  ```wiki
  type Transposable f = (Traversable f, Applicative f)
  type Reduce m x = (Monad m, Monoid (m x))
  type Stringy x = (Read x, Show x)
  ```

- Allow these synonym facts to appear wherever a class constraint can appear.  For example

  ```wiki
  class Stringy a => C a where ....
  f :: Reduce m x => x -> m x
  ```

- Allow nested tuple constraints, with componentwise unpacking and inference, so that `(Stringy x, Eq x)` is a valid constraint without flattening it to `(Read x, Show x, Eq x)`.

- Retain the policy of defaulting to kind `*` in ambiguous inference problems -- notably `()` is the unit type and the trivial constraint -- except where overridden by kind signatures.  For example:

  ```wiki
  type MyUnit = () -- gives the unit type by default
  type MyTrue = () :: Fact  -- needs the kind signature to override the default
  ```

- Allow the **type family** mechanism to extend to the new kinds, pretty much straight out of the box. For example:

  ```wiki
  type family   HasDerivatives n     f :: Fact
  type instance HasDerivatives Z     f  = ()
  type instance HasDerivatives (S n) f  = (Differentiable f, HasDerivatives n (D f))
  ```

  where `Differentiable` is the class of differentiable functors and `D f` is the associated derivative functor.

## More syntax


One might consider a syntax for giving fully explicit kinds to type synonyms, like this:

```wiki
type Reduce :: (* -> *) -> * -> Fact where
  Reduce m x = (Monad m, Monoid (m x))
```

## Discussion

`Fact` synonyms appear to overlap with superclases.  For example one could say

```wiki
class (Read x, Show x) => Stringy x where {}
```


But there are significant differences

- Using the class mechanism forces you to give an `instance` declaration too.  Perhaps something like

  ```wiki
  instance (Read x, Show x) => Stringy x where {}
  ```

  This is painful duplication, and (worse) there is little to stop you writing an overlapping instance later.  At least, looking at the instance doesn't tell you that no overlapping instance is intended.

- The type family mechamism gives new power.  For example, consider the celebrated collection example:

  ```wiki
  class Collection c where
    type family X c :: * -> Fact
    empty :: c a
    insert :: (X c) => c a -> a -> c a

  instance Collection [] where
    type instance X [] a = Eq a
    insert xs x = ...

  instance Collection Data.Set where
    type instance X Data.Set a = Ord a
    insert s x = ...
  ```

  See [ Bulk types with class](http://research.microsoft.com/en-us/um/people/simonpj/papers/collections.ps.gz).

## Bikeshed discussion of nomenclature

`Fact` is the working name for the new kind. `Constraint` is an obvious contender, but long. `Prop` should *not* be used, as any analogy to Prop in the Calculus of Constructions would be bogus: proofs of a Prop are computationally irrelevant and discarded by program extraction, but witnesses to a Fact are material and computationally crucial. Another thought: 'Constraint' sounds more like a requirement than a guarantee, whereas 'Fact' is neutral. On the other hand, the kinding `F :: Fact` might be misleadingly suggestive of `F`'s truth, over and above its well-formedness.


\[illissius\] I checked thesaurus.com for (even remotely) plausible names, and this is what I found:

- Fact, Knowledge, Information, Data, Evidence, Proof
- Requirement, Constraint, Guarantee, Promise, Contract
- Characteristic, Attribute, Property, Trait, Capability
- Axiom, Lemma, Theorem, Proposition, Postulate, Premise, Claim, Posit
