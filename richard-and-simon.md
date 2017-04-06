# Summary of tasks to be completed


... as discussed by Richard and Simon. This page is mostly for our own notes, but others are welcome to read it.

- Sort out `matchTypeable` (see email) [\#13333](https://gitlab.haskell.org//ghc/ghc/issues/13333)
- Sort out `mkCastTy` (see email)
- Implement homogeneous as per Stephanie's paper
- Fix [\#11715](https://gitlab.haskell.org//ghc/ghc/issues/11715) according to Richard's plan
- Change flattener to be homogeneous ([\#12919](https://gitlab.haskell.org//ghc/ghc/issues/12919))
- Remove `solveSomeEqualities`
- Generalized injectivity [\#10832](https://gitlab.haskell.org//ghc/ghc/issues/10832), vis-a-vis Constrained Type Families paper
- [\#13333](https://gitlab.haskell.org//ghc/ghc/issues/13333) (Typeable regression)
- Taking better advantage of levity polymorphism:

  - Could `[]` be a data family?
  - Unlifted newtypes
  - Unlifted datatypes
  - generalized classes in base
  - ...
- [\#11739](https://gitlab.haskell.org//ghc/ghc/issues/11739) (simplify axioms)
- Fix all the `TypeInType` bugs
- Clean up pure unifier to make the fact that kind coercions *only* affect type variables by using, e.g., `getCastedTyVar_maybe`.
- Is it possible to remove all the pushing in `mkCastTy`? I think so. The pushing doesn't help `splitTyConApp` at all, because pushing a coercion into a `TyConApp` doesn't make it become another `TyConApp`. The pushing *does* help `splitAppTy`, but perhaps all usages of `splitAppTy` already account for casts. *Dangling problem:* what on earth to do about `LRCo`, given that it's not always possible to push? It seems that it may be impossible to make `LRCo` work in a way that respects `eqType` equality, with `eqType`'s ability to move casts about willy-nilly.

**Iceland_jack**: By `[]` as a data family do you mean:

```
datafamily[](a ::TYPE(rep ::RuntimeRep))::Typedatainstance[](a ::Type)=[]| a :[a]datainstance[](a ::TYPEIntRep)=INil|ICons a [a]...
```


I invite you to look at [ this gist](https://gist.github.com/Icelandjack/1824f4544c86b4ab497282783f94c360) posted on [\#12369](https://gitlab.haskell.org//ghc/ghc/issues/12369) and [\#13341](https://gitlab.haskell.org//ghc/ghc/issues/13341).
