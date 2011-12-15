# Kind polymorphism and datatype promotion


This page gives additional implementation details for the `-XPolyKinds` flag. The grand design is described in the paper [ Giving Haskell a Promotion](http://dreixel.net/research/pdf/ghp.pdf). Most of the work has been done and merged into GHC 7.4.1. The relevant user documentation is in \[the user's guide (add link when it's up)\] and on the [ Haskell wiki page](http://haskell.org/haskellwiki/GHC/Kinds). What still doesn't work, or doesn't work correctly, is described here.

# Explicit kind variables

# Kind defaulting in type families

# [ \#5682](http://hackage.haskell.org/trac/ghc/ticket/5682) (proper handling of infix promoted constructors)

# Kind synonyms (from type synonym promotion)


At the moment we are not promoting type synonyms, i.e. the following is invalid:

```wiki
data Nat = Ze | Su Nat
type Nat2 = Nat

type family Add (m :: Nat2) (n :: Nat2) :: Nat2
```

**Future work:** promote type synonyms to kind synonyms.

# Generalized Algebraic Data Kinds (GADKs)