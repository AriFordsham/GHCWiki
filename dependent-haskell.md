# Adding dependent types to Haskell


This page is to track design and implementation ideas around adding a form of dependent types to Haskell. This work will also fix bug [\#7961](https://gitlab.haskell.org//ghc/ghc/issues/7961).

## Related work


There are several published works very relevant to the design:

- [ System FC with Explicit Kind Equality](http://www.cis.upenn.edu/~eir/papers/2013/fckinds/fckinds.pdf). Stephanie Weirich, Justin Hsu, and Richard A. Eisenberg. ICFP 2013.
- [ Type Inference, Haskell, and Dependent Types](https://personal.cis.strath.ac.uk/adam.gundry/thesis/thesis-2013-07-24.pdf). Adam Gundry. PhD Thesis, 2013.


There are also many works addressing the use of dependent types in Haskell. Here is a selection:

- [ Dependently typed programming with singletons](http://www.cis.upenn.edu/~eir/papers/2012/singletons/paper.pdf). Richard A. Eisenberg and Stephanie Weirich. Haskell Symposium 2012.
- [ Hasochism: The Pleasure and Pain of Dependently Typed Haskell](https://personal.cis.strath.ac.uk/conor.mcbride/pub/hasochism.pdf). Sam Lindley and Conor McBride. Haskell Symposium 2013.
