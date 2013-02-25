### Add MetaML-style quotes


The [ Template Haskell Proposal](http://hackage.haskell.org/trac/ghc/blog/Template%20Haskell%20Proposal) contains a sub-proposal to
[ Add MetaML-style quotes](http://hackage.haskell.org/trac/ghc/blog/Template%20Haskell%20Proposal#PartB:AddMetaML-styletypedquotes).  This requires a more detailed design.


In the case of terms (only), we know from MetaML that we can have typed quotations. These are rather useful as the metaocaml exploration of the [ Shonan Challenge report](http://okmij.org/ftp/meta-programming/Shonan-challenge.pdf) and [ code](https://github.com/StagedHPC/shonan-challenge) (amongst others) shows.  Right now, however, all this work is done in (the recently reborn) [ metaocaml](http://okmij.org/ftp/ML/MetaOCaml.html) rather than in (Template) Haskell because of the availability of typed quotes and splices for increased correctness.

1. **Add a new abstract type of typed expressions** TExp a 

1. **Add a new term quotation form**`[|| e ||]`, called a typed quote; the type of the quote is `TExp ty`, where `ty` is the type of `e`. In the type-system jargon, this is the "introduction form" for `TExp`. 

1. **Add a new splice form**`$$e`, called a typed splice. The term `e` must have type `TExp ty`, and the splice `$$e` then has type ty. This is the "elimination form" for `TExp`. 

1. **Add a constant which takes a typed quote and returns an untyped one**: `unType :: TExp a -> Q Exp`

1. **Run these new typed splices in the typechecker, not the renamer.**

1. **Use renaming of binding forms to insure capture-avoidance** (a la MetaML).

1. **Cross-stage persistence will remain unchanged.**  To be able to use an identifier at future stages, it must be fully available, which means that it needs to be defined in a previous compilation unit if it will be spliced (by name) into a term.


The justification for 2 and 3 are classical.  For ensuring type-safety (at the current state of knowledge), it is important that `TExp` be abstract, as sound typed-expression manipulation is very hard to achieve, especially in the presence of binders.  A future extension may open this up, whenever this particular tough nut is cracked, but for now we must have 1.  In theory, `unType` (4) is not needed; in practice, it probably will be.  5 is obvious: type information needs to be available for typed splices, and this is not available in the renamer.  6 comes from MetaML, and basically just means that the renamer will be applied to typed splices as well.  7 documents a non-change \[which is a little awkward in code, was not present in the old metaocaml, but is actually in the new metaocaml).
