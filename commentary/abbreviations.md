
Certain abbreviations are used pervasively throughout the GHC source code.  A few, like "Tc", are easy to figure out.  Others, like "Occ", are more challenging (in case you were wondering, none of GHC's code is [ Occult](http://en.wikipedia.org/wiki/Occult), except perhaps in the Latin sense)


See also: [ A similar list on the GHC Users' wiki](http://www.haskell.org/haskellwiki/GHC/List_of_abbreviations).

- "Occ" means "Occurrence"

  - However, in the context of [ OccName](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Compiler/RdrNameType#TheOccNametype), "occurrence" actually means "unqualified"

- "Rn" means "Renamer"

- "Rdr" means "Parser" (do not ask me why, I don't know)

- "Tc" means "TypeCheck{ing,er}"

- "Lcl" means "Local"

- "Gbl" means "Global"

- "Loc" means "Location", as in SrcLoc

- "Hs" means "Haskell Syntax" (generally as opposed to Core -- for example, Expr vs HsExpr)
