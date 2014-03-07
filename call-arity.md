# Call Arity notes


This wiki page is about the Call Arity analysis and transformation, which eta-expands definitions if we know that it is always being called with a certain number of arguments. This looks a the *uses* of a function, which is different from the the code in  CoreArity, which looks at the *definition* of a function.


This pages does **not** document Call Arity as implemented; that documentation should be found and maintained with the code, at [source:compiler/simplCore/CallArity.hs](/trac/ghc/browser/compiler/simplCore/CallArity.hs)[](/trac/ghc/export/HEAD/ghc/compiler/simplCore/CallArity.hs).


This page used to discuss possible changes to the analysis, but these are implemented now (\[cb8a63c\]), so I removed the obsolete notes from here.
