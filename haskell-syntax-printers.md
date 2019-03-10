## Printers for Haskell Syntax


Here, we discuss the details and design of existing, and desired, printers for Haskell syntax.
One key motivation is to gather enough knowledge around the topic, so we can restructure `Outputable` machinery in GHC (specially with [ Growable AST](ImplementingTreesThatGrow)), and reuse it as a well-tested and well-maintained alternative to the existing printing packages.    

### Classical Pretty Printers


todo!
(e.g., Hughes and Simon's, Wadler's, Leijen's, and also Christiansen's) 

### Exact Printers


todo! (e.g.,[ HSE](https://hackage.haskell.org/package/haskell-src-exts))

### Debug Printers (Outputters)


todo! (e.g., `Outputable` instances in GHC, with the desired modification that they also print annotations)

### Data Printers (Show)


todo! (e.g., `Show` type class in Prelude)
