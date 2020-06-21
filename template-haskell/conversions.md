## Template Haskell Conversions


There are various conversions between datatypes involved in the Template Haskell implementation.  The following diagram summarises the operations and how they compose, and proposes a refactoring.
 

```wiki
        GHC.Tc.Gen.Splice.         GHC.ThToHs.
          reify                      convertToHsDecls
TyThing ----------------> TH.Dec ---------------------------> HsSyn RdrName
   |                    ^   |
   |                   /    |
   |  A               /     |
   |                 /      | 
   v                / B     |
HsSyn Name --------'        |
   |                        | C
   | GHC.HsToCore.Quote.    |
   |   dsBracket            |
   |                        |
   V                        |
 Core <---------------------'
```


We currently have `GHC.Tc.Gen.Splice.reify` for reifying a `TyThing` into TH syntax, and `GHC.ThToHs` for converting TH syntax back into `HsSyn`.  Also, we have `GHC.HsToCore.Quote` for converting the contents of TH brackets into the `Core` code which generates the TH syntax for those brackets.


We do not have (A), (B), or (C).


We need (A), for Haddock.


Hence, we could:
 

- Refactor `GHC.Tc.Gen.Splice.reify` into pieces A and B.
- implement C, and replace `GHC.HsToCore.Quote.dsBracket` with the composition `C.B`.


Code for A is in #3355


