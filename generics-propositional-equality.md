
This page sketches the ideas how to equip `GHC.Generics` with type-level reasoning facilities.


Motivation


the gdiff library requires a family GADT for describing the constructors of datatypes that one wants to `diff` and `patch`. This family has to cover (identify) all appearing data type constructors in _value tree_ transitively. Given two such constructor identifiers gdiff uses propositional equality (called `decEq` in the library) to get hold of the constructor's data in a type-safe manner.


GHC.Generics provides a `Rep t p` (representation of the data structure at the type level) for every data type when the user demands `deriving Generic`. Metadata is attached to parts of this representation which can be queried for names, modules, fixity, etc. at runtime.


Marriage of GHC.Generics with `gdiff` appears straightforward weren't there one detail: the metadata is not available at the type level, so `gdiff`'s requirement for propositional equality cannot be satisfied.


To support this propositional equality in GHC.Generics we have to equip the {datatype, constructor and selector} metatypes with type-level information so that we can use Data.TypeEquality-provided functions (e.g. sameNat, sameSymbol) on them.


Implementation Idea
