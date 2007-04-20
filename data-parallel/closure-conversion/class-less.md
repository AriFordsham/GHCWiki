## Closure conversion without a conversion class


The following scheme - if Roman doesn't find any problems with it (he is notorious for that) - should be simpler than what we had in mind so far for mixing converted and unconverted code.

### Type declarations


If a type declaration for constructor `T` occurs in a converted module, we
(1) generate a converted type declaration `T_CC` together two conversion functions `fr_T` and `to_T`, and
(2) store these three names in the representation of `T`.
Concerning Point (2), more precisely the alternatives of `TyCon.TyCon` get a new field `tyConCC :: Maybe (TyCon, Id, Id)`.  This field is `Nothing` for data constructors for which we have no conversion and `Just (T_CC, fr_T, to_T)` if we have a conversion.


Incidentally, if during conversion we come across a type declaration that we don't know how to convert (as it uses fancy extensions), we just don't generate a conversion.

### Class declarations

### Instance declarations