## Closure conversion without a conversion class


The following scheme - if Roman doesn't find any problems with it (he is notorious for that) - should be simpler than what we had in mind so far for mixing converted and unconverted code.

### Type declarations


If a type declaration for constructor `T` occurs in a converted module, we
(1) generate a converted type declaration `T_CC` together two conversion functions `fr_T` and `to_T`, and
(2) store these three names in the representation of `T`.
Concerning Point (2), more precisely the alternatives of `TyCon.TyCon` get a new field `tyConCC :: Maybe (TyCon, Id, Id)`.  This field is `Nothing` for data constructors for which we have no conversion and `Just (T_CC, fr_T, to_T)` if we have a conversion.


Incidentally, if during conversion we come across a type declaration that we don't know how to convert (as it uses fancy extensions), we just don't generate a conversion.

### Class declarations


If we come across a class declaration for a class `C` during conversion, we convert it generating `C_CC`.  Like with type constructors, `Class.Class` gets a `classCC :: Maybe Class` field that is `Just C_CC` for classes that have a conversion.  We also ensure that the `classTyCon` of `C`, let's call it `T_C`, refers to `T_C_CC` and `fr_T_C` and `to_T_C` in its `tyConCC` field, and that the `classTyCon` of `C_CC` is `T_C_CC`.

### Instance declarations


If we encounter an instance declaration for `C tau` during conversion, there are two alternatives: we have a conversion for `C` or not:

- if we do not have a conversion, we generate an instance (and hence dfun) for `C tau^`, where `tau^` is the closure converted `tau`;
- if we have a conversion, we generate an instance for `C_CC tau^`.


In any case, we add a field `is_CC :: Just Instance` to `InstEnv.Instance` that contains the additionally generated instance.  And in both cases, we should be able to derive the required code for the dfun from the definition of `C tau`.  We also make sure that the `dfun`'s `idCC` field (see below) is set to that of the converted dfun.

### Type terms


We determine the converted type `t^` of `t` as follows:

```wiki
T^            = T_CC , if available
                T    , otherwise
a^            = a
(t1 t2)^      = t1^ t2^
(t1 -> t2)^   = Clo t1 t2
(forall a.t)^ = forall a.t^
(C t1 => t2)^ = C_CC t1^ => t2^ , if available
                C t1^ => t2^    , otherwise
```

### Value bindings


When converting a toplevel binding for `f :: t`, we generate `f_CC :: t^`.  The alternatives `GlobalId` and `LocalId` of `Var.Var` get a new field `idCC :: Maybe Id` and the `Id` for `f` contains `Just f_CC` in that field.

### Core terms


Apart from the standard rules, we need to handle the following special cases:

- We come across a value variable `v` where `idCC v == Nothing` whose type is `t`: we generate `convert t v` (see below).
- We come across a case expression where the scrutinised type `T` has `tyConCC T == Nothing`: we leave the case expression as is (i.e., unconverted), but make sure that the `idCC` field of all variables bound by patterns in the alternatives have their `idCC` field as `Nothing`.  (This implies that the previous case will kick in and convert the (unconverted) values obtained after decomposition.)
- We come across a dfun: If its `idCC` field is `Nothing`, we keep the selection as is, but apply `convert t e` from it it, where `t` is the type of the selected method and `e` the selection expression.  If `idCC` is `Just d_CC`, and the dfun's class is converted, `d_CC` is fully converted.  If it's class is not converted, we also keep the selection unconverted, but have a bit less to do in `convert t e`.  **TODO** This needs to be fully worked out.

### Generating conversions