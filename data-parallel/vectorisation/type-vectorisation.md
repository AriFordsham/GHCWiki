## Type Vectorisation


The transformation of types includes both closure conversion and the pairing of scalar with lifted computations.

### Transformation rules

```wiki
T*            = T_V , if T_V exists
              = T    , otherwise
a*            = a_v
(t1 -> t2)*   = (  t1*  -> t2*,   , if kindOf t1 == #
                 [:t1*  -> t2*:])   or kindOf t2 == #
              = (  t1* :-> t2*,   , otherwise
                 [:t1* :-> t2*:])
-----weiter
(t1 t2)^      = t1^ t2^
(forall a.t)^ = forall a_CC.t^
```