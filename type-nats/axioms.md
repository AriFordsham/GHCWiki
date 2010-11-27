
These are just incomplete notes!


Notation:

```wiki
m,n:    literals of kind Nat
r,s,t:  arbitrary terms of kind Nat
{expr}: 
```


Comparison:

```wiki
leqDef:   m <= n    -- if "m <= n"
leqLeast: 0 <= t
leqRefl:  t <= t
leqTrans: (r <= s, s <= t) => r <= t
```


Addition:

```wiki
addDef:      m + n ~ {m + n}
addUnit:     0 + t ~ t
addAssoc:    (r + s) + t ~ r + (s + t)
addCommutes: t + s ~ s + t
addCancel:   (r + s ~ r + t) => s ~ t
```


Multiplication:

```wiki
mulDef:      m * n ~ {m * n}
mulUnit:     1 * t ~ t
mulAssoc:    (r * s) * t ~ r * (s * t)
mulCommutes: t * s ~ s * t
mulCancel:   (r * s ~ r * t, 1 <= r) => s ~ t
```


Interactions:

```wiki
addMulDistr: r * (s + t) = (r * s) + (r * t)
```


References:

- [ http://en.wikipedia.org/wiki/Semiring](http://en.wikipedia.org/wiki/Semiring)
- [ http://en.wikipedia.org/wiki/Cancellation_property](http://en.wikipedia.org/wiki/Cancellation_property)