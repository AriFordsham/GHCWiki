
Notation:

```wiki
m,n,k:  natural numbers
a,b,c:  types of kind Nat
```


When overlapping, earlier rules take precedence (as in Haskell multi-equation definitions).


Top-level interactions for `TypeNat`:

```wiki
TypeNat m
```


Top-level interactions for \<=

```wiki
m <= n   <=> {m <= n}
0 <= a   <=> True
a <= 0   <=> a ~ 0
```


Top-level interactions for +.

```wiki
(m + n ~ k) <=> {m + n == k}
(m + a ~ n) <=> a ~ {n - m}    -- n >= m
(a + m ~ n) <=> a ~ {n - m}    -- n >= m
(0 + a ~ b) <=> a ~ b
(a + 0 ~ b) <=> a ~ b
(a + b ~ 0) <=> (a ~ 0, b ~ 0)
(a + b ~ a) <=> (b ~ 0)
(a + b ~ b) <=> (a ~ 0)
(a + a ~ b) <=> (2 * a ~ b)
(a + m ~ b) <=> (m + a ~ b)    -- simple normalization cuts down on some rules
```


Top-level interactions for \*.

```wiki
(m * n ~ k) <=> {m * n == k}
(m * a ~ n) <=> a ~ {n / m}     -- m `divides` n
(a * m ~ n) <=> a ~ {n / m}     -- m `divides` n
(0 * a ~ b) <=> b ~ 0
(a * 0 ~ b) <=> b ~ 0
(1 * a ~ b) <=> a ~ b
(a * 1 ~ b) <=> a ~ b
(a * b ~ 1) <=> (a ~ 1, b ~ 1)
(a * a ~ b) <=> a ^ 2 ~ b
(m * a ~ a) <=> a ~ 0            -- 2 <= m
```


Top-level interactions for `^`

```wiki
(m ^ n ~ k) <=> {m ^ n} == k
(m ^ a ~ n) <=> a ~ {log m n}   -- log (base m) of n exists
(a ^ m ~ n) <=> a ~ {root m n}  -- m-th root of n exists
(a ^ 0 ~ b) <=> b ~ 1
(a ^ 1 ~ b) <=> a ~ b
(a ^ m ~ a) <=> (a <= 1)        -- 2 <= m
(1 ^ a ~ b) <=> b ~ 1
(m ^ a ~ a) <=> False           -- m /= 1
```