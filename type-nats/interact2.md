```wiki
a <= b
  (b <= a) <=> a ~ b
```

```wiki
a + b ~ c
  a + b ~ d <=> c ~ d
  b + a ~ d <=> c ~ d

  a + d ~ c <=> b ~ d
  d + a ~ c <=> b ~ d
  d + b ~ c <=> a ~ d
  b + d ~ c <=> a ~ d

m + b ~ c
  n + d ~ c <=> {m - n} + b ~ d  -- n <= m
  n + d ~ c <=> {n - m} + d ~ b  -- m >=m
```

```wiki
a * b ~ c
  a * b ~ d <=> c ~ d
  b * a ~ d <=> c ~ d

m * b ~ c
  n * b ~ c <=> (b ~ 0, c ~ 0)    -- m /= n
  n * d ~ c <=> {m / g} * b ~ {n / g} * d    -- g = gcd m n, g /= 1, replaces both assumptions

  b + c ~ d <=> {m + 1} * b ~ d
  c + b ~ d <=> {m + 1} * b ~ d
```