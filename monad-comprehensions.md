# Monad comprehensions


Monad comprehensions are currently in development. See ticket [\#4370](https://gitlab.haskell.org//ghc/ghc/issues/4370).

## Translation rules

```wiki
Variables    : x and y
Expressions  : e, f and g
Patterns     : w
Qualifiers   : p, q and r
```


The main translation rule for monad comprehensions.

```wiki
[ e | q ] = [| q |] >>= (return . (\q_v -> e))
```

`(.)_v` rules. Note that `_v` is a postfix rule application.

```wiki
(w <- e)_v = w
(let w = d)_v = w
(g)_v = ()
(p , q)_v = (p_v,q_v)
(p | v)_v = (p_v,q_v)
(q, then f)_v = q_v
(q, then f by e)_v = q_v
(q, then group by e using f)_v = q_v
(q, then group using f)_v = q_v
```

`[|.|]` rules.

```wiki
[| w <- e |] = e
[| let w = d |] = return d
[| g |] = guard g
[| p, q |] = ([| p |] >>= (return . (\p_v ->  [| q |] >>= (return . (\q_v -> (p_v,q_v)))))) >>= id
[| p | q |] = mzip [| p |] [| q |]
[| q, then f |] = f [| q |]
[| q, then f by e |] = f (\q_v -> e) [| q |]
[| q, then group by e using f |] = (f (\q_v -> e) [| q |]) >>= (return . (unzip q_v))
[| q, then group using f |] = (f [| q |]) >>= (return . (unzip q_v))
```

`unzip (.)` rules. Note that `unzip` is a desugaring rule (i.e., not a function to be included in the generated code).

```wiki
unzip () = id
unzip x  = id
unzip (w1,w2) = \e -> ((unzip w1) (e >>= (return .(\(x,y) -> x))), (unzip w2) (e >>= (return . (\(x,y) -> y))))
```

### Examples


Some translation examples (using the do notation):

```wiki
[ x+y | x <- Just 1, y <- Just 2 ]

=>

do x <- Just 1
   y <- Just 2
   return (x+y)
```


Transform statements:

```wiki
[ x | x <- [1..], then take 10 ]

=>

take 10 (do
  x <- [1..]
  return x)
```


Grouping statements (note the change of types):

```wiki
[ (x :: [Int]) | x <- [1,2,1,2], then group by x ] :: [[Int]]
```


Parallel statements:

```wiki
[ x+y | x <- [1,2,3]
      | y <- [4,5,6] ]

=>

do (x,y) <- mzip [1,2,3] [4,5,6]
   return (x+y)
```

## Implementation details